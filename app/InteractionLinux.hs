{-# LANGUAGE TemplateHaskell #-}

module InteractionLinux
  ( InteractionResult,
    getIrSingleValue,
    getIrValues,
    questionUser,
  )
where

import Brick
import Brick.Main (defaultMain)
import Brick.Widgets.Border
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable (Foldable (toList))
import qualified Data.Map as M
import Data.Sequence (Seq, empty, fromList, mapWithIndex, (|>))
import qualified Data.Sequence as Seq (index)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro
import Lens.Micro.TH -- for makeLenses
-- for makeLenses
import SemanticAnalyzer (SymbolInformation (EnumerationSymbol), SymbolTable, EnumerationValue (EnumerationValue, enumValue), RequiresRule (SetsValueArea, RequiresValue))
import Text.ParserCombinators.Parsec.Char (tab)
import Text.ParserCombinators.Parsec.Token (GenTokenParser(symbol))

-- Data Types

data InteractionResult
  = ValueResult ValueTable
  | AmountResult Int
  deriving (Show)

-- State data type

type ValueTable = M.Map String [String]

data Mode
  = ToBeDetermined
  | Manual
  | Random

data Step
  = ChooseMode
  | ChooseParameter
  | ChooseValueForParameter
  deriving (Eq)

data UI = UI
  { _values :: Seq String,
    _index :: Int
  }

makeLenses ''UI

data State = State
  { _mode :: Mode,
    _step :: Step,
    _ui :: UI,
    _valueTable :: ValueTable,
    _symbolTable :: SymbolTable,
    _currentParameter :: String
  }

makeLenses ''State

initialState :: SymbolTable -> State
initialState table =
  State
    { _mode = ToBeDetermined,
      _step = ChooseMode,
      _ui =
        UI
          { _values = empty |> "Manual" |> "Random",
            _index = 0
          },
      _valueTable = M.empty,
      _symbolTable = table,
      _currentParameter = ""
    }

getValueAmount :: State -> Int
getValueAmount state = length $ state ^. ui . values

currentValue :: State -> String
currentValue state = (state ^. ui . values) `Seq.index` (state ^. ui . index)

-- App definition

app :: App State e ()
app =
  App
    { appDraw = draw,
      appHandleEvent = handler,
      appStartEvent = return,
      appAttrMap = myMap,
      appChooseCursor = showFirstCursor
    }

-- Drawing

draw :: State -> [Widget n]
draw state =
  [ vBox
      (str "Choose mode:" : (str . show) (state ^. ui . index) : drawValues state)
  ]

drawValues :: State -> [Widget n]
drawValues state = toList $ mapWithIndex (value2Widget (state ^. ui . index)) (state ^. ui . values)

value2Widget :: Int -> Int -> String -> Widget n
value2Widget currentIndex valueIndex value
  | currentIndex /= valueIndex = str value
  | currentIndex == valueIndex = withAttr current $ str value
  | otherwise = emptyWidget

-- Attributes

current = attrName "current"

globalDefault :: Attr
globalDefault = defAttr

myMap :: s -> AttrMap
myMap s =
  attrMap
    globalDefault
    [ (current, black `on` white)
    ]

-- Event Handling

handler :: State -> BrickEvent n e -> EventM n (Next State)
handler state (VtyEvent (EvKey KEsc [])) = halt state
handler state (VtyEvent (EvKey KDown [])) = continue (nextElement state)
handler state (VtyEvent (EvKey KUp [])) = continue (previousElement state)
handler state (VtyEvent (EvKey KEnter [])) = nextStep state
handler state event = continue state

nextElement :: State -> State
nextElement state
  | state ^. ui . index < getValueAmount state - 1 = over (ui . index) (+ 1) state
  | state ^. ui . index >= getValueAmount state - 1 = set (ui . index) 0 state
  | otherwise = state

previousElement :: State -> State
previousElement state
  | state ^. ui . index <= 0 = set (ui . index) (getValueAmount state - 1) state
  | state ^. ui . index > 0 = over (ui . index) (subtract 1) state
  | otherwise = state

nextStep :: State -> EventM n (Next State)
nextStep state
  | state ^. step == ChooseMode = nextStepFromChooseMode state
  | state ^. step == ChooseParameter = chooseValueForParameterStep state
  | state ^. step == ChooseValueForParameter = (chooseParameterStep . addValueFromSelection) state
  | otherwise = halt state

nextStepFromChooseMode :: State -> EventM n (Next State)
nextStepFromChooseMode state
  | state ^. ui . index == 0 = chooseParameterStep state
  | state ^. ui . index == 1 = halt (set mode Random state)
  | otherwise = error "Index out of range"

chooseParameterStep :: State -> EventM n (Next State)
chooseParameterStep state =
  continue $
      state
        & set step ChooseParameter
        & set (ui . values) (fromList (M.keys (enumerationSymbolsOnly (state ^. symbolTable)))) -- Notation: https://mail.haskell.org/pipermail/haskell-cafe/2013-November/111288.html


-- | Takes a state and adds the selected value to the value
-- table
addValueFromSelection :: State -> State
addValueFromSelection state =
  state
    & over valueTable (M.insert (state ^. currentParameter) [currentValue state])


-- | Takes a symbol table and returns a symbol table
-- where everything except EnumerationSymbols is removed 
enumerationSymbolsOnly :: SymbolTable -> SymbolTable
enumerationSymbolsOnly = M.filter f
  where f v = case v of
          EnumerationSymbol _ -> True
          _ -> False


chooseValueForParameterStep :: State -> EventM n (Next State)
chooseValueForParameterStep state =
  continue $
      state
        & set currentParameter (currentValue state)
        & set step ChooseValueForParameter
        & set (ui . values) (possibleValues (currentValue state) (state ^. valueTable) (state ^. symbolTable))

-- | Takes a symbol table, a value table and a parameter
-- name of an EnumerationSymbol and returns possible 
-- values for the parameter
-- 
-- Takes Requires-Rules and current values of other
-- parameters into account (that is why a value table
-- is needed)
possibleValues :: String -> ValueTable -> SymbolTable -> Seq String
possibleValues parameter vTable sTable = possibleValuesForEnumerationSymbol parameter vTable (sTable M.! parameter)

-- | Takes a parameter an EnumerationSymbol and a value 
-- table and returns possible values for the EnumerationSymbol
-- with regard to the current value table.
possibleValuesForEnumerationSymbol :: String -> ValueTable -> SymbolInformation -> Seq String
possibleValuesForEnumerationSymbol parameter valueTable (EnumerationSymbol enumValues) =
  fromList $
    map enumValue $
    filter (valueValid parameter valueTable) enumValues
possibleValuesForEnumerationSymbol _ _ _ = error "Called possibleValuesForEnumerationSymbol with non-EnumerationSymbol"

-- | Takes a parameter, an enumeration value and a value
-- table and returns a sequence of possible values with
-- regard to the value table
valueValid :: String -> ValueTable -> EnumerationValue -> Bool
valueValid parameter valueTable (EnumerationValue value requiresRules) =
  foldr (ruleValid parameter valueTable) True requiresRules

-- | Takes a parameter, a value table, a requires rool and an
-- accumulator and returns the accumulator + if the rule is
-- affected
ruleValid :: String -> ValueTable -> RequiresRule -> Bool -> Bool
ruleValid _ _ (SetsValueArea _ _) accum = accum
ruleValid param vTable (RequiresValue secondParam secondValue) accum =
  accum &&
    case M.lookup secondParam vTable of
      Nothing -> True
      Just a -> length a == 1 && head a == secondValue

-- Interface

getIrValues :: String -> InteractionResult -> Either String [String]
getIrValues k vt = Right ["Hello"]

getIrSingleValue :: String -> InteractionResult -> Either String String
getIrSingleValue k vt = Right "Hello"

questionUser :: SymbolTable -> ExceptT String IO InteractionResult
questionUser table = do
  finalState <- lift $ defaultMain app (initialState table)
  except $ Left "Test"