{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import ConstraintGraph (Constraint, ConstraintGraph, Edge, Value)
import qualified ConstraintGraph as G
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable (Foldable (toList))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Sequence (Seq, empty, fromList, mapWithIndex, (|>))
import qualified Data.Sequence as Seq (fromList, index)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import qualified Helper as H (index)
import Lens.Micro
import Lens.Micro.Internal (Each)
import Lens.Micro.TH
import SemanticAnalyzer (SymbolInformation (EnumerationSymbol), SymbolTable)

-- Internal stuff to make each work with sequence

instance Each (Seq a) (Seq b) a b where
  each = traverse

-- Data Types

data InteractionResult
  = ValueResult ValueTable
  | AmountResult Int
  deriving (Show)

-- State data type

type ValueTable = M.Map String [String]

type ValueRange = M.Map String [String]

addValue :: Ord a => a -> b -> M.Map a [b] -> M.Map a [b]
addValue key value m =
  if M.member key m
    then M.adjust (value :) key m
    else M.insert key [value] m

data Mode
  = ToBeDetermined
  | Manual
  | Random
  deriving (Show)

data Step
  = ChooseMode
  | ChooseParameter
  | ChooseValueForParameter
  deriving (Eq, Show)

data UIItem = UIItem
  { _identifier :: String,
    _displayText :: String
  }
  deriving (Show)

makeLenses ''UIItem

data UI = UI
  { _values :: Seq UIItem,
    _index :: Int
  }
  deriving (Show)

makeLenses ''UI

data State = State
  { _mode :: Mode,
    _step :: Step,
    _ui :: UI,
    _valueTable :: ValueTable,
    _symbolTable :: SymbolTable,
    _constraintGraph :: G.ConstraintGraph,
    _valueRange :: ValueRange,
    _currentParameter :: String,
    _debug :: String,
    _configs :: [[Value]]
  }

makeLenses ''State

class Beautify a where
  beautify :: a -> String

instance Beautify State where
  beautify s =
    "\nMode: " ++ show (s ^. mode) ++ "\n"
      ++ "Step: "
      ++ show (s ^. step)
      ++ "\n"
      ++ "UI: \n"
      ++ beautify (s ^. ui)
      ++ "\n"
      ++ "ValueTable: \n"
      ++ beautify (s ^. valueTable)
      ++ "\n"
      ++ "valueRange: \n"
      ++ beautify (s ^. valueRange)
      ++ "\n"
      ++ "Current Parameter: "
      ++ s ^. currentParameter
      ++ "\n"
      ++ "Debug: \n"
      ++ s ^. debug

instance (Show a, Show b) => Beautify (M.Map a b) where
  beautify m = concatMap mapper (M.toList m)
    where
      mapper (k, v) = "\t" ++ show k ++ " | " ++ show v ++ "\n"

instance Beautify UI where
  beautify ui =
    "\tIndex: " ++ show (ui ^. index)
      ++ "\n \t"
      ++ " Values:\n"
      ++ beautify (ui ^. values)

instance (Beautify a) => Beautify (Seq a) where
  beautify seq = "\t\t[\n" ++ concatMap beautify seq ++ "\t\t]\n"

instance Beautify UIItem where
  beautify item = "\t\tIdentifier: " ++ (item ^. identifier) ++ "\n"

instance Beautify a => Beautify [a] where
  beautify l = "\t\t[\n" ++ concatMap beautify l ++ "\t\t]\n"

instance Beautify String where
  beautify s = s

initialState :: SymbolTable -> ConstraintGraph -> State
initialState table graph = {- evaluateValueRange -} s
  where
    s =
      State
        { _mode = ToBeDetermined,
          _step = ChooseMode,
          _ui =
            UI
              { _values = empty |> UIItem "manual" "Manual" |> UIItem "random" "Random",
                _index = 0
              },
          _valueTable = M.empty,
          _symbolTable = table,
          _constraintGraph = graph,
          _valueRange = M.empty,
          _currentParameter = "",
          _debug = ""--,
          --_configs = G.configs graph
        }

getValueAmount :: State -> Int
getValueAmount state = length $ state ^. ui . values

currentValue :: State -> UIItem
currentValue state = (state ^. ui . values) `Seq.index` (state ^. ui . index)

currentItemIdentifier :: State -> String
currentItemIdentifier state = currentValue state ^. identifier

stringToUIItem :: String -> UIItem
stringToUIItem s = UIItem s s

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
      ( [str "Choose mode:"]
          ++ drawValues state
          ++ [str ("\n" ++ beautify state)]
      )
  ]

-- | Takes a state and returns a widget for each
-- value in state.ui.values
drawValues :: State -> [Widget n]
drawValues state = toList $ mapWithIndex (value2Widget (state ^. ui . index)) (fromList (state ^.. ui . values . each . displayText))

-- | Takes the currently selected index,
-- the index of the value to be drawed and
-- the visual representation of the value.
--
-- Returns a widget for the value
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
  | state ^. step == ChooseValueForParameter = (chooseParameterStep {-. evaluateSelection-}) state
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
      & set
        (ui . values)
        (buildDataItems (state ^. valueTable) (state ^. symbolTable))

buildDataItems :: ValueTable -> SymbolTable -> Seq UIItem
buildDataItems vTable sTable =
  fromList $
    zipWith
      UIItem
      (M.keys (enumerationSymbolsOnly sTable))
      (visualRepresentations vTable sTable)

visualRepresentations :: ValueTable -> SymbolTable -> [String]
visualRepresentations vTable sTable =
  map (visualRepresentation vTable) (M.keys (enumerationSymbolsOnly sTable))

visualRepresentation :: ValueTable -> String -> String
visualRepresentation vTable s =
  case M.lookup s vTable of
    Nothing -> s
    Just a -> s ++ " = " ++ show a



-- | Takes a symbol table and returns a symbol table
-- where everything except EnumerationSymbols is removed
enumerationSymbolsOnly :: SymbolTable -> SymbolTable
enumerationSymbolsOnly = M.filter f
  where
    f v = case v of
      EnumerationSymbol _ -> True
      _ -> False

chooseValueForParameterStep :: State -> EventM n (Next State)
chooseValueForParameterStep state =
  continue $
    state
      & set currentParameter p
      & set step ChooseValueForParameter
      & set (ui . values) uiValuesSeq
  where
    p = currentItemIdentifier state
    valRangeForP = (state ^. valueRange) M.! p
    uiValues = map (\s -> UIItem s s) valRangeForP
    uiValuesSeq = Seq.fromList uiValues

-- Interface

getIrValues :: String -> InteractionResult -> Either String [String]
getIrValues k vt = Right ["Hello"]

getIrSingleValue :: String -> InteractionResult -> Either String String
getIrSingleValue k vt = Right "Hello"

questionUser :: (SymbolTable, ConstraintGraph) -> ExceptT String IO InteractionResult
questionUser semResult = do
  finalState <- lift $ defaultMain app (uncurry initialState semResult)
  except $ Left "Test"