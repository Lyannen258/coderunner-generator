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
import Data.Sequence (Seq, empty, fromList, length, mapWithIndex, (|>))
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Lens.Micro
import Lens.Micro.TH -- for makeLenses
-- for makeLenses
import SemanticAnalyzer (SymbolInformation, SymbolTable)
import Text.ParserCombinators.Parsec.Char (tab)

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
    _symbolTable :: SymbolTable
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
      _symbolTable = table
    }

getValueAmount :: State -> Int
getValueAmount state = Data.Sequence.length $ state ^. ui . values

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
        & set (ui . values) (fromList (M.keys (state ^. symbolTable))) -- Schreibweise: https://mail.haskell.org/pipermail/haskell-cafe/2013-November/111288.html

-- Interface

getIrValues :: String -> InteractionResult -> Either String [String]
getIrValues k vt = Right ["Hello"]

getIrSingleValue :: String -> InteractionResult -> Either String String
getIrSingleValue k vt = Right "Hello"

questionUser :: SymbolTable -> ExceptT String IO InteractionResult
questionUser table = do
  finalState <- lift $ defaultMain app (initialState table)
  except $ Left "Test"