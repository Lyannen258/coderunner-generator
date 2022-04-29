{-# LANGUAGE TemplateHaskell #-}

module CoderunnerGenerator.Types.ConfigurationState where

import CoderunnerGenerator.Types.ConstraintGraph (Node, node', _value, parameter)
import CoderunnerGenerator.Types.SymbolTable ( Value, finalValue)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Extras (view)

-- Configuration State type
data ConfigurationState = ConfigurationState
  { _base :: Set (Set Node), -- all possible configurations
    _remaining :: Set (Set Node) -- configurations left over after selections were made
  }
  deriving (Show)

makeLenses ''ConfigurationState

-- Construction
---------------

-- | Initialize with a set of configurations
init :: Set (Set Node) -> ConfigurationState
init s = ConfigurationState s s

-- Modification
---------------

set :: String -> [String] -> ConfigurationState -> ConfigurationState
set p v cs = ConfigurationState (cs ^. base) remaining'
  where
    selectedValue = node' (p, finalValue v)
    remaining' = S.filter f (cs ^. remaining)
    f config = selectedValue `S.member` config

-- Information
--------------

-- | Possible values for a parameter
for :: String -> ConfigurationState -> [[Value]]
for p cs = S.toList onlyValueStrings
  where
    remainingValues = S.unions $ cs ^. remaining
    filteredByParameter = S.filter f remainingValues
      where
        f n = p == view parameter n
    onlyValueStrings = S.map _value filteredByParameter
