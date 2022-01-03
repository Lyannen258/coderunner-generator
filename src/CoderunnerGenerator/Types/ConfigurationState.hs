{-# LANGUAGE TemplateHaskell #-}

module CoderunnerGenerator.Types.ConfigurationState where

import CoderunnerGenerator.Types.ConstraintGraph (Value (Value), _value)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as S
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)

-- Configuration State type
data ConfigurationState = ConfigurationState
  { _base :: Set (Set Value), -- all possible configurations
    _remaining :: Set (Set Value) -- configurations left over after selections were made
  }
  deriving (Show)

makeLenses ''ConfigurationState

-- Construction
---------------

-- | Initialize with a set of configurations
init :: Set (Set Value) -> ConfigurationState
init s = ConfigurationState s s

-- Modification
---------------

set :: String -> [String] -> ConfigurationState -> ConfigurationState
set p v cs = ConfigurationState (cs ^. base) remaining'
  where
    selectedValue = Value p v
    remaining' = S.filter f (cs ^. remaining)
    f config = selectedValue `S.member` config

-- Information
--------------

-- | Possible values for a parameter
for :: String -> ConfigurationState -> [[String]]
for p cs = S.toList onlyValueStrings
  where
    remainingValues = S.unions $ cs ^. remaining
    filteredByParameter = S.filter f remainingValues
      where
        f (Value param v) = p == param
    onlyValueStrings = S.map _value filteredByParameter
