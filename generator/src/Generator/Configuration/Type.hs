{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator.Configuration.Type where

import Generator.ParameterName (ParameterName)
import Lens.Micro.TH (makeFields)

data Configuration = Configuration
  { parameters :: [Parameter],
    randomNumbers :: [Int]
  }
  deriving (Show)

data Parameter = Parameter
  { name :: ParameterName,
    valueComponent :: ValueComponent
  }
  deriving (Show)

data ValueComponent = Single SingleComponent | Multi MultiComponent
  deriving (Show, Eq)

data SingleComponent = SingleComponent
  { _singleComponentSelectedValue :: Value,
    _singleComponentAllValues :: [Value]
  }
  deriving (Show, Eq)

data MultiComponent = MultiComponent
  { _multiComponentSelectedValueRange :: [Value],
    _multiComponentAllValueRanges :: [[Value]]
  }
  deriving (Show, Eq)

data Value = Regular String | Tuple [String]
  deriving (Show, Eq)

tupleInsideAnotherValue :: String
tupleInsideAnotherValue =
  "Found a tuple-parameter inside of the value range of another parameter."

makeFields ''SingleComponent
makeFields ''MultiComponent