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
  deriving (Show)

data SingleComponent = SingleComponent
  { _singleComponentSelectedValue :: Value,
    _singleComponentAllValues :: [Value]
  }
  deriving (Show)

data MultiComponent = MultiComponent
  { _multiComponentSelectedValueRange :: [Value],
    _multiComponentAllValueRanges :: [[Value]]
  }
  deriving (Show)

data Value = Regular String | Tuple [String]
  deriving (Show)

makeFields ''SingleComponent
makeFields ''MultiComponent