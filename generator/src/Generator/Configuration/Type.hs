{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator.Configuration.Type where

import Generator.ParameterName (ParameterName)
import Lens.Micro.TH (makeFields)

data Configuration = Configuration
  { parameters :: [Parameter],
    generalInfo :: GeneralInfo,
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
  { _singleComponentSelectedValue :: String,
    _singleComponentAllValues :: [String]
  }
  deriving (Show)

data MultiComponent = MultiComponent
  { _multiComponentSelectedValueRange :: [String],
    _multiComponentAllValueRanges :: [[String]]
  }
  deriving (Show)

data GeneralInfo = GeneralInfo
  { taskName :: String,
    author :: String,
    fileName :: String,
    outputDirectory :: String
  }
  deriving (Show)

makeFields ''SingleComponent
makeFields ''MultiComponent