{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module CoderunnerGenerator.Types.Configuration
  ( Configuration,
    Parameter,
    ParameterName,
    empty,
    addSingleParameter,
    addMultiParameter,
    getSingleValue,
    contains,
  )
where

import Data.Foldable (find)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeFields)

newtype Configuration = Configuration {parameters :: [Parameter]}
  deriving (Show)

data Parameter = Single SingleParameter | Multi MultiParameter
  deriving (Show)

data SingleParameter = SingleParameter
  { _singleParameterName :: ParameterName,
    _singleParameterSelectedValue :: String,
    _singleParameterAllValues :: [String]
  }
  deriving (Show)

data MultiParameter = MultiParameter
  { _multiParameterName :: ParameterName,
    _multiParameterSelectedValueRange :: [String],
    _multiParameterAllValueRanges :: [[String]]
  }
  deriving (Show)

type ParameterName = String

makeFields ''SingleParameter
makeFields ''MultiParameter

empty :: Configuration
empty = Configuration []

addSingleParameter :: String -> String -> [String] -> Configuration -> Configuration
addSingleParameter pn sv avs c = c {parameters = parameters c ++ [Single (SingleParameter pn sv avs)]}

addMultiParameter :: String -> [String] -> [[String]] -> Configuration -> Configuration
addMultiParameter pn sv avs c = c {parameters = parameters c ++ [Multi (MultiParameter pn sv avs)]}

getSingleValue :: Configuration -> ParameterName -> Maybe String
getSingleValue c pn = do
  param <- getParameter c pn
  case param of
    Single sp -> return $ sp ^. selectedValue
    Multi mp -> Nothing

getMultiValue :: Configuration -> ParameterName -> Maybe [String]
getMultiValue c pn = do
  param <- getParameter c pn
  case param of
    Single sp -> Nothing
    Multi mp -> return $ mp ^. selectedValueRange

contains :: Configuration -> ParameterName -> Bool
contains c pn = case getParameter c pn of
  Nothing -> False
  Just pa -> True

getParameter :: Configuration -> ParameterName -> Maybe Parameter
getParameter c pn = find f (parameters c)
  where
    f :: Parameter -> Bool
    f (Single sp) = sp ^. name == pn
    f (Multi mp) = mp ^. name == pn

--getAllValues :: Configuration -> ParameterName -> [String]
--evaluateMethod :: Configuration -> ParameterName -> String -> [String] -> [String] -- String is function name, First [String] is arguments, returned [String] is result