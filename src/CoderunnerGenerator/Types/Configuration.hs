{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CoderunnerGenerator.Types.Configuration
  ( Configuration,
    Parameter,
    ParameterName,
    empty,
    addSingleParameter,
    addMultiParameter,
    getSingleValue,
    getMultiValue,
    contains,
    evaluateMethod,
  )
where

import CoderunnerGenerator.Helper (maybeToEither)
import Data.Foldable (find)
import Data.List (intercalate)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeFields)
import Text.Read (readMaybe)

data Configuration = Configuration
  { parameters :: [Parameter],
    generalInfo :: GeneralInfo
  }
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

data GeneralInfo = GeneralInfo 
  { taskName :: String,
    author :: String,
    fileName :: String,
    outputDirectory :: String
  }
  deriving (Show)

makeFields ''SingleParameter
makeFields ''MultiParameter

empty :: Configuration
empty = Configuration [] (GeneralInfo "" "" "" "") -- TODO add real general info

addSingleParameter :: String -> String -> [String] -> Configuration -> Configuration
addSingleParameter pn sv avs c = c {parameters = parameters c ++ [Single (SingleParameter pn sv avs)]}

addMultiParameter :: String -> [String] -> [[String]] -> Configuration -> Configuration
addMultiParameter pn sv avs c = c {parameters = parameters c ++ [Multi (MultiParameter pn sv avs)]}

getSingleValue :: Configuration -> ParameterName -> Maybe String
getSingleValue c pn = do
  param <- getParameter c pn
  case param of
    Single sp -> return $ sp ^. selectedValue
    Multi _ -> Nothing

getMultiValue :: Configuration -> ParameterName -> Maybe [String]
getMultiValue c pn = do
  param <- getParameter c pn
  case param of
    Single _ -> Nothing
    Multi mp -> return $ mp ^. selectedValueRange

contains :: Configuration -> ParameterName -> Bool
contains c pn = case getParameter c pn of
  Nothing -> False
  Just _ -> True

getParameter :: Configuration -> ParameterName -> Maybe Parameter
getParameter c pn = find f (parameters c)
  where
    f :: Parameter -> Bool
    f (Single sp) = sp ^. name == pn
    f (Multi mp) = mp ^. name == pn

evaluateMethod :: Configuration -> ParameterName -> String -> [String] -> Either String [String] -- String is function name, First [String] is arguments, returned [String] is result
evaluateMethod conf pn "all" _ = getAllValues conf pn
evaluateMethod conf pn fn@"random" [arg] = do
  allVs <- getAllValues conf pn
  amount <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
  return $ take amount allVs
-- TODO add actual randomness
evaluateMethod _ _ fnName args = Left $ noMatchingMethodErr fnName args

getAllValues :: Configuration -> ParameterName -> Either String [String]
getAllValues conf pn = case param of
  Nothing -> Left $ paramNotSetErr pn
  Just (Single p) -> return $ p ^. allValues
  Just (Multi p) -> return $ p ^. selectedValueRange
  where
    param = getParameter conf pn

noMatchingMethodErr :: String -> [String] -> String
noMatchingMethodErr m args =
  "There is no method with the name '" ++ m
    ++ "' and parameters "
    ++ intercalate ", " args

paramNotSetErr :: ParameterName -> String
paramNotSetErr pn = "Could not infer a value for parameter '" ++ pn ++ "' because it was not declared."

argumentMustBeTypeErr :: String -> String -> String -> String
argumentMustBeTypeErr methodName argPos type' =
  "The " ++ argPos ++ ". argument for " ++ methodName ++ " must be of type " ++ type'