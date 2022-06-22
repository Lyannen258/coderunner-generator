{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Generator.Configuration
  ( Configuration,
    ParameterName,
    empty,
    addParameter,
    getSingleValue,
    getMultiValue,
    contains,
    evaluateMethod,
  )
where

import Generator.Helper (maybeToEither)
import Data.Foldable (find)
import Data.List (intercalate)
import Lens.Micro ((^.))
import Lens.Micro.TH (makeFields)
import Text.Read (readMaybe)
import System.Random

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

type ParameterName = String

data GeneralInfo = GeneralInfo
  { taskName :: String,
    author :: String,
    fileName :: String,
    outputDirectory :: String
  }
  deriving (Show)

makeFields ''SingleComponent
makeFields ''MultiComponent

empty :: IO Configuration
empty = do
  Configuration [] (GeneralInfo "" "" "" "") . randoms <$> getStdGen -- TODO add real general info

class ParameterValue v where
  addParameter :: String -> v -> [v] -> Configuration -> Configuration

instance ParameterValue String where
  addParameter pn sv avs c = c {parameters = parameters c ++ [Parameter pn (Single (SingleComponent sv avs))]}

instance ParameterValue [String] where
  addParameter pn sv avs c = c {parameters = parameters c ++ [Parameter pn (Multi (MultiComponent sv avs))]}

getSingleValue :: Configuration -> ParameterName -> Maybe String
getSingleValue c pn = do
  vc <- getValueComponent c pn
  case vc of
    Single sc -> return $ sc ^. selectedValue
    Multi _ -> Nothing

getMultiValue :: Configuration -> ParameterName -> Maybe [String]
getMultiValue c pn = do
  vc <- getValueComponent c pn
  case vc of
    Single _ -> Nothing
    Multi mc -> return $ mc ^. selectedValueRange

contains :: Configuration -> ParameterName -> Bool
contains c pn = case getParameter c pn of
  Nothing -> False
  Just _ -> True

getParameter :: Configuration -> ParameterName -> Maybe Parameter
getParameter c pn = find f (parameters c)
  where
    f :: Parameter -> Bool
    f (Parameter pn' _) = pn == pn'

getValueComponent :: Configuration -> ParameterName -> Maybe ValueComponent
getValueComponent c pn = valueComponent <$> getParameter c pn

evaluateMethod :: Configuration -> ParameterName -> String -> [String] -> Either String [String] -- String is function name, First [String] is arguments, returned [String] is result
evaluateMethod conf pn "all" _ = getAllValues conf pn
evaluateMethod conf pn fn@"random" [arg] = do
  allVs <- getAllValues conf pn
  amount <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
  let rs = take amount (randomNumbers conf)
  return $ map (\r -> allVs !! (r `mod` length allVs)) rs
evaluateMethod _ _ fnName args = Left $ noMatchingMethodErr fnName args

getAllValues :: Configuration -> ParameterName -> Either String [String]
getAllValues conf pn = case vc of
  Nothing -> Left $ paramNotSetErr pn
  Just (Single p) -> return $ p ^. allValues
  Just (Multi p) -> return $ p ^. selectedValueRange
  where
    vc = getValueComponent conf pn

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