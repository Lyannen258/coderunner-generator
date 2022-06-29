{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Generator.Configuration
  ( Configuration,
    getSingleValue,
    getMultiValue,
    contains,
    evaluateMethod,
  )
where

import Data.Foldable (find)
import Data.List (intercalate)
import Generator.Configuration.Type
import Generator.Helper (maybeToEither, singleton)
import Generator.ParameterName
import Lens.Micro ((^.))
import Text.Read (readMaybe)

getSingleValue :: Configuration -> ParameterName -> Maybe String
getSingleValue c pn = do
  vc <- getValueComponent c pn
  case vc of
    Single sc -> return $ toString (sc ^. selectedValue)
    _ -> Nothing

getMultiValue :: Configuration -> ParameterName -> Maybe [String]
getMultiValue c pn = do
  vc <- getValueComponent c pn
  case vc of
    Single _ -> Nothing
    Multi mc -> return $ map toString (mc ^. selectedValueRange)

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
evaluateMethod conf pn "all" _ = map toString <$> getAllValues conf pn
evaluateMethod conf pn fn@"random" [arg] = do
  allVs <- getAllValues conf pn
  amount <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
  let rs = take amount (randomNumbers conf)
  return $ map (\r -> toString $ allVs !! (r `mod` length allVs)) rs
evaluateMethod conf pn fn@"get" [arg] = case getValueComponent conf pn of
  Nothing -> Left . paramNotSetErr $ pn
  Just (Single sc) -> singleton <$> getTupleX pn arg fn (sc ^. selectedValue)
  Just (Multi mc) -> mapM (getTupleX pn arg fn) (mc ^. selectedValueRange)
evaluateMethod _ _ fnName args = Left $ noMatchingMethodErr fnName args

getTupleX :: ParameterName -> String -> String -> Value -> Either String String
getTupleX pn arg fn v = case v of
  Regular _ -> Left . getButNotATupleErr $ pn
  Tuple ss -> do
    index <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
    if index <= length ss && not (null ss)
      then return $ ss !! index
      else Left $ tupleHasNotEnoughEntriesErr pn index (length ss)

toString :: Value -> String
toString (Regular s) = s
toString (Tuple ss) = head ss

getAllValues :: Configuration -> ParameterName -> Either String [Value]
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
paramNotSetErr pn =
  "Could not infer a value for parameter '"
    ++ unParameterName pn
    ++ "' because it was not declared."

argumentMustBeTypeErr :: String -> String -> String -> String
argumentMustBeTypeErr methodName argPos type' =
  "The " ++ argPos ++ ". argument for " ++ methodName ++ " must be of type " ++ type'

getButNotATupleErr :: ParameterName -> String
getButNotATupleErr pn = "Called get on parameter " ++ unParameterName pn ++ ", but it is not a tuple."

tupleHasNotEnoughEntriesErr :: ParameterName -> Int -> Int -> String
tupleHasNotEnoughEntriesErr pn requested maxVs =
  "Requested element "
    ++ show requested
    ++ " of tuple-parameter "
    ++ unParameterName pn
    ++ " but it has only "
    ++ show maxVs
    ++ " values."