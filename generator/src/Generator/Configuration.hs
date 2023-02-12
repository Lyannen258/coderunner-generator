{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Generator.Configuration
  ( Configuration,
    -- getSingleValue,
    -- getMultiValue,
    -- contains,
    -- evaluateMethod,
  )
where

import Generator.Configuration.Internal
import Generator.Configuration.Type
import Generator.Helper (maybeToEither, singleton)
import Generator.Atoms (ParameterName)
import Lens.Micro ((^.))
import Text.Read (readMaybe)

-- getSingleValue :: Configuration -> ParameterName -> Maybe String
-- getSingleValue c pn = do
--   vc <- getValueComponent c pn
--   case vc of
--     Single sc -> toString (sc ^. selectedValue)
--     _ -> Nothing

-- getMultiValue :: Configuration -> ParameterName -> Maybe [String]
-- getMultiValue c pn = do
--   vs <- getMultiValue' c pn
--   mapM toString vs

-- contains :: Configuration -> ParameterName -> Bool
-- contains c pn = case getParameter c pn of
--   Nothing -> False
--   Just _ -> True

-- evaluateMethod :: Configuration -> ParameterName -> String -> [String] -> Either String [String] -- String is function name, First [String] is arguments, returned [String] is result
-- evaluateMethod conf pn "all" _ = do
--   vs <- getAllValues conf pn
--   maybeToEither (mapM toString vs) tupleInsideAnotherValue
-- evaluateMethod conf pn fn@"random" [arg] = do
--   allVs <- getAllValues conf pn
--   amount <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
--   let rs = take amount (randomNumbers conf)
--   let maybeValues = map (\r -> toString $ allVs !! (r `mod` length allVs)) rs
--   maybeToEither (sequence maybeValues) "There were tuple values inside of the randomly choosen values"
-- evaluateMethod conf pn fn@"get" [arg] = case getValueComponent conf pn of
--   Nothing -> Left . paramNotSetErr $ pn
--   Just (Single sc) -> singleton <$> getTupleX pn arg fn (sc ^. selectedValue)
--   Just (Multi mc) -> mapM (getTupleX pn arg fn) (mc ^. selectedValueRange)
-- evaluateMethod _ _ fnName args = Left $ noMatchingMethodErr fnName args
