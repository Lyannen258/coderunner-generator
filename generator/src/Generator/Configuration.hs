{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Generator.Configuration
  ( Configuration,
    getSingleValue,
    getMultiValue,
    -- contains,
    evaluateMethod,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Error (MonadError (throwError))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import Generator.Atoms (AtomicValue (value), MultiValue (value), ParameterName (name), SingleValue (value))
import Generator.Configuration.Internal
import Generator.Configuration.Type hiding (name)
import Generator.Helper (maybeToError, singleton)
import Lens.Micro ((^.))
import Text.Read (readMaybe)

getSingleValue :: Configuration -> ParameterName -> Maybe String
getSingleValue c pn = (.value) . (.value) . selectedValue <$> getSingleParameter pn c

getMultiValue :: Configuration -> ParameterName -> Maybe [String]
getMultiValue c pn =
  (map (.value))
    . toList
    . (.value)
    . selectedValue
    <$> getMultiParameter pn c

-- contains :: Configuration -> ParameterName -> Bool
-- contains c pn = case getParameter c pn of
--   Nothing -> False
--   Just _ -> True

evaluateMethod :: MonadError String m => Configuration -> ParameterName -> String -> [String] -> m [String] -- String is function name, First [String] is arguments, returned [String] is result
-- evaluateMethod conf pn "all" _ = do
--   vs <- getAllValues conf pn
--   maybeToEither (mapM toString vs) tupleInsideAnotherValue
-- evaluateMethod conf pn fn@"random" [arg] = do
--   allVs <- getAllValues conf pn
--   amount <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
--   let rs = take amount (randomNumbers conf)
--   let maybeValues = map (\r -> toString $ allVs !! (r `mod` length allVs)) rs
--   maybeToEither (sequence maybeValues) "There were tuple values inside of the randomly choosen values"
evaluateMethod conf pn "get" [arg] =
  maybeToError
    (evaluateGetMethodSingle conf pn arg <|> evaluateGetMethodMulti conf pn arg)
    ("Not possible to find a value in parameter " ++ name pn ++ " with index " ++ arg)
evaluateMethod _ _ fnName args = throwError $ noMatchingMethodErr fnName args

evaluateGetMethodSingle :: Configuration -> ParameterName -> String -> Maybe [String]
evaluateGetMethodSingle conf pn arg = do
  param <- getSingleTupleParameter pn conf
  index <- readMaybe arg
  v <- getValueByIndexSingle param index
  return [v.value]

evaluateGetMethodMulti :: Configuration -> ParameterName -> String -> Maybe [String]
evaluateGetMethodMulti conf pn arg = do
  param <- getMultiTupleParameter pn conf
  index <- readMaybe arg
  values <- getValueByIndexMulti param index
  return $ toList $ fmap (.value) values