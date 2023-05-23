module Generator.Configuration.Internal where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, intercalate)
import Data.Sequence qualified as Seq
import Generator.Atoms
import Generator.Configuration.Type
import Generator.Helper (maybeToError)
import Generator.ParseResult.Type qualified as PR
import System.Random (getStdGen, randoms)

empty :: IO Configuration
empty = do
  Configuration [] [] [] [] . randoms <$> getStdGen

getSelection :: (MonadReader (ConfigRaw, PR.ParseResult) m, MonadError String m) => ParameterName -> m Int
getSelection pn = do
  cr <- asks fst
  case lookup pn cr.config of
    Nothing -> throwError ("getSelection: Parameter" ++ pn.name ++ " does not exist in ConfigRaw")
    Just i -> return i

addSingleParameter :: SingleParameter -> ConfigurationM ()
addSingleParameter p = do
  c <- get
  put $ c {singleParameters = c.singleParameters ++ [p]}
  return ()

addSingleTupleParameter :: SingleTupleParameter -> ConfigurationM ()
addSingleTupleParameter p = do
  c <- get
  put $ c {singleTupleParameters = c.singleTupleParameters ++ [p]}
  return ()

addMultiParameter :: MultiParameter -> ConfigurationM ()
addMultiParameter p = do
  c <- get
  put $ c {multiParameters = c.multiParameters ++ [p]}
  return ()

addMultiTupleParameter :: MultiTupleParameter -> ConfigurationM ()
addMultiTupleParameter p = do
  c <- get
  put $ c {multiTupleParameters = c.multiTupleParameters ++ [p]}
  return ()

data ParameterType = SingleT | SingleTupleT | MultiT | MultiTupleT
  deriving (Show)

parameterType :: (MonadState Configuration m, MonadError String m) => ParameterName -> m (Maybe ParameterType)
parameterType pn = do
  c <- get
  return $
    f c.singleParameters SingleT
      <|> f c.singleTupleParameters SingleTupleT
      <|> f c.multiParameters MultiT
      <|> f c.multiTupleParameters MultiTupleT
  where
    f :: [Parameter v a] -> ParameterType -> Maybe ParameterType
    f container ret = case findParameter container pn of
      Just _ -> Just ret
      Nothing -> Nothing

getSingleParameter :: ParameterName -> Configuration -> Maybe SingleParameter
getSingleParameter pn c = findParameter c.singleParameters pn

getSingleParameterM :: (MonadState Configuration m, MonadError String m) => ParameterName -> m SingleParameter
getSingleParameterM pn = do
  c <- get
  maybeToError (getSingleParameter pn c) $
    "getSingleParameter: " ++ pn.name ++ " is not a single parameter or does not exist"

getMultiParameter :: ParameterName -> Configuration -> Maybe MultiParameter
getMultiParameter pn c = findParameter c.multiParameters pn

getMultiParameterM :: (MonadState Configuration m, MonadError String m) => ParameterName -> m MultiParameter
getMultiParameterM pn = do
  c <- get
  maybeToError (getMultiParameter pn c) $
    "getMultiParameter: " ++ pn.name ++ " is not a multi parameter or does not exist"

getSingleTupleParameter :: ParameterName -> Configuration -> Maybe SingleTupleParameter
getSingleTupleParameter pn c = findParameter c.singleTupleParameters pn

getSingleTupleParameterM :: (MonadState Configuration m, MonadError String m) => ParameterName -> m SingleTupleParameter
getSingleTupleParameterM pn = do
  c <- get
  maybeToError (getSingleTupleParameter pn c) $
    "getSingleTupleParameter: " ++ pn.name ++ " is not a single-tuple parameter or does not exist"

getMultiTupleParameter :: ParameterName -> Configuration -> Maybe MultiTupleParameter
getMultiTupleParameter pn c = findParameter c.multiTupleParameters pn

getMultiTupleParameterM :: (MonadState Configuration m, MonadError String m) => ParameterName -> m MultiTupleParameter
getMultiTupleParameterM pn = do
  c <- get
  maybeToError (getMultiTupleParameter pn c) $
    "getMultiTupleParameter: " ++ pn.name ++ " is not a multi-tuple parameter or does not exist"

findParameter :: [Parameter v a] -> ParameterName -> Maybe (Parameter v a)
findParameter ps pn = find (\p -> p.name == pn) ps

getValueByIndexSingle :: SingleTupleParameter -> Int -> Maybe AtomicValue
getValueByIndexSingle stp i = Seq.lookup i stp.selectedValue.value

getValueByIndexMulti :: MultiTupleParameter -> Int -> Maybe (Seq.Seq AtomicValue)
getValueByIndexMulti mtp i = Seq.lookup i mtp.selectedValue.value

noMatchingMethodErr :: String -> [String] -> String
noMatchingMethodErr m args =
  "There is no method with the name '"
    ++ m
    ++ "' and parameters "
    ++ intercalate ", " args
