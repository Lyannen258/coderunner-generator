module Generator.Configuration.Internal where

import Control.Applicative ((<|>))
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List (find, intercalate)
import Generator.Atoms
import Generator.Atoms (ParameterName (..))
import Generator.Configuration.Type
import Generator.Helper (maybeToError)
import Generator.ParseResult.Type qualified as PR
import Lens.Micro ((^.))
import System.Random (getStdGen, randoms)
import Text.Read (readMaybe)
import qualified Data.Sequence as Seq

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

parameterType :: (MonadState Configuration m, MonadError String m) => ParameterName -> m (Maybe ParameterType)
parameterType pn = do
  c <- get
  return $
    f c.singleParameters SingleT
      <|> f c.singleParameters SingleT
      <|> f c.singleParameters SingleT
      <|> f c.singleParameters SingleT
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

-- getParameter :: Configuration -> ParameterName -> Maybe Parameter
-- getParameter c pn = find f (parameters c)
--   where
--     f :: Parameter -> Bool
--     f (Parameter pn' _) = pn == pn'

-- getValueComponent :: Configuration -> ParameterName -> Maybe ValueComponent
-- getValueComponent c pn = valueComponent <$> getParameter c pn

-- getMultiValue' :: Configuration -> ParameterName -> Maybe [Value]
-- getMultiValue' c pn = do
--   vc <- getValueComponent c pn
--   case vc of
--     Single _ -> Nothing
--     Multi mc -> Just $ mc ^. selectedValueRange

-- getTupleX :: ParameterName -> String -> String -> Value -> Either String String
-- getTupleX pn arg fn v = case v of
--   Regular _ -> Left . getButNotATupleErr $ pn
--   Tuple ss -> do
--     index <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
--     if index <= length ss && not (null ss)
--       then return $ ss !! index
--       else Left $ tupleHasNotEnoughEntriesErr pn index (length ss)

-- toString :: Value -> Maybe String
-- toString (Regular s) = Just s
-- toString (Tuple _) = Nothing

-- getAllValues :: Configuration -> ParameterName -> Either String [Value]
-- getAllValues conf pn = case vc of
--   Nothing -> Left $ paramNotSetErr pn
--   Just (Single p) -> return $ p ^. allValues
--   Just (Multi p) -> return $ p ^. selectedValueRange
--   where
--     vc = getValueComponent conf pn

noMatchingMethodErr :: String -> [String] -> String
noMatchingMethodErr m args =
  "There is no method with the name '"
    ++ m
    ++ "' and parameters "
    ++ intercalate ", " args

-- paramNotSetErr :: ParameterName -> String
-- paramNotSetErr pn =
--   "Could not infer a value for parameter '"
--     ++ pn.name
--     ++ "' because it was not declared."

-- argumentMustBeTypeErr :: String -> String -> String -> String
-- argumentMustBeTypeErr methodName argPos type' =
--   "The " ++ argPos ++ ". argument for " ++ methodName ++ " must be of type " ++ type'

-- getButNotATupleErr :: ParameterName -> String
-- getButNotATupleErr pn = "Called get on parameter " ++ pn.name ++ ", but it is not a tuple."

-- tupleHasNotEnoughEntriesErr :: ParameterName -> Int -> Int -> String
-- tupleHasNotEnoughEntriesErr pn requested maxVs =
--   "Requested element "
--     ++ show requested
--     ++ " of tuple-parameter "
--     ++ pn.name
--     ++ " but it has only "
--     ++ show maxVs
--     ++ " values."