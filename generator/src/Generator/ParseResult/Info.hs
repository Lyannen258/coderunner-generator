{-# LANGUAGE FunctionalDependencies #-}

module Generator.ParseResult.Info
  ( getParameter,
    getParameterM,
    getParameterNames,
    getConstraints,
    from,
    to,
    valueIndex,
    getRangeSingle,
    getRangeSingleTuple,
    getRangeMulti,
    getRangeMultiTuple,
    countValues
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (Foldable (toList), find)
import Data.List (nub)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq (elemIndexL, empty, fromList)
import Generator.Atoms
import Generator.Helper (maybeToEither)
import Generator.ParseResult.Type

-- | Get a 'Parameter' from a 'ParseResult' by its name
getParameter :: ParseResult -> ParameterName -> Maybe (Parameter)
getParameter (ParseResult (ParameterComposition ps _)) pn =
  find f ps
  where
    f :: Parameter -> Bool
    f p = p.name == pn

getParameterM :: MonadState (ParseResult) m => ParameterName -> m (Maybe (Parameter))
getParameterM n = do
  pr <- get
  return $ getParameter pr n

getRangeSingle :: (MonadState (ParseResult) m, MonadError String m) => ParameterName -> m (SingleRange IncompleteAtomicValue)
getRangeSingle pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (Single r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

getRangeSingleTuple :: (MonadState (ParseResult) m, MonadError String m) => ParameterName -> m (SingleTupleRange IncompleteAtomicValue)
getRangeSingleTuple pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (SingleTuple r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

getRangeMulti :: (MonadState (ParseResult) m, MonadError String m) => ParameterName -> m (MultiRange IncompleteAtomicValue)
getRangeMulti pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (Multi r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

getRangeMultiTuple :: (MonadState (ParseResult) m, MonadError String m) => ParameterName -> m (MultiTupleRange IncompleteAtomicValue)
getRangeMultiTuple pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (MultiTuple r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

-- containsMultiParamUsage :: IncompleteParseResult -> ParameterName -> Bool
-- containsMultiParamUsage pr pn = case getParameter pr pn of
--   Nothing -> False
--   Just (Parameter _ (Single (SingleRange vs))) -> any (containsMultiParam pr) vs
--   Just (Parameter _ (Multi (MultiRange vs))) -> (any . any) (containsMultiParam pr) vs
--   Just (Parameter _ (SingleTuple (SingleTupleRange vs))) -> any (containsMultiParam pr) vs
--   Just (Parameter _ (MultiTuple (MultiTupleRange vs))) -> any (any (any (containsMultiParam pr) . unTuple)) vs

-- containsMultiParam :: IncompleteParseResult -> IncompleteAtomicValue -> Bool
-- containsMultiParam _ (Final _) = False
-- containsMultiParam pr (NeedsInput vps) = any (valuePartContainsMultiParamUsage pr) vps

-- valuePartContainsMultiParamUsage :: ParseResult -> ValuePart -> Bool
-- valuePartContainsMultiParamUsage _ (StringPart _) = False
-- valuePartContainsMultiParamUsage pr (ParameterPart pn) = isMulti pr pn
-- valuePartContainsMultiParamUsage pr (TupleSelect pn _) = isMulti pr pn

getParameterNames :: ParseResult -> [ParameterName]
getParameterNames (ParseResult (ParameterComposition ps _)) =
  map (.name) ps

-- | Get the list of constraints from a 'ParseResult'
getConstraints :: ParseResult -> [Constraint]
getConstraints (ParseResult (ParameterComposition _ cs)) = cs

-- | Get the first tuple of a constraint
from :: Constraint -> (ParameterName, Int)
from (Constraint x _) = x

-- | Get the second tuple of a constraint
to :: Constraint -> (ParameterName, Int)
to (Constraint _ x) = x

valueIndex :: Eq (v a) => v a -> RangeType v a -> Maybe Int
valueIndex v r = Seq.elemIndexL v r.range

countValues :: ParseResult -> ParameterName -> Int
countValues pr pn = case getParameter pr pn of
  Nothing -> 0
  Just (Parameter _ range) -> case range of
    Single r -> length r.range
    SingleTuple r -> length r.range
    Multi r -> length r.range
    MultiTuple r -> length r.range

-- isMulti :: ParseResult -> ParameterName -> Bool
-- isMulti pr pn = case getParameter pr pn of
--   Just (Parameter _ (Multi _)) -> True
--   Just (Parameter _ (MultiTuple _)) -> True
--   _ -> False

-- isSingle :: ParseResult -> ParameterName -> Bool
-- isSingle pr pn = case getParameter pr pn of
--   Just (Parameter _ (Single _)) -> True
--   Just (Parameter _ (SingleTuple _)) -> True
--   _ -> False

-- | Check if parameter contains a multi parameter usage somewhere in its values

{- containsMultiParamUsage :: ParseResult -> ParameterName -> Bool
containsMultiParamUsage pr n = case getParameter pr n of
  Nothing -> False
  Just (Parameter _ range) -> getValues range
   -}

{-
any f $ getAllValues pr n
where
  f :: Value -> Bool
  f (RegularValue rv) = f' rv
  f (TupleValue (Tuple _)) = False

  f' :: RegularValue -> Bool
  f' (Final _) = False
  f' (NeedsInput vs) = any (valuePartContainsMultiParamUsage pr) vs -}