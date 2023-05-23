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
    countValues,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Data.List (find)
import Data.Sequence qualified as Seq
import Generator.Atoms
import Generator.ParseResult.Type

-- | Get a 'Parameter' from a 'ParseResult' by its name
getParameter :: ParseResult -> ParameterName -> Maybe Parameter
getParameter (ParseResult (ParameterComposition ps _)) pn =
  find f ps
  where
    f :: Parameter -> Bool
    f p = p.name == pn

getParameterM :: MonadState ParseResult m => ParameterName -> m (Maybe Parameter)
getParameterM n = do
  pr <- get
  return $ getParameter pr n

getRangeSingle :: (MonadState ParseResult m, MonadError String m) => ParameterName -> m (SingleRange IncompleteAtomicValue)
getRangeSingle pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (Single r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

getRangeSingleTuple :: (MonadState ParseResult m, MonadError String m) => ParameterName -> m (SingleTupleRange IncompleteAtomicValue)
getRangeSingleTuple pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (SingleTuple r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

getRangeMulti :: (MonadState ParseResult m, MonadError String m) => ParameterName -> m (MultiRange IncompleteAtomicValue)
getRangeMulti pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (Multi r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

getRangeMultiTuple :: (MonadState ParseResult m, MonadError String m) => ParameterName -> m (MultiTupleRange IncompleteAtomicValue)
getRangeMultiTuple pn = do
  param <- getParameterM pn
  case param of
    Just (Parameter _ (MultiTuple r)) -> return r
    _ -> throwError $ pn.name ++ "does not have a single range or does not exist."

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