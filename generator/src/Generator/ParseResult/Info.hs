{-# LANGUAGE FunctionalDependencies #-}

module Generator.ParseResult.Info
  ( getParameter,
    getParameterNames,
    findValueIndex,
    makeParam,
    makeRange,
    mergeRanges,
    getValues,
    getConstraints,
    countValues,
    from,
    to,
    containsMultiParamUsage,
    ValueType,
    isSingle,
  )
where

import Data.Foldable (Foldable (toList), find)
import Data.List (nub)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (elemIndexL, fromList)
import Generator.ParameterName (ParameterName)
import Generator.ParseResult.Type

-- | Get a 'Parameter' from a 'ParseResult' by its name
getParameter :: ParseResult -> ParameterName -> Maybe Parameter
getParameter (ParseResult (ParameterComposition ps _)) pn =
  find f ps
  where
    f :: Parameter -> Bool
    f p = name p == pn

class (Eq v) => ValueType v r | v -> r, r -> v where
  findValueIndex :: Parameter -> v -> Maybe Int
  makeRange :: [v] -> r
  makeParam :: ParameterName -> r -> Parameter
  getValues :: r -> [v]
  mergeRanges :: r -> r -> r
  containsMultiParam :: ParseResult -> v -> Bool

instance ValueType RegularValue SingleRange where
  findValueIndex (Parameter _ (Single (SingleRange vs))) v = Seq.elemIndexL v vs
  findValueIndex _ _ = Nothing
  makeRange vs = SingleRange (Seq.fromList vs)
  makeParam pn r = Parameter pn (Single r)
  getValues (SingleRange vs) = toList vs
  mergeRanges r1 r2 = (SingleRange . Seq.fromList . nub) (getValues r1 ++ getValues r2)

  containsMultiParam _ (Final _) = False
  containsMultiParam pr (NeedsInput vps) = any (valuePartContainsMultiParamUsage pr) vps

instance ValueType TupleValue SingleTupleRange where
  findValueIndex (Parameter _ (SingleTuple (SingleTupleRange vs))) v = Seq.elemIndexL v vs
  findValueIndex _ _ = Nothing
  makeRange vs = SingleTupleRange (Seq.fromList vs)
  makeParam pn r = Parameter pn (SingleTuple r)
  getValues (SingleTupleRange vs) = toList vs
  mergeRanges r1 r2 = (SingleTupleRange . Seq.fromList . nub) (getValues r1 ++ getValues r2)

  containsMultiParam pr (Tuple rvs) = any (containsMultiParam pr) rvs

instance ValueType (Seq RegularValue) MultiRange where
  findValueIndex (Parameter _ (Multi (MultiRange vs))) v = Seq.elemIndexL v vs
  findValueIndex _ _ = Nothing
  makeRange vs = MultiRange (Seq.fromList vs)
  makeParam pn r = Parameter pn (Multi r)
  getValues (MultiRange vs) = toList vs
  mergeRanges r1 r2 = (MultiRange . Seq.fromList . nub) (getValues r1 ++ getValues r2)

  containsMultiParam pr rvs = any (containsMultiParam pr) rvs

instance ValueType (Seq TupleValue) MultiTupleRange where
  findValueIndex (Parameter _ (MultiTuple (MultiTupleRange vs))) v = Seq.elemIndexL v vs
  findValueIndex _ _ = Nothing
  makeRange vs = MultiTupleRange (Seq.fromList vs)
  makeParam pn r = Parameter pn (MultiTuple r)
  getValues (MultiTupleRange vs) = toList vs
  mergeRanges r1 r2 = (MultiTupleRange . Seq.fromList . nub) (getValues r1 ++ getValues r2)

  containsMultiParam pr tvs = any (containsMultiParam pr) tvs

containsMultiParamUsage :: ParseResult -> ParameterName -> Bool
containsMultiParamUsage pr pn = case getParameter pr pn of
  Nothing -> False
  Just (Parameter _ (Single (SingleRange vs))) -> any (containsMultiParam pr) vs
  Just (Parameter _ (Multi (MultiRange vs))) -> any (containsMultiParam pr) vs
  Just (Parameter _ (SingleTuple (SingleTupleRange vs))) -> any (containsMultiParam pr) vs
  Just (Parameter _ (MultiTuple (MultiTupleRange vs))) -> any (containsMultiParam pr) vs

name :: Parameter -> ParameterName
name (Parameter n _) = n

getParameterNames :: ParseResult -> [ParameterName]
getParameterNames (ParseResult (ParameterComposition ps _)) =
  map name ps

-- | Get the list of constraints from a 'ParseResult'
getConstraints :: ParseResult -> [Constraint]
getConstraints (ParseResult (ParameterComposition _ cs)) = cs

-- | Get the first tuple of a constraint
from :: Constraint -> (ParameterName, Int)
from (Constraint x _) = x

-- | Get the second tuple of a constraint
to :: Constraint -> (ParameterName, Int)
to (Constraint _ x) = x

countValues :: ParseResult -> ParameterName -> Int
countValues pr pn = case getParameter pr pn of
  Nothing -> 0
  Just (Parameter _ range) -> case range of
    Single r -> length . getValues $ r
    SingleTuple r -> length . getValues $ r
    Multi r -> length . getValues $ r
    MultiTuple r -> length . getValues $ r

isMulti :: ParseResult -> ParameterName -> Bool
isMulti pr pn = case getParameter pr pn of
  Just (Parameter _ (Multi _)) -> True
  Just (Parameter _ (MultiTuple _)) -> True
  _ -> False

isSingle :: ParseResult -> ParameterName -> Bool
isSingle pr pn = case getParameter pr pn of
  Just (Parameter _ (Single _)) -> True
  Just (Parameter _ (SingleTuple _)) -> True
  _ -> False

valuePartContainsMultiParamUsage :: ParseResult -> ValuePart -> Bool
valuePartContainsMultiParamUsage _ (StringPart _) = False
valuePartContainsMultiParamUsage pr (ParameterPart pn) = isMulti pr pn
valuePartContainsMultiParamUsage pr (TupleSelect pn _) = isMulti pr pn

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