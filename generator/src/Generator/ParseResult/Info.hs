module Generator.ParseResult.Info (
    findValueIndex,
    getAtIndex,
    getAtIndexSingle,
    getAtIndexMulti,
    getParameterValues,
    getParameter,
    getParameterNames,
    countValues,
    getConstraints,
    containsMultiParamUsage,
    first,
    second,
    isSingle,
    isMulti
) where

import Data.Foldable (Foldable (foldl', toList), find)
import Data.Sequence as Seq (Seq, elemIndexL, length, lookup)
import Generator.ParameterName (ParameterName, unParameterName)
import Generator.ParseResult.Type
  ( Constraint (..),
    Parameter (..),
    ParameterComposition (ParameterComposition),
    ParameterValue (..),
    ParseResult (..),
    Value (..),
    ValuePart (..),
    RegularValue (..),
    TupleValue (..)
  )

findValueIndex :: ParseResult -> ParameterName -> ParameterValue -> Maybe Int
findValueIndex pr pn v = case getParameterValues pr pn of
  Left _ -> Nothing
  Right vs -> Seq.elemIndexL v vs

getAtIndex :: ParseResult -> ParameterName -> Int -> Either String ParameterValue
getAtIndex pr pn i = case getParameterValues pr pn of
  Left s -> Left s
  Right vs -> getE vs
  where
    getE :: Seq a -> Either String a
    getE s = case Seq.lookup i s of
      Nothing -> Left "Index out of bounds"
      Just a -> Right a

getAtIndexSingle :: ParseResult -> ParameterName -> Int -> Either String Value
getAtIndexSingle pr pn i = case getAtIndex pr pn i of
  Left s -> Left s
  Right (SingleValue v) -> return v
  Right (MultiValue _) -> Left $ unParameterName pn ++ " is not a single parameter."

getAtIndexMulti :: ParseResult -> ParameterName -> Int -> Either String (Seq Value)
getAtIndexMulti pr pn i = case getAtIndex pr pn i of
  Left s -> Left s
  Right (MultiValue v) -> return v
  Right (SingleValue _) -> Left $ unParameterName pn ++ " is not a multi parameter."

-- | Get all values for a specific parameter.
getParameterValues :: ParseResult -> ParameterName -> Either String (Seq ParameterValue)
getParameterValues ps pn =
  case parameter of
    Just (Parameter _ pvs) -> Right pvs
    Nothing -> Left $ "No values for parameter " ++ unParameterName pn
  where
    parameter :: Maybe Parameter
    parameter = getParameter ps pn

-- | Get a 'Parameter' from a 'ParseResult' by its name
getParameter :: ParseResult -> ParameterName -> Maybe Parameter
getParameter (ParseResult (ParameterComposition ps _)) pn =
  find f ps
  where
    f :: Parameter -> Bool
    f (Parameter name _) = name == pn

-- | Get a List of the names of all contained Parameters
getParameterNames :: ParseResult -> [ParameterName]
getParameterNames (ParseResult (ParameterComposition ps _)) =
  map f ps
  where
    f :: Parameter -> ParameterName
    f (Parameter name _) = name

-- | Get the amount of values for a parameter
countValues :: ParseResult -> ParameterName -> Int
countValues pr pn = case parameter of
  Nothing -> 0
  Just (Parameter _ vs) -> Seq.length vs
  where
    parameter = getParameter pr pn

-- | Get the list of constraints from a 'ParseResult'
getConstraints :: ParseResult -> [Constraint]
getConstraints (ParseResult (ParameterComposition _ cs)) = cs

-- | Check if parameter contains a multi parameter usage somewhere in its values
containsMultiParamUsage :: ParseResult -> ParameterName -> Bool
containsMultiParamUsage pr n = any f $ getAllValues pr n
  where
    f :: Value -> Bool
    f (RegularValue rv) = f' rv
    f (TupleValue (Tuple _)) = False

    f' :: RegularValue -> Bool
    f' (Final _) = False
    f' (NeedsInput vs) = any (valuePartContainsMultiParamUsage pr) vs

valuePartContainsMultiParamUsage :: ParseResult -> ValuePart -> Bool
valuePartContainsMultiParamUsage _ (StringPart _) = False
valuePartContainsMultiParamUsage pr (ParameterPart pn) = isMulti pr pn

-- | Get all 'Value's of a parameter, no matter if single or multi
getAllValues :: ParseResult -> ParameterName -> [Value]
getAllValues pr pn = case getParameter pr pn of
  Nothing -> []
  Just (Parameter _ vs) -> foldl' f [] vs
  where
    f :: [Value] -> ParameterValue -> [Value]
    f vs pv = vs ++ getAllValuesFromParameterValue pv

getAllValuesFromParameterValue :: ParameterValue -> [Value]
getAllValuesFromParameterValue (SingleValue v) = [v]
getAllValuesFromParameterValue (MultiValue vs) = toList vs

-- | Get the first tuple of a constraint
first :: Constraint -> (ParameterName, Int)
first (Constraint x _) = x

-- | Get the second tuple of a constraint
second :: Constraint -> (ParameterName, Int)
second (Constraint _ x) = x

-- | Checks if the parameter is a single parameter
isSingle :: ParseResult -> ParameterName -> Bool
isSingle pr pn = case param of
  Just (Parameter _ vs) -> case Seq.lookup 0 vs of
    Just (SingleValue _) -> True
    _ -> False
  _ -> False
  where
    param = getParameter pr pn

-- | Checks if the parameter is a multi parameter
isMulti :: ParseResult -> ParameterName -> Bool
isMulti pr pn = case param of
  Just (Parameter _ vs) -> case Seq.lookup 0 vs of
    Just (MultiValue _) -> True
    _ -> False
  _ -> False
  where
    param = getParameter pr pn