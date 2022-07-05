module Generator.ParseResult
  ( ParseResult,
    Parameter,
    addConstraint,
    addParameter,
    empty,
    makeParam,
    makeRange
  )
where

import Data.Foldable (foldl')
import Data.List (nub)
import Generator.Helper (maybeToEither)
import Generator.ParameterName
import Generator.ParseResult.Info
import Generator.ParseResult.Type

-- | An empty parse result
empty :: ParseResult
empty = ParseResult $ ParameterComposition [] []

-- | Add values for a parameter to a 'ParseResult'. Duplicate values
-- will not be added, if there are already values for the parameter.
addParameter :: ParseResult -> Parameter -> Either String ParseResult
addParameter pr@(ParseResult pc@(ParameterComposition ps _)) p@(Parameter pn r) =
  case getParameter pr pn of
    Nothing -> return $ ParseResult $ pc {parameters = ps ++ [p]}
    Just (Parameter _ r2) -> do
      mergedValues <- mergeRanges' r2 r
      return $ ParseResult $ pc {parameters = foldl' (replace $ Parameter pn mergedValues) [] ps}
  where
    replace :: Parameter -> [Parameter] -> Parameter -> [Parameter]
    replace p1@(Parameter n _) acc p2@(Parameter n' _)
      | n' == n = acc ++ [p1]
      | otherwise = acc ++ [p2]

-- | Merge two 'ParameterValues'. Removes duplicates.
mergeRanges' :: Range -> Range -> Either String Range
mergeRanges' vs1 vs2 = case (vs1, vs2) of
  (Single sr, Single sr2) -> Right . Single $ mergeRanges sr sr2
  (SingleTuple st, SingleTuple st2) -> Right . SingleTuple $ mergeRanges st st2
  (Multi st, Multi st2) -> Right . Multi $ mergeRanges st st2
  (MultiTuple st, MultiTuple st2) -> Right . MultiTuple $ mergeRanges st st2
  _ -> Left "It is not allowed to mix different types of values in the same parameter."

-- | Add a constraint between two parameter values.
addConstraint :: (ValueType vt1 r1, ValueType vt2 r2) => ParseResult -> (ParameterName, vt1) -> (ParameterName, vt2) -> Either String ParseResult
addConstraint pr@(ParseResult pc) (p1, v1) (p2, v2) =
  let pos :: ValueType v r => ParameterName -> v -> Maybe Int
      pos p v = do
        p' <- getParameter pr p
        findValueIndex p' v

      constraintsNew :: Maybe [Constraint]
      constraintsNew = do
        v1pos <- pos p1 v1
        v2pos <- pos p2 v2
        return $ nub $ getConstraints pr ++ [Constraint (p1, v1pos) (p2, v2pos)]
   in do
        constraintsNew' <- maybeToEither constraintsNew "Cannot add constraint"
        return $ ParseResult $ pc {constraints = constraintsNew'}