module Generator.ParseResult
  ( ParseResult,
    Parameter,
    addConstraint,
    addConstraints,
    addParameter,
    empty,
    mergeRanges,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State
import Data.Foldable (foldl', toList)
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Sequence as Seq
import Generator.Atoms
import Generator.ParseResult.Info
import Generator.ParseResult.Type

-- | An empty parse result
empty :: ParseResult
empty = ParseResult $ ParameterComposition [] []

mergeRanges :: (MonadState (ParseResult) m, MonadError String m, Eq a) => Range a -> Range a -> m (Range a)
mergeRanges (Single a) (Single b) =
  (return . Single . RangeType . Seq.fromList . nub)
    (toList a.range ++ toList b.range)
mergeRanges (SingleTuple a) (SingleTuple b) =
  (return . SingleTuple . RangeType . Seq.fromList . nub)
    (toList a.range ++ toList b.range)
mergeRanges (Multi a) (Multi b) =
  (return . Multi . RangeType . Seq.fromList . nub)
    (toList a.range ++ toList b.range)
mergeRanges (MultiTuple a) (MultiTuple b) =
  (return . MultiTuple . RangeType . Seq.fromList . nub)
    (toList a.range ++ toList b.range)
mergeRanges _ _ = throwError "Cannot merge two different ranges"

-- | Add values for a parameter to a 'ParseResult'. Duplicate values
-- will not be added, if there are already values for the parameter.
addParameter :: (MonadState (ParseResult) m, MonadError String m) => Parameter -> m ()
addParameter p = do
  maybeParameter <- getParameterM p.name
  case maybeParameter of
    Nothing -> do
      pr <- get
      put $ ParseResult $ ParameterComposition (pr.comp.parameters ++ [p]) (pr.comp.constraints)
    Just pOld -> do
      pr <- get
      mergedRange <- mergeRanges pOld.range p.range
      put . ParseResult $
        ParameterComposition
          (foldl' (replace $ Parameter pOld.name mergedRange) [] pr.comp.parameters)
          pr.comp.constraints
  where
    replace :: Parameter -> [Parameter] -> Parameter -> [Parameter]
    replace p1@(Parameter n _) acc p2@(Parameter n' _)
      | n' == n = acc ++ [p1]
      | otherwise = acc ++ [p2]

addConstraint :: (ParameterName, Int) -> (ParameterName, Int) -> ParseResultBuilder ()
addConstraint (n1, i1) (n2, i2) = do
  p1 <- getParameterM n1
  p2 <- getParameterM n2
  pr <- get
  case catMaybes [p1, p2] of
    [_, _] -> put . ParseResult $ pr.comp {constraints = pr.comp.constraints ++ [Constraint (n1, i1) (n2, i2)]}
    _ -> throwError "Cannot add constraint for parameters that do not exist."

addConstraints :: (ParameterName, [Int]) -> (ParameterName, [Int]) -> ParseResultBuilder ()
addConstraints (n1, is1) (n2, is2)
  | length is1 == length is2 =
      mapM_
        (uncurry addConstraint)
        (zip [(n1, x) | x <- is1] [(n2, x) | x <- is2])
  | otherwise = throwError "addConstraints: Lists is1 and is2 do not have the same length."

-- | Add a constraint between two parameter values.
-- addConstraint :: IncompleteParseResult -> (ParameterName, a) -> (ParameterName, b) -> Either String IncompleteParseResult
-- addConstraint pr@(ParseResult pc) (p1, v1) (p2, v2) =
--   let pos :: ParameterName -> v -> Maybe Int
--       pos p v = do
--         p' <- getParameter pr p
--         valueIndex p' v

--       constraintsNew :: Maybe [Constraint]
--       constraintsNew = do
--         v1pos <- pos p1 v1
--         v2pos <- pos p2 v2
--         return $ nub $ getConstraints pr ++ [Constraint (p1, v1pos) (p2, v2pos)]
--    in do
--         constraintsNew' <- maybeToEither constraintsNew "Cannot add constraint"
--         return $ ParseResult $ pc {constraints = constraintsNew'}