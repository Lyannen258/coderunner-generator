module Generator.ParseResult
  ( ParseResult,
    ParameterValue,
    Parameter,
    makeParam,
    makeValue,
    addParameter,
    addConstraint,
    empty,
    MakeParam,
  )
where

import Data.Foldable (Foldable (toList), foldl')
import Data.List (nub)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Generator.Helper (maybeToEither)
import Generator.ParameterName
import Generator.ParseResult.Info
import Generator.ParseResult.Type

-- | An empty parse result
empty :: ParseResult
empty = ParseResult $ ParameterComposition [] []

-- Construct a 'Parameter' with different types of values
class MakeParam v where
  makeValue :: v -> ParameterValue
  makeParam :: ParameterName -> [v] -> Parameter

instance MakeParam Value where
  makeParam pn vs = Parameter pn $ Seq.fromList (map SingleValue vs)
  makeValue = SingleValue

instance MakeParam [Value] where
  makeParam pn vs = Parameter pn $ Seq.fromList (map (MultiValue . Seq.fromList) vs)
  makeValue = MultiValue . Seq.fromList

-- | Add values for a parameter to a 'ParseResult'. Duplicate values
-- will not be added, if there are already values for the parameter.
addParameter :: ParseResult -> Parameter -> Either String ParseResult
addParameter pr@(ParseResult pc@(ParameterComposition ps _)) p@(Parameter pn pvs) =
  case getParameter pr pn of
    Nothing -> return $ ParseResult $ pc {parameters = ps ++ [p]}
    Just (Parameter _ pvs2) -> do
      mergedValues <- mergeParameterValues pvs2 pvs
      return $ ParseResult $ pc {parameters = foldl' (replace $ Parameter pn mergedValues) [] ps}
  where
    replace :: Parameter -> [Parameter] -> Parameter -> [Parameter]
    replace p1@(Parameter n _) acc p2@(Parameter n' _)
      | n' == n = acc ++ [p1]
      | otherwise = acc ++ [p2]

-- | Merge two 'ParameterValues'. Removes duplicates.
mergeParameterValues :: Seq ParameterValue -> Seq ParameterValue -> Either String (Seq ParameterValue)
mergeParameterValues vs1 vs2 =
  (Right . Seq.fromList . nub) (toList vs1 ++ toList vs2)

-- | Add a constraint between two parameter values.
addConstraint :: ParseResult -> (ParameterName, ParameterValue) -> (ParameterName, ParameterValue) -> Either String ParseResult
addConstraint pr@(ParseResult pc) v1 v2 =
  let v1pos :: Maybe Int
      v1pos = uncurry (findValueIndex pr) v1

      v2pos :: Maybe Int
      v2pos = uncurry (findValueIndex pr) v2

      constraintsNew :: Maybe [Constraint]
      constraintsNew = do
        v1pos' <- v1pos
        v2pos' <- v2pos
        return $ nub $ getConstraints pr ++ [Constraint (fst v1, v1pos') (fst v2, v2pos')]
   in do
        constraintsNew' <- maybeToEither constraintsNew "Cannot add constraint"
        return $ ParseResult $ pc {constraints = constraintsNew'}
