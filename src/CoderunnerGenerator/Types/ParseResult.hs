module CoderunnerGenerator.Types.ParseResult
  ( ParseResult,
    Constraint,
    ParameterName,
    Value (Final, NeedsInput),
    ValuePart (StringPart, ParameterPart),
    addValues,
    addMultiValues,
    addConstraint,
    addMultiConstraint,
    empty,
    getParameterNames,
    getSingleParameterValues,
    getMultiParameterValues,
    first,
    second,
    getConstraints,
    countValues,
    isSingle,
    isMulti
  )
where

import Data.Foldable (elem, find, foldl')
import Data.List (nub)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Debug.Pretty.Simple (pTraceShowId)

-- | Main data type. Used to pass information from a specialized generator to the main generator.
newtype ParseResult = ParseResult ParameterComposition -- maybe add Enumeration in future
  deriving (Show)

-- | Holds information about the composition of paramters, e.g.
-- - values for parameters
-- - constraints between parameters
data ParameterComposition = ParameterComposition
  { parameters :: [Parameter],
    constraints :: [Constraint]
  }
  deriving (Show)

-- | Simple alias for 'String'
type ParameterName = String

-- | Data type for a parameter. Contains the parameter name and a list of possible values.
data Parameter
  = Parameter ParameterName (Seq Value)
  | MultiParameter ParameterName (Seq (Seq Value))
  deriving (Show)

-- | Data that contains information about a value for a parameter.
data Value
  = -- | Constructor for a fully determined value that does not contain any other parameter.
    Final String
  | -- | Constructor for a value that contains at least one other parameter. It can be fully determined only after the value for the contained parameter was selected.
    NeedsInput [ValuePart]
  deriving (Eq, Show)

-- | Data type that makes up the parts for a not fully determined 'Value'.
data ValuePart
  = -- | Constructor for a constant part, e.g. in @{{ type }}myfunc();@ this would be "myfunc();""
    StringPart String
  | -- | Constructor for a con-constant part (the usage of a parameter that will
    -- be replaced by the value once it is determined), e.g. in @{{type}}myfunc();@ this would be "type"
    ParameterPart ParameterName
  deriving (Eq, Show)

-- | Data type for a constraint between 2 parameter values. The values are identified by their index.
-- First one requires the second one.
data Constraint = Constraint (ParameterName, Int) (ParameterName, Int)
  deriving (Show, Eq)

-- | Add values for a parameter to a 'ParseResult'. Duplicate values
-- will not be added, if there are already values for the parameter.
addValues :: ParseResult -> ParameterName -> [Value] -> ParseResult
addValues pr@(ParseResult pc@(ParameterComposition ps cs)) pn vs =
  ParseResult $ pc {parameters = newParameters}
  where
    currentValues :: Seq Value
    currentValues = getSingleParameterValues pr pn

    allValues :: Seq Value
    allValues = foldl' addIfNotContained currentValues vs

    addIfNotContained :: Seq Value -> Value -> Seq Value
    addIfNotContained vs v
      | v `elem` vs = vs
      | otherwise = vs Seq.|> v

    newParameters :: [Parameter]
    newParameters = foldl' d [] ps ++ [Parameter pn allValues]

    d :: [Parameter] -> Parameter -> [Parameter]
    d ps p@(Parameter name _)
      | name == pn = ps
      | otherwise = ps ++ [p]
    d ps p = ps ++ [p]

addMultiValues :: ParseResult -> ParameterName -> [Value] -> ParseResult
addMultiValues pr@(ParseResult pc@(ParameterComposition ps cs)) pn vs =
  ParseResult $ pc {parameters = newParameters}
  where
    currentValues :: Seq (Seq Value)
    currentValues = getMultiParameterValues pr pn

    allValues :: Seq (Seq Value)
    allValues = addIfNotContained currentValues (Seq.fromList vs)

    addIfNotContained :: Seq (Seq Value) -> Seq Value -> Seq (Seq Value)
    addIfNotContained vs v
      | v `elem` vs = vs
      | otherwise = vs Seq.|> v

    newParameters :: [Parameter]
    newParameters = foldl' d [] ps ++ [MultiParameter pn allValues]

    d :: [Parameter] -> Parameter -> [Parameter]
    d ps p@(MultiParameter name _)
      | name == pn = ps
      | otherwise = ps ++ [p]
    d ps p = ps ++ [p]

-- | Add a constraint between two parameter values.
addConstraint :: ParseResult -> (ParameterName, Value) -> (ParameterName, Value) -> Maybe ParseResult
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
        constraintsNew' <- constraintsNew
        return $ ParseResult $ pc {constraints = constraintsNew'}

findValueIndex :: ParseResult -> ParameterName -> Value -> Maybe Int
findValueIndex pr pn v =
  Seq.elemIndexL v (getSingleParameterValues pr pn)

-- | Add a constraint between two parameter values.
addMultiConstraint :: ParseResult -> (ParameterName, Value) -> (ParameterName, [Value]) -> Maybe ParseResult
addMultiConstraint pr@(ParseResult pc) v1 v2 =
  let v1pos :: Maybe Int
      v1pos = uncurry (findValueIndex pr) v1

      v2pos :: Maybe Int
      v2pos = uncurry findValueIndexMulti v2

      findValueIndexMulti :: ParameterName -> [Value] -> Maybe Int
      findValueIndexMulti pn v =
        Seq.elemIndexL (Seq.fromList v) (getMultiParameterValues pr pn)

      constraintsNew :: Maybe [Constraint]
      constraintsNew = do
        v1pos' <- v1pos
        v2pos' <- v2pos
        return $ nub $ getConstraints pr ++ [Constraint (fst v1, v1pos') (fst v2, v2pos')]
   in do
        constraintsNew' <- constraintsNew
        return $ ParseResult $ pc {constraints = constraintsNew'}

-- | Get all values for a specific parameter.
getSingleParameterValues :: ParseResult -> ParameterName -> Seq Value
getSingleParameterValues ps pm =
  case parameter of
    Just (Parameter _ vs) -> vs
    Just (MultiParameter _ _) -> Seq.empty
    Nothing -> Seq.empty
  where
    parameter :: Maybe Parameter
    parameter = getParameter ps pm

getMultiParameterValues :: ParseResult -> ParameterName -> Seq (Seq Value)
getMultiParameterValues ps pm =
  case parameter of
    Just (Parameter _ _) -> Seq.empty
    Just (MultiParameter _ vs) -> vs
    Nothing -> Seq.empty
  where
    parameter :: Maybe Parameter
    parameter = getParameter ps pm

-- | Get a 'Parameter' from a 'ParseResult' by its name
getParameter :: ParseResult -> ParameterName -> Maybe Parameter
getParameter (ParseResult (ParameterComposition ps _)) pn =
  find f ps
  where
    f :: Parameter -> Bool
    f (Parameter name _) = name == pn
    f (MultiParameter name _) = name == pn

-- | Get a List of the names of all contained Parameters
getParameterNames :: ParseResult -> [ParameterName]
getParameterNames (ParseResult (ParameterComposition ps _)) =
  map f ps
  where
    f :: Parameter -> ParameterName
    f (Parameter name _) = name
    f (MultiParameter name _) = name

-- | Get the amount of values for a parameter
countValues :: ParseResult -> ParameterName -> Int
countValues pr pn = case parameter of
  Nothing -> 0
  Just (Parameter _ vs) -> length vs
  Just (MultiParameter _ vs) -> length vs
  where
    parameter = getParameter pr pn

-- | Get the list of constraints from a 'ParseResult'
getConstraints :: ParseResult -> [Constraint]
getConstraints (ParseResult (ParameterComposition _ cs)) = cs

-- | Get the first tuple of a constraint
first :: Constraint -> (ParameterName, Int)
first (Constraint x _) = x

-- | Get the second tuple of a constraint
second :: Constraint -> (ParameterName, Int)
second (Constraint _ x) = x

-- | An empty parse result
empty :: ParseResult
empty = ParseResult $ ParameterComposition [] []

-- | Checks if the parameter is a single parameter
isSingle :: ParseResult -> ParameterName -> Bool
isSingle pr pn = case param of
  Just (Parameter _ _) -> True
  _ -> False
  where
    param = getParameter pr pn

-- | Checks if the parameter is a multi parameter
isMulti :: ParseResult -> ParameterName -> Bool
isMulti pr pn = case param of
  Just (MultiParameter _ _) -> True
  _ -> False
  where
    param = getParameter pr pn