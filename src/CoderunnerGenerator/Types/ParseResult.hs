module CoderunnerGenerator.Types.ParseResult
  ( ParseResult,
    Constraint,
    ParameterName,
    Value (Final, NeedsInput),
    ValuePart (StringPart, ParameterPart),
    ParameterValue,
    singleValue,
    multiValue,
    singleParam,
    multiParam,
    addParameter,
    addConstraint,
    empty,
    getParameterNames,
    getParameterValues,
    first,
    second,
    getConstraints,
    countValues,
    isSingle,
    isMulti,
    getAtIndex,
    getAtIndexSingle,
    getAtIndexMulti,
  )
where

import CoderunnerGenerator.Helper (maybeToEither)
import Data.Foldable (Foldable (toList), elem, find, foldl')
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
  = Parameter ParameterName (Seq ParameterValue)
  deriving (Show)

-- | A parameter value. Needed to differentiate between single values and value ranges
data ParameterValue
  = SingleValue Value
  | MultiValue (Seq Value)
  deriving (Show, Eq)

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

-- | Construct a 'Parameter' with 'SingleValues'
singleParam :: ParameterName -> [Value] -> Parameter
singleParam pn vs = Parameter pn $ Seq.fromList (map SingleValue vs)

-- | Construct a 'Parameter' with 'MultiValues'
multiParam :: ParameterName -> [[Value]] -> Parameter
multiParam pn vs = Parameter pn $ Seq.fromList (map (MultiValue . Seq.fromList) vs)

-- | Construct a single value
singleValue :: Value -> ParameterValue
singleValue = SingleValue

-- | Construct a multi value
multiValue :: [Value] -> ParameterValue
multiValue = MultiValue . Seq.fromList

-- | Add values for a parameter to a 'ParseResult'. Duplicate values
-- will not be added, if there are already values for the parameter.
addParameter :: ParseResult -> Parameter -> Either String ParseResult
addParameter pr@(ParseResult pc@(ParameterComposition ps cs)) p@(Parameter pn pvs) =
  case getParameter pr pn of
    Nothing -> return $ ParseResult $ pc {parameters = ps ++ [p]}
    Just (Parameter _ pvs2) -> do
      mergedValues <- mergeParameterValues pvs2 pvs
      return $ ParseResult $ pc {parameters = foldl' (replace $ Parameter pn mergedValues) [] ps}
  where
    replace :: Parameter -> [Parameter] -> Parameter -> [Parameter]
    replace p1@(Parameter pn _) acc p2@(Parameter pn' _)
      | pn' == pn = acc ++ [p1]
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

findValueIndex :: ParseResult -> ParameterName -> ParameterValue -> Maybe Int
findValueIndex pr pn v = case getParameterValues pr pn of
  Left s -> Nothing
  Right vs -> Seq.elemIndexL v vs

getAtIndex :: ParseResult -> ParameterName -> Int -> Either String ParameterValue
getAtIndex pr pn i = case getParameterValues pr pn of
  Left s -> Left s
  Right vs -> getE vs i
  where
    getE :: Seq a -> Int -> Either String a
    getE seq i = case Seq.lookup i seq of
      Nothing -> Left "Index out of bounds"
      Just a -> Right a

getAtIndexSingle :: ParseResult -> ParameterName -> Int -> Either String Value
getAtIndexSingle pr pn i = case getAtIndex pr pn i of
  Left s -> Left s
  Right (SingleValue v) -> return v
  Right (MultiValue v) -> Left $ pn ++ " is not a single parameter."

getAtIndexMulti :: ParseResult -> ParameterName -> Int -> Either String (Seq Value)
getAtIndexMulti pr pn i = case getAtIndex pr pn i of
  Left s -> Left s
  Right (MultiValue v) -> return v
  Right (SingleValue v) -> Left $ pn ++ " is not a multi parameter."

-- | Get all values for a specific parameter.
getParameterValues :: ParseResult -> ParameterName -> Either String (Seq ParameterValue)
getParameterValues ps pm =
  case parameter of
    Just (Parameter _ pvs) -> Right pvs
    Nothing -> Left $ "No values for parameter " ++ pm
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
  Just (Parameter _ vs) -> length vs
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
  Just (Parameter _ vs) -> case Seq.lookup 0 vs of
    Just (SingleValue v) -> True
    _ -> False
  _ -> False
  where
    param = getParameter pr pn

-- | Checks if the parameter is a multi parameter
isMulti :: ParseResult -> ParameterName -> Bool
isMulti pr pn = case param of
  Just (Parameter _ vs) -> case Seq.lookup 0 vs of
    Just (MultiValue v) -> True
    _ -> False
  _ -> False
  where
    param = getParameter pr pn