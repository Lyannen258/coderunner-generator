module CoderunnerGenerator.Types.ParseResult
  ( ParseResult,
    ParameterName,
    Value (Final, NeedsInput),
    ValuePart (StringPart, ParameterPart)
  )
where

import Data.Foldable (elem, find)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
data Parameter = Parameter ParameterName (Seq Value)
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
  deriving (Show)

-- | Add values for a parameter to a 'ParseResult'. Duplicate values
-- will not be added, if there are already values for the parameter.
addValues :: ParseResult -> ParameterName -> [Value] -> ParseResult
addValues pr@(ParseResult pc@(ParameterComposition ps cs)) pn vs =
  ParseResult $ pc {parameters = newParameters}
  where
    currentValues :: Seq Value
    currentValues = getParameterValues pr pn

    allValues :: Seq Value
    allValues = foldr addIfNotContained currentValues vs

    addIfNotContained :: Value -> Seq Value -> Seq Value
    addIfNotContained v vs
      | v `elem` vs = vs
      | otherwise = vs Seq.|> v

    newParameters :: [Parameter]
    newParameters = foldr d [] ps ++ [Parameter pn allValues]

    d :: Parameter -> [Parameter] -> [Parameter]
    d p@(Parameter name _) ps
      | name == pn = ps
      | otherwise = ps ++ [p]

-- | Get all values for a specific parameter.
getParameterValues :: ParseResult -> ParameterName -> Seq Value
getParameterValues ps pm =
  case parameter of
    Just (Parameter _ vs) -> vs
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

-- | An empty parse result
empty :: ParseResult
empty = ParseResult $ ParameterComposition [] []
