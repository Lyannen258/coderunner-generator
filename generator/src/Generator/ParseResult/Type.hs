module Generator.ParseResult.Type where

import Data.Sequence as Seq
import Generator.ParameterName

-- | Main data type. Used to pass information from a specialized generator to the main generator.
newtype ParseResult = ParseResult ParameterComposition -- maybe add Enumeration in future
  deriving (Show)

-- | Holds information about the composition of parameters, e.g.
-- - values for parameters
-- - constraints between parameters
data ParameterComposition = ParameterComposition
  { parameters :: [Parameter],
    constraints :: [Constraint]
  }
  deriving (Show)

-- | Data type for a parameter. Contains the parameter name and a list of possible values.
data Parameter = Parameter ParameterName Range
  deriving (Show)

data Range
  = Single SingleRange
  | SingleTuple SingleTupleRange
  | Multi MultiRange
  | MultiTuple MultiTupleRange
  deriving (Show)

newtype SingleRange = SingleRange (Seq RegularValue)
  deriving (Show)

newtype SingleTupleRange = SingleTupleRange (Seq TupleValue)
  deriving (Show)

newtype MultiRange = MultiRange (Seq (Seq RegularValue))
  deriving (Show)

newtype MultiTupleRange = MultiTupleRange (Seq (Seq TupleValue))
  deriving (Show)

-- | Data that contains information about a regular value for a parameter.
data RegularValue
  = -- | Constructor for a fully determined value that does not contain any other parameter.
    Final String
  | -- | Constructor for a value that contains at least one other parameter. It can be fully determined only after the value for the contained parameter was selected.
    NeedsInput [ValuePart]
  deriving (Eq, Show)

-- | Represents a tuple
newtype TupleValue = Tuple (Seq RegularValue)
  deriving (Eq, Show)

-- | Data type that makes up the parts for a not fully determined 'Value'.
data ValuePart
  = -- | Constructor for a constant part, e.g. in @{{ type }}myfunc();@ this would be "myfunc();""
    StringPart String
  | -- | Constructor for a con-constant part (the usage of a parameter that will
    -- be replaced by the value once it is determined), e.g. in @{{type}}myfunc();@ this would be "type"
    ParameterPart ParameterName
  | -- | Constructor for a non-constant part, where the get-function was used on a tuple
    TupleSelect ParameterName Int
  deriving (Eq, Show)

-- | Data type for a constraint between 2 parameter values. The values are identified by their index.
-- First one requires the second one.
data Constraint = Constraint (ParameterName, Int) (ParameterName, Int)
  deriving (Show, Eq)