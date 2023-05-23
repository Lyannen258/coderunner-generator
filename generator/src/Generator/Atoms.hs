module Generator.Atoms
  ( ParameterName (..),
    Range (..),
    AtomicValue (..),
    IncompleteAtomicValue (..),
    ValuePart (..),
    makeSingleRange,
    makeSingleTupleRange,
    makeMultiRange,
    makeMultiTupleRange,
    SingleRange,
    SingleTupleRange,
    MultiRange,
    MultiTupleRange,
    RangeType (..),
    SingleValue (..),
    SingleTupleValue (..),
    MultiValue (..),
    MultiTupleValue (..),
    tupleLookup,
  )
where

import Data.Sequence as Seq

newtype ParameterName = ParameterName {name :: String}
  deriving (Eq)

instance Show ParameterName where
  show = name

data Range a
  = Single (SingleRange a)
  | SingleTuple (SingleTupleRange a)
  | Multi (MultiRange a)
  | MultiTuple (MultiTupleRange a)
  deriving (Show)

newtype RangeType v a = RangeType {range :: Seq (v a)}
  deriving (Show)

newtype SingleValue a = SV {value :: a}
  deriving (Show, Eq)

newtype SingleTupleValue a = STV {value :: Seq a}
  deriving (Show, Eq)

tupleLookup :: Seq a -> Int -> Either String a
tupleLookup s i = case Seq.lookup i s of
  Just v -> return v
  _ -> Left "Index out of bounds"

newtype MultiValue a = MV {value :: Seq a}
  deriving (Show, Eq)

newtype MultiTupleValue a = MTV {value :: Seq (Seq a)}
  deriving (Show, Eq)

type SingleRange = RangeType SingleValue

makeSingleRange :: [a] -> Range a
makeSingleRange = Single . RangeType . Seq.fromList . map SV

type SingleTupleRange = RangeType SingleTupleValue

makeSingleTupleRange :: [[a]] -> Range a
makeSingleTupleRange = SingleTuple . RangeType . Seq.fromList . map (STV . Seq.fromList)

type MultiRange = RangeType MultiValue

makeMultiRange :: [[a]] -> Range a
makeMultiRange = Multi . RangeType . Seq.fromList . map (MV . Seq.fromList)

type MultiTupleRange = RangeType MultiTupleValue

makeMultiTupleRange :: [[[a]]] -> Range a
makeMultiTupleRange = MultiTuple . RangeType . Seq.fromList . map (MTV . Seq.fromList . map Seq.fromList)

newtype AtomicValue = AtomicValue {value :: String}
  deriving (Eq, Show)

-- | Data type that contains information about a possibly incomplete regular value for a parameter.
newtype IncompleteAtomicValue = IncompleteAtomicValue {parts :: [ValuePart]}
  deriving (Eq, Show)

-- | Data type that makes up the parts for a not fully determined 'Value'.
data ValuePart
  = -- | Constructor for a constant part, e.g. in @{{ type }}myfunc();@ this would be "myfunc();""
    StringPart String
  | -- | Constructor for a con-constant part (the usage of a parameter that will
    -- be replaced by the value once it is determined), e.g. in @{{type}}myfunc();@ this would be "type"
    IdUsage ParameterName
  | -- | Constructor for a non-constant part, where the get-function was used on a tuple
    TupleSelect ParameterName Int
  deriving (Eq, Show)