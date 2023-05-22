module Generator.ParameterParser.AST where

import Generator.Atoms

-- | Represents the parameter body
newtype ParameterAST = ParameterAST
  {statements :: [ParameterStatement]}
  deriving (Show)

-- | Represents parameter statement
data ParameterStatement = ParameterStatement
  { mainPart :: ParameterPart,
    reqPart :: Maybe ParameterPart
  }
  deriving (Show)

-- | Represents a parameter part
--
-- Consists of an identifier and a value list
data ParameterPart = ParameterPart
  { identifier :: ParameterName,
    range :: Range IncompleteAtomicValue
  }
  deriving (Show)