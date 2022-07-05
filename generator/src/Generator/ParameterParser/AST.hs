{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator.ParameterParser.AST where

import Lens.Micro.TH

data Position = Position
  { _lineStart :: Int,
    _lineEnd :: Int,
    _colStart :: Int,
    _colEnd :: Int
  }
  deriving (Show)

placeholder :: Position -- TO BE REMOVED
placeholder = Position 0 0 0 0

-- | Represents the parameter body
data ParameterAST = ParameterAST
  { parameterASTPosition :: Position,
    parameterASTParameterStatements :: [ParameterStatement]
  }
  deriving (Show)

-- | Represents parameter statement
data ParameterStatement = ParameterStatement
  { parameterStatementPosition :: Position,
    parameterStatementMain :: ParameterPart,
    parameterStatementRequires :: Maybe ParameterPart
  }
  deriving (Show)

-- | Represents a parameter part
--
-- Consists of an identifier and a value list
data ParameterPart
  = SingleParameterPart
      { parameterPartIdentifier :: Identifier,
        parameterPartValues :: [ParameterValue]
      }
  | MultiParameterPart
      { parameterPartIdentifier :: Identifier,
        parameterPartValueRanges :: [[ParameterValue]]
      }
  deriving (Show)

-- | Represents a Parameter Value
--
-- Consists of a list of parameter parts
data ParameterValue
  = Regular [ParameterValuePart]
  | Tuple [[ParameterValuePart]]
  deriving (Show)

-- | Represents a component of a parameter value
--
-- Possible components are simple strings and usages of identifiers
data ParameterValuePart = Simple String | IdUsage Identifier | TupleSelect Identifier Int
  deriving (Show, Eq, Ord)

containsOtherParameters :: ParameterValuePart -> Bool
containsOtherParameters (IdUsage _) = True
containsOtherParameters (TupleSelect _ _) = True
containsOtherParameters _ = False

isTuplePart :: ParameterPart -> Bool
isTuplePart (SingleParameterPart _ ((Tuple _) : _)) = True
isTuplePart (MultiParameterPart _ (((Tuple _) : _) : _)) = True
isTuplePart _ = False

type Identifier = String

makeLenses ''Position

makeFields ''ParameterAST
makeFields ''ParameterStatement
makeFields ''ParameterPart