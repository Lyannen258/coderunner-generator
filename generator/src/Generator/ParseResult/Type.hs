{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Generator.ParseResult.Type where

import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader
import Control.Monad.State
import Data.Sequence as Seq
import Generator.Atoms

newtype ParseResultBuilder x = ParseResultBuilder
  { unParseResultM :: ExceptT String (State ParseResult) x
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState ParseResult,
      MonadError String
    )



-- | Main data type. Used to pass information from a specialized generator to the main generator.
newtype ParseResult = ParseResult {comp :: ParameterComposition} -- maybe add Enumeration in future
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
data Parameter = Parameter
  { name :: ParameterName,
    range :: Range IncompleteAtomicValue
  }
  deriving (Show)

-- | Data type for a constraint between 2 parameter values. The values are identified by their index.
-- First one requires the second one.
data Constraint = Constraint (ParameterName, Int) (ParameterName, Int)
  deriving (Show, Eq)