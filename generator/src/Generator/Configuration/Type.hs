{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Generator.Configuration.Type where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Generator.Atoms
import Generator.ParseResult.Type (ParseResult)

type ConfigListRaw = [ConfigRaw]

newtype ConfigRaw = ConfigRaw {config :: [(ParameterName, Int)]}

newtype ConfigurationM x = ConfigurationM
  {unConfigurationM :: ReaderT (ConfigRaw, ParseResult) (StateT Configuration (Either String)) x}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Configuration,
      MonadError String,
      MonadReader (ConfigRaw, ParseResult)
    )

data Configuration = Configuration
  { singleParameters :: [SingleParameter],
    singleTupleParameters :: [SingleTupleParameter],
    multiParameters :: [MultiParameter],
    multiTupleParameters :: [MultiTupleParameter],
    randomNumbers :: [Int]
  }
  deriving (Show)

type SingleParameter = Parameter SingleValue AtomicValue

type MultiParameter = Parameter MultiValue AtomicValue

type SingleTupleParameter = Parameter SingleTupleValue AtomicValue

type MultiTupleParameter = Parameter MultiTupleValue AtomicValue

data Parameter v a = Parameter
  { name :: ParameterName,
    selectedValue :: v a,
    range :: RangeType v a
  }
  deriving (Show)