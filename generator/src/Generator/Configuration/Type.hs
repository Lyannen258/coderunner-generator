{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Generator.Configuration.Type where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Generator.Atoms
import Generator.ParseResult.Type (ParseResult)

newtype ParseResultFinal x = ParseResultFinal
  {unParseResultFinal :: ReaderT ParseResult (StateT Configuration (Either String)) x}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Configuration,
      MonadError String,
      MonadReader ParseResult
    )

data Configuration = Configuration
  { singleParameters :: [Parameter SingleValue AtomicValue],
    singleTupleParameters :: [Parameter SingleTupleValue AtomicValue],
    multiParameters :: [Parameter MultiValue AtomicValue],
    multiTupleParameters :: [Parameter MultiTupleValue AtomicValue],
    randomNumbers :: [Int]
  }
  deriving (Show)

data Parameter v a = Parameter
  { name :: ParameterName,
    selectedValue :: v a,
    range :: RangeType v a
  }
  deriving (Show)