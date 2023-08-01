{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Generator.App (App (..)) where

import Control.Monad.Except
import Control.Monad.Reader
import Generator.Globals (Globals)

newtype App r u a b = App
  { unApp ::
      ReaderT
        (Globals r u a)
        (ExceptT String IO)
        b
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Globals r u a), MonadError String)