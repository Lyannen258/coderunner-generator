{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Generator.App (App(..)) where

import Control.Monad.Except
import Control.Monad.Reader
import Generator.Globals (Globals)

newtype App r u a = App
  { unApp ::
      ReaderT
        (Globals r u)
        (ExceptT String IO)
        a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (Globals r u), MonadError String)