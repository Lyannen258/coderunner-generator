module Generator.App (App) where

import Generator.Globals (Globals)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)

type App r u = ReaderT (Globals r u) (ExceptT String IO)