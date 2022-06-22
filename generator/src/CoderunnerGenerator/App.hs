module CoderunnerGenerator.App (App) where

import CoderunnerGenerator.Globals (Globals)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)

type App r u = ReaderT (Globals r u) (ExceptT String IO)