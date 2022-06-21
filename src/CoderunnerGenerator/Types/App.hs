module CoderunnerGenerator.Types.App (App) where

import CoderunnerGenerator.Types.Globals (Globals)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)

type App r u = ReaderT (Globals r u) (ExceptT String IO)