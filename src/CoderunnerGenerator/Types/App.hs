module CoderunnerGenerator.Types.App (App) where

import CoderunnerGenerator.Types.Globals (Globals)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Except (ExceptT)

type App s = ReaderT (Globals s) (ExceptT String IO)