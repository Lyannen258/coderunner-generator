module CoderunnerGenerator.Types.App where

import CoderunnerGenerator.Types.Globals (Globals)
import Control.Monad.Trans.Reader (ReaderT)

type App s = ReaderT (Globals s) IO