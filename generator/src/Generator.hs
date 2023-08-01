module Generator (run, runCustomCmdArgs, module G) where

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Generator.App (App (unApp))
import Generator.CmdArgs (emptyParser)
import Generator.CmdArgs qualified as CmdArgs
import Generator.Configuration (Configuration)
import Generator.Globals (GeneratorFunction (..), ParserFunction (..), constructGlobals)
import Generator.Globals qualified as G (GeneratorFunction (..), ParserFunction (..))
import Generator.Main (main)
import Generator.ToParseResult
import Options.Applicative (Parser)

-- | The run function
runCustomCmdArgs :: (Show r, ToParseResult r) => ParserFunction r s -> GeneratorFunction s a -> Parser a -> IO ()
runCustomCmdArgs parser generator argParser = do
  args <- CmdArgs.executeParser argParser
  let g = constructGlobals parser generator args
  res <- runExceptT (runReaderT (unApp main) g)
  case res of
    Left s -> putStrLn $ "Error: " ++ s
    Right _ -> return ()

run :: (Show r, ToParseResult r) => ParserFunction r s -> GeneratorFunction s () -> IO ()
run parser generator = runCustomCmdArgs parser generator emptyParser