module Generator (run) where

import Control.Monad.Except (runExceptT, MonadError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Generator.App (App (unApp))
import qualified Generator.CmdArgs as CmdArgs
import Generator.Configuration (Configuration)
import Generator.Globals (constructGlobals)
import Generator.Main (main)
import Generator.ToParseResult
import Generator.CmdArgs (emptyParser)
import Options.Applicative (Parser)

-- | r is the return that the run function works with,
-- u is the user state that is fed to the generator function
-- without alteration (usually you want to save your ast in u)
type ParserFunction r u =
  String -> Either String (r, u)

-- | u is the user state that was returned from the parser function
type GeneratorFunction u =
  [Configuration] -> u -> IO (Either String String)

-- | The run function
runCustomCmdArgs :: (Show r, ToParseResult r) => ParserFunction r s -> GeneratorFunction s -> Parser a -> IO ()
runCustomCmdArgs parser generator argParser = do
  args <- CmdArgs.executeParser argParser
  let g = constructGlobals parser generator args
  res <- runExceptT (runReaderT (unApp main) g)
  case res of
    Left s -> putStrLn $ "Error: " ++ s
    Right _ -> return ()

run :: (Show r, ToParseResult r) => ParserFunction r s -> GeneratorFunction s -> IO ()
run parser generator = runCustomCmdArgs parser generator emptyParser