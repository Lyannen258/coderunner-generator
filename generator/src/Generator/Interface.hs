module CoderunnerGenerator.Interface (run) where

import qualified CoderunnerGenerator.CmdArgs as CmdArgs
import CoderunnerGenerator.Main (main)
import CoderunnerGenerator.Configuration (Configuration)
import CoderunnerGenerator.Globals (constructGlobals)
import CoderunnerGenerator.ToParseResult
import Control.Exception (SomeException, try)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)

-- | r is the return that the run function works with,
-- u is the user state that is fed to the generator function
-- without alteration (usually you want to save your ast in u)
type ParserFunction r u =
  String -> Either String (r, u)

-- | u is the user state that was returned from the parser function
type GeneratorFunction u =
  [Configuration] -> u -> Either String String

-- | The run function
run :: (Show r, ToParseResult r) => ParserFunction r s -> GeneratorFunction s -> IO ()
run parser generator = handleErrors $ do
  args <- CmdArgs.executeParser
  let g = constructGlobals parser generator args
  res <- runExceptT (runReaderT main g)
  case res of
    Left s -> putStrLn $ "Error: " ++ s
    Right _ -> return ()

handleErrors :: IO () -> IO ()
handleErrors action = do
  res <- resIO
  case res of
    Left e -> print $ show e
    _ -> return ()
  where
    resIO = try action :: IO (Either SomeException ())