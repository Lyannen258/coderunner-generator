module CoderunnerGenerator.Interface (run) where

import qualified CoderunnerGenerator.CmdArgs as CmdArgs
import CoderunnerGenerator.Main (main)
import CoderunnerGenerator.Types.Configuration (Configuration)
import CoderunnerGenerator.Types.Globals (constructGlobals)
import CoderunnerGenerator.Types.ParseResult (ParseResult)
import Control.Exception (SomeException, try)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (runReaderT)

type ParserFunction s =
  (String -> Either String (ParseResult, s))

type GeneratorFunction s =
  ([Configuration] -> s -> Either String String)

run :: ParserFunction s -> GeneratorFunction s -> IO ()
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