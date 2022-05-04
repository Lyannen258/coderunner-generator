module CoderunnerGenerator.Interface where

import CoderunnerGenerator.Types.Globals
import CoderunnerGenerator.Main
import Control.Monad.Trans.Reader
import qualified CoderunnerGenerator.CmdArgs as CmdArgs
import CoderunnerGenerator.Types.ParseResult ( ParseResult )
import CoderunnerGenerator.Types.Configuration ( Configuration )
import Control.Exception ( SomeException, try ) 

run :: (String -> Either String (ParseResult, s)) -> ([Configuration] -> s -> [String]) -> IO ()
run parser generator = handleErrors $ do
  args <- CmdArgs.executeParser
  let g = constructGlobals parser generator args
  runReaderT main g

handleErrors :: IO () -> IO ()
handleErrors action = do
  res <- resIO
  case res of
    Left e -> print $ show e
    _ -> return ()
  where
    resIO = try action :: IO (Either SomeException ())