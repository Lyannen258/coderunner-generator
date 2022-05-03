module CoderunnerGenerator.Interface where

import CoderunnerGenerator.Types.Globals
import CoderunnerGenerator.Main
import Control.Monad.Trans.Reader
import qualified CoderunnerGenerator.CmdArgs as CmdArgs

run :: (String -> (ParseResult, s)) -> ([Configuration] -> s -> [String]) -> IO ()
run parser generator = handleErrors $ do
  args <- CmdArgs.executeParser
  let g = constructGlobals parser generator args
  runReaderT main g