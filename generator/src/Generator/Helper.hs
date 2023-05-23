module Generator.Helper where

import Control.Monad.Except
import Generator.ParseResult.Type (ParseResultBuilder)

maybeToError :: MonadError String m => Maybe a -> String -> m a
maybeToError (Just a) _ = return a
maybeToError Nothing s = throwError s

maybeToParseResult :: Maybe a -> String -> ParseResultBuilder a
maybeToParseResult (Just a) _ = return a
maybeToParseResult Nothing s = throwError s

fillToTwenty :: String -> String
fillToTwenty s =
  if length s < 20
    then fillToTwenty (s ++ " ")
    else s

singleton :: a -> [a]
singleton a = [a]

removeFirst :: (a, b, c) -> (b, c)
removeFirst (_, b, c) = (b, c)

printLn :: MonadIO m => String -> m ()
printLn = liftIO . putStrLn