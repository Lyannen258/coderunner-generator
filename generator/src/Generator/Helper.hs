module Generator.Helper where
import Control.Monad.IO.Class (MonadIO(liftIO))
import Generator.ParseResult.Type (ParseResultBuilder)
import Control.Monad.Except

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing s = Left s

maybeToParseResult :: Maybe a -> String -> ParseResultBuilder a
maybeToParseResult (Just a) _ = return a
maybeToParseResult (Nothing) s = throwError s

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