module CoderunnerGenerator.Helper where

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing s = Left s

fillToTwenty :: String -> String
fillToTwenty s =
  if length s < 20
    then fillToTwenty (s ++ " ")
    else s

singleton :: a -> [a]
singleton a = [a]

removeFirst :: (a, b, c) -> (b, c)
removeFirst (_, b, c) = (b, c)