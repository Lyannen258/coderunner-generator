{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module CoderunnerGenerator.Interface where

data ParserConfig = ParserConfig
  { sections :: [String]
    -- keywords :: [String]
  }

class Runnable a where
  run :: a -> (b -> String) -> IO ()

instance Runnable (String -> a) where
  run parser generator = do
    putStrLn "Not yet implemented"
    return ()

instance Runnable ParserConfig where
  run parser generator = do

    return ()