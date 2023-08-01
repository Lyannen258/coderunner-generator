module Generator.Globals
  ( Globals,
    constructGlobals,
    getParser,
    getGenerator,
    getTemplateFilePath,
    getDebugOutputFlag,
    getAmount,
    getMaxConfigurations,
    getInteractive,
  )
where

import Generator.CmdArgs (Args (amount, debugOutput, interactive, maxConfigurations, templateFile))
import Generator.Configuration

data Globals r u a = Globals
  { parser :: String -> Either String (r, u),
    generator :: [Configuration] -> u -> IO (Either String String),
    args :: Args a
  }

constructGlobals :: (String -> Either String (r, u)) -> ([Configuration] -> u -> IO (Either String String)) -> Args a -> Globals r u a
constructGlobals = Globals

getParser :: Globals r u a -> (String -> Either String (r, u))
getParser = parser

getGenerator :: Globals r u a -> ([Configuration] -> u -> IO (Either String String))
getGenerator = generator

getTemplateFilePath :: Globals r u a -> String
getTemplateFilePath = templateFile . args

getDebugOutputFlag :: Globals r u a -> Bool
getDebugOutputFlag = debugOutput . args

getAmount :: Globals r u a -> Maybe Int
getAmount = amount . args

getMaxConfigurations :: Globals r u a -> Bool
getMaxConfigurations = maxConfigurations . args

getInteractive :: Globals r u a -> Bool
getInteractive = interactive . args