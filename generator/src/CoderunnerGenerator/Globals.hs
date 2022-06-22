module CoderunnerGenerator.Globals
  ( Globals,
    constructGlobals,
    getParser,
    getGenerator,
    getTemplateFilePath,
    getDebugOutputFlag,
    getAmount,
    getMaxConfigurations
  )
where

import CoderunnerGenerator.CmdArgs (Args (amount, debugOutput, templateFile, maxConfigurations))
import CoderunnerGenerator.Configuration

data Globals r u = Globals
  { parser :: String -> Either String (r, u),
    generator :: [Configuration] -> u -> Either String String,
    args :: Args
  }

constructGlobals :: (String -> Either String (r, u)) -> ([Configuration] -> u -> Either String String) -> Args -> Globals r u
constructGlobals = Globals

getParser :: Globals r u -> (String -> Either String (r, u))
getParser = parser

getGenerator :: Globals r u -> ([Configuration] -> u -> Either String String)
getGenerator = generator

getTemplateFilePath :: Globals r u -> String
getTemplateFilePath = templateFile . args

getDebugOutputFlag :: Globals r u -> Bool
getDebugOutputFlag = debugOutput . args

getAmount :: Globals r u -> Maybe Int
getAmount = amount . args

getMaxConfigurations :: Globals r u -> Bool
getMaxConfigurations = maxConfigurations . args