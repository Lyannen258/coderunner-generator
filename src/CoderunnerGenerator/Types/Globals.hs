module CoderunnerGenerator.Types.Globals
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
import CoderunnerGenerator.Types.ParseResult
import CoderunnerGenerator.Types.Configuration

data Globals s = Globals
  { parser :: String -> Either String (ParseResult, s),
    generator :: [Configuration] -> s -> Either String String,
    args :: Args
  }

constructGlobals :: (String -> Either String (ParseResult, s)) -> ([Configuration] -> s -> Either String String) -> Args -> Globals s
constructGlobals = Globals

getParser :: Globals s -> (String -> Either String (ParseResult, s))
getParser = parser

getGenerator :: Globals s -> ([Configuration] -> s -> Either String String)
getGenerator = generator

getTemplateFilePath :: Globals s -> String
getTemplateFilePath = templateFile . args

getDebugOutputFlag :: Globals s -> Bool
getDebugOutputFlag = debugOutput . args

getAmount :: Globals s -> Maybe Int
getAmount = amount . args

getMaxConfigurations :: Globals s -> Bool
getMaxConfigurations = maxConfigurations . args