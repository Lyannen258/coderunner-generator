module CoderunnerGenerator.Types.Globals
  ( constructGlobals,
    getParser,
    getGenerator,
    getTemplateFilePath,
    getDebugOutputFlag,
    getAmount,
  )
where

import CoderunnerGenerator.CmdArgs (Args (amount, debugOutput, templateFile))
import CoderunnerGenerator.Types.ParseResult
import CoderunnerGenerator.Types.Configuration

data Globals s = Globals
  { parser :: String -> (ParseResult, s),
    generator :: [Configuration] -> s -> [String],
    args :: Args
  }

constructGlobals :: (String -> (ParseResult, s)) -> ([Configuration] -> s -> [String]) -> Args -> Globals s
constructGlobals = Globals

getParser :: Globals s -> (String -> (ParseResult, s))
getParser = parser

getGenerator :: Globals s -> ([Configuration] -> s -> [String])
getGenerator = generator

getTemplateFilePath :: Globals s -> String
getTemplateFilePath = templateFile . args

getDebugOutputFlag :: Globals s -> Bool
getDebugOutputFlag = debugOutput . args

getAmount :: Globals s -> Maybe Int
getAmount = amount . args