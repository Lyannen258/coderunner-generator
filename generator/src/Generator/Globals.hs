module Generator.Globals
  ( Globals,
    ParserFunction (..),
    GeneratorFunction (..),
    constructGlobals,
    getParser,
    getGenerator,
    getTemplateFilePath,
    getDebugOutputFlag,
    getAmount,
    getMaxConfigurations,
    getInteractive,
    getAdditional
  )
where

import Generator.CmdArgs (Args (amount, debugOutput, interactive, maxConfigurations, templateFile), additional)
import Generator.Configuration

data Globals r u a = Globals
  { parser :: ParserFunction r u,
    generator :: GeneratorFunction u a,
    args :: Args a
  }

-- | r is the return that the run function works with,
-- u is the user state that is fed to the generator function
-- without alteration (usually you want to save your ast in u)
newtype ParserFunction r u
  = PF (String -> Either String (r, u))

-- | u is the user state that was returned from the parser function
data GeneratorFunction u a
  = GF ([Configuration] -> u -> IO (Either String String))
  | GFCstm ([Configuration] -> Int -> u -> a -> IO (Either String String))

constructGlobals :: ParserFunction r u -> GeneratorFunction u a -> Args a -> Globals r u a
constructGlobals = Globals

getParser :: Globals r u a -> ParserFunction r u
getParser = parser

getGenerator :: Globals r u a -> GeneratorFunction u a
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

getAdditional :: Globals r u a -> a
getAdditional = additional . args