module CoderunnerGenerator.CmdArgs where

import Options.Applicative

-- * Arguments Type

data Args = Args
  { templateFile :: String,
    amount :: Maybe Int,
    debugOutput :: Bool,
    maxConfigurations :: Bool
  }
  deriving (Show)

-- * Parser Info

parserInfo :: ParserInfo Args
parserInfo = info parser briefDesc

-- * Parsers

parser :: Parser Args
parser =
  Args
    <$> templateFileParser
    <*> optional amountParser
    <*> debugParser
    <*> maxParser

templateFileParser :: Parser String
templateFileParser =
  strArgument $
    metavar "templateFile"
      <> action "file"

amountParser :: Parser Int
amountParser =
  option auto $
    long "amount"
      <> short 'a'

debugParser :: Parser Bool
debugParser =
  switch $
    long "debug"
    <> short 'd'

maxParser :: Parser Bool
maxParser =
  switch $
    long "max"
    <> short 'm'


-- * Execution function

executeParser :: IO Args
executeParser = execParser parserInfo