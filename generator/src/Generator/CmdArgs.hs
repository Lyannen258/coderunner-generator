module Generator.CmdArgs where

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
parserInfo = info (parser <**> helper) fullDesc

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
      <> help "Path to the template file"

amountParser :: Parser Int
amountParser =
  option auto $
    long "amount"
      <> short 'a'
      <> metavar "AMOUNT"
      <> help "Specify the amount of variants to generate"

debugParser :: Parser Bool
debugParser =
  switch $
    long "debug"
      <> short 'd'
      <> help "Output debug information"

maxParser :: Parser Bool
maxParser =
  switch $
    long "max"
      <> short 'm'
      <> help "Return the maximum amount of variants possible with the given template file"

-- * Execution function

executeParser :: IO Args
executeParser = execParser parserInfo