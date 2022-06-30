module Generator.Moodle.CPPProgram.Parser (Generator.Moodle.CPPProgram.Parser.parse) where

import Generator.Moodle.CPPProgram.AbstractSyntaxTree
import Generator.ParameterParser as PRP
import Generator.ParserUtils
import qualified Generator.ParameterParser.AST as PAST
import Lens.Micro ((^.), (^?), _Just)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (fromMaybe)

-- * Interface

parse :: String -> Either String (PAST.ParameterAST, Template)
parse s = do
  tmpl <- parseTemplate s
  return (fromMaybe (PAST.ParameterAST PAST.placeholder []) (tmpl ^? parameterSection . _Just . parameterBody), tmpl)

parseTemplate :: String -> Either String Template
parseTemplate template =
  let result = Text.Megaparsec.parse coderunnerParser "" template
   in case result of
        Left peb -> Left $ errorBundlePretty peb
        Right tem -> Right tem

-- * Section headlines

parameter :: String
parameter = "Parameter"

task :: String
task = "Task"

solution :: String
solution = "Solution"

preAllocation :: String
preAllocation = "PreAllocation"

test :: String
test = "Tests"

name :: String
name = "Name"

author :: String
author = "Author"

headlines :: [String]
headlines = [parameter, task, solution, preAllocation, test, name, author]

-- * Main Parser

coderunnerParser :: Parser Template
coderunnerParser =
  Template
    placeholder
    <$> simpleSectionParser name
    <*> simpleSectionParser author
    <*> optional parameterSectionParser
    <*> sectionParser task
    <*> optional (sectionParser solution)
    <*> optional (sectionParser preAllocation)
    <*> optional testSectionParser

-- * Parameter Section

parameterSectionParser :: Parser ParameterSection
parameterSectionParser =
  ParameterSection
    placeholder
    <$> (parameterHeadlineParser *> PRP.parser (headlineOneOfParser headlines))

-- * Other Sections

sectionParser :: String -> Parser Section
sectionParser h = do
  Section placeholder
    <$> headlineParser h
    <*> some (sectionBodyComponentParser $ headlineOneOfParser headlines)

headlineParser :: String -> Parser String
headlineParser h =
  let headlineWithColon = h ++ ":"

      headlineParser' :: Parser String
      headlineParser' = (try . hlexeme . string) headlineWithColon
   in do
        _ <- headlineParser'
        _ <- eol
        return h -- Do not take the colon

headlineOneOfParser :: [String] -> Parser String
headlineOneOfParser hs =
  let hParsers :: [Parser String]
      hParsers = map headlineParser hs
   in choice hParsers

sectionBodyComponentParser :: Parser a -> Parser SectionBodyComponent
sectionBodyComponentParser textEnd =
  try (textComponentParser textEnd) <|> outputComponentParser

textComponentParser :: Parser a -> Parser SectionBodyComponent
textComponentParser textEnd = do
  listOfStrings <- some (textComponentChar textEnd)
  let content = concat listOfStrings
  return $ TextComponent content

textComponentChar :: Parser a -> Parser String
textComponentChar textEnd = do
  notFollowedBy (textComponentEndParser textEnd)
  c <- ((: []) <$> printChar) <|> eol
  _ <- optional blockComment
  return c

textComponentEndParser :: Parser a -> Parser ()
textComponentEndParser additionalEndParser = do
  (() <$ additionalEndParser) <|> (() <$ openOutput) <|> blockComment <|> eof

outputComponentParser :: Parser SectionBodyComponent
outputComponentParser =
  OutputComponent <$> outputParser

testSectionParser :: Parser TestSection
testSectionParser = do
  _ <- headlineParser test
  testcases <- some testCaseParser
  return $ TestSection placeholder testcases

testCaseParser :: Parser TestCase
testCaseParser = do
  _ <- inputHeadlineParser
  i <- some $ sectionBodyComponentParser outcomeHeadlineParser
  _ <- outcomeHeadlineParser
  o <- some $ sectionBodyComponentParser $ inputHeadlineParser <|> headlineOneOfParser headlines
  return $ TestCase placeholder i o

inputHeadlineParser :: Parser String
inputHeadlineParser = headlineParser "Input"

outcomeHeadlineParser :: Parser String
outcomeHeadlineParser = headlineParser "Outcome"

-- * Output / Parameter Usage

outputParser :: Parser Output
outputParser = do
  sp <- getSourcePos
  _ <- openOutput
  output <- stringOutputParser <|> parameterUsageParser
  _ <- closingOutput
  return $ Output (Position (unPos . sourceLine $ sp) (unPos . sourceColumn $ sp)) output

stringOutputParser :: Parser OutputInner
stringOutputParser =
  TextConstant <$> (openQuotes *> someTill printChar ((try . lookAhead) closingQuotes) <* closingQuotes)

parameterUsageParser :: Parser OutputInner
parameterUsageParser = do
  i <- identifierParser
  cp <- (optional . try) callPartParser
  return $ Parameter $ ParameterUsage i cp

callPartParser :: Parser CallPart
callPartParser = do
  _ <- point
  callIdentifier <- identifierParser
  _ <- openParenth
  args <- argsParser
  _ <- closingParenth
  return $ CallPart callIdentifier args

argsParser :: Parser [String]
argsParser = sepBy argParser comma

-- * Simple Section

simpleSectionParser :: String -> Parser SimpleSection
simpleSectionParser hl = do
  _ <- headlineParser hl
  b <- some simpleSectionComponentParser
  return $ SimpleSection placeholder hl (concat b)

simpleSectionComponentParser :: Parser String
simpleSectionComponentParser = do
  notFollowedBy $ headlineOneOfParser headlines
  ((: []) <$> printChar) <|> eol