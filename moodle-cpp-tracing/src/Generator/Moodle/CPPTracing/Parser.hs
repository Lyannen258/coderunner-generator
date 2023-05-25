module Generator.Moodle.CPPTracing.Parser (Generator.Moodle.CPPTracing.Parser.parse) where

import Generator.Atoms (ParameterName (ParameterName))
import Generator.Moodle.CPPTracing.AbstractSyntaxTree
import Generator.ParameterParser as PRP
import Generator.ParameterParser.AST (ParameterAST)
import Generator.ParserUtils
import Lens.Micro ((^.))
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor (($>))

-- * Interface

parse :: String -> Either String (ParameterAST, Template)
parse s = do
  tmpl <- parseTemplate s
  return (tmpl.parameterSection, tmpl)

parseTemplate :: String -> Either String Template
parseTemplate template =
  let result = Text.Megaparsec.parse coderunnerParser "" template
   in case result of
        Left peb -> Left $ errorBundlePretty peb
        Right tem -> Right tem

-- * Section headlines

parameter :: String
parameter = "Parameter"

code :: String
code = "Code"

title :: String
title = "Title"

ttype :: String
ttype = "Type"

feedback :: String
feedback = "Feedback"

headlines :: [String]
headlines = [parameter, code, title, ttype, feedback]

-- * Main Parser

coderunnerParser :: Parser Template
coderunnerParser =
  Template
    <$> simpleSectionParser title
    <*> traceTypeParser
    <*> parameterSectionParser
    <*> sectionParser code
    <*> sectionParser feedback

-- * Parameter Section

parameterSectionParser :: Parser ParameterAST
parameterSectionParser =
  parameterHeadlineParser *> PRP.parser (headlineOneOfParser headlines)

-- * Trace type parser
traceTypeParser :: Parser TraceType
traceTypeParser = do
  _ <- headlineParser ttype
  x <- comp <|> out
  _ <- eol
  return x
  where
    comp = (try . hlexeme . string) "compile" $> Compile
    out = (try . hlexeme . string) "output" $> Output

-- * Other Sections

sectionParser :: String -> Parser Section
sectionParser h = do
  Section 
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

-- * Output / Parameter Usage

outputParser :: Parser Output
outputParser =
  openOutput
    *> (stringOutputParser <|> parameterUsageParser)
    <* closingOutput

stringOutputParser :: Parser Output
stringOutputParser =
  TextConstant <$> (openQuotes *> someTill printChar ((try . lookAhead) closingQuotes) <* closingQuotes)

parameterUsageParser :: Parser Output
parameterUsageParser = do
  i <- ParameterName <$> identifierParser
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
  return $ SimpleSection hl (concat b)

simpleSectionComponentParser :: Parser String
simpleSectionComponentParser = do
  notFollowedBy $ headlineOneOfParser headlines
  ((: []) <$> printChar) <|> eol