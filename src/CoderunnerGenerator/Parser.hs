-- |
-- Module      : CoderunnerGenerator.Parser
-- Description : Contains parsers to analyze a template file and form an abstract syntax tree
--
-- Contains parsers to analyze a template file and form an abstract syntax tree
--
-- The main parser is @coderunnerParser@. It is made up of section specific parsers, which
-- in turn are constructed by using even more granular parsers.
--
-- The parsers are loosely correlated with the rules in /grammar.ebnf/.
module CoderunnerGenerator.Parser where

import CoderunnerGenerator.Types.AbstractSyntaxTree
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

-- * Parser type

type Parser = ParsecT Void String (Reader [String])

-- * Lexemes

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

whitespace :: Parser ()
whitespace = L.space space1 empty blockComment

hwhitespace :: Parser ()
hwhitespace = L.space hspace1 empty blockComment

lexeme :: Parser s -> Parser s
lexeme = L.lexeme whitespace

hlexeme :: Parser s -> Parser s
hlexeme = L.lexeme hwhitespace

-- * Main Parser

coderunnerParser :: Parser Template
coderunnerParser =
  Template
    placeholder
    <$> parameterSectionParser
    <*> otherSectionsParser

-- * Parameter Section

parameterSectionParser :: Parser ParameterSection
parameterSectionParser =
  ParameterSection
    placeholder
    <$> (parameterHeadlineParser *> parameterBodyParser)

parameterBodyParser :: Parser ParameterBody
parameterBodyParser =
  ParameterBody
    placeholder
    <$> many parameterStatementParser

parameterStatementParser :: Parser ParameterStatement
parameterStatementParser = do
  headlines <- lift ask
  notFollowedBy $ otherHeadlineParser headlines
  firstParameterPart <- parameterPartParser
  secondParameterPart <- optional (requiresParser *> parameterPartParser)
  return $
    ParameterStatement placeholder firstParameterPart secondParameterPart

parameterPartParser :: Parser ParameterPart
parameterPartParser = do
  identifier <- identifierParser
  openParenth
  valueList <- valueListParser
  closingParenth
  return $
    ParameterPart identifier valueList

valueListParser :: Parser [ParameterValue]
valueListParser = sepBy valueParser comma

valueParser :: Parser ParameterValue
valueParser = do
  openQuotes
  valueParts <- some valuePartParser
  closingQuotes
  return $ ParameterValue valueParts

valuePartParser :: Parser ParameterValuePart
valuePartParser =
  try simpleValuePartParser <|> idUsageValuePartParser

simpleValuePartParser :: Parser ParameterValuePart
simpleValuePartParser = do
  notFollowedBy stringEndLookAhead
  firstChar <- printChar
  rest <- manyTill printChar stringEndLookAhead
  return $ Simple (firstChar : rest)

stringEndLookAhead :: Parser ()
stringEndLookAhead = do
  x <- (try . lookAhead) (fmap (: []) closingQuotes <|> openOutput)
  return ()

idUsageValuePartParser :: Parser ParameterValuePart
idUsageValuePartParser = do
  openOutput
  identifier <- identifierParser
  closingOutput
  return $ IdUsage identifier

-- * Other Section

otherSectionsParser :: Parser [Section]
otherSectionsParser =
  some $ dbg "otherSection" otherSectionParser

otherSectionParser :: Parser Section
otherSectionParser = do
  headlines <- lift ask
  Section placeholder
    <$> otherHeadlineParser headlines
    <*> some (dbg "bodyComponent" sectionBodyComponentParser)

otherHeadlineParser :: [String] -> Parser String
otherHeadlineParser headlines =
  let headlinesWithColon = map (++ ":") headlines
      headlineParsers :: [Parser String]
      headlineParsers = map (try . hlexeme . string) headlinesWithColon
   in do
        headline <- choice headlineParsers
        eol
        return $ init headline -- Do not take the colon

sectionBodyComponentParser :: Parser SectionBodyComponent
sectionBodyComponentParser =
  try (dbg "textComponent" textComponentParser) <|> dbg "outputComponent" outputComponentParser

textComponentParser :: Parser SectionBodyComponent
textComponentParser = do
  listOfStrings <- someTill textComponentChar (try (lookAhead textComponentEndParser))
  let content = concat listOfStrings
  return $ TextComponent content

textComponentEndParser :: Parser ()
textComponentEndParser = do
  headlines <- lift ask
  (() <$ otherHeadlineParser headlines) <|> (() <$ openOutput) <|> blockComment <|> eof

textComponentChar :: Parser String
textComponentChar = do
  notFollowedBy textComponentEndParser
  c <- ((: []) <$> printChar) <|> eol
  optional blockComment
  return c

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
  id <- identifierParser
  callPart <- (optional . try) callPartParser
  return $ Parameter $ ParameterUsage placeholder id callPart

callPartParser :: Parser CallPart
callPartParser = do
  point
  callIdentifier <- identifierParser
  openParenth
  args <- argsParser
  closingParenth
  return $ CallPart callIdentifier args

argsParser :: Parser [String]
argsParser = sepBy argParser comma

-- * Lexemes

requiresParser :: Parser String
requiresParser = lexeme (string "requires")

identifierParser :: Parser String
identifierParser = lexeme (some letterChar)

parameterHeadlineParser :: Parser String
parameterHeadlineParser = hlexeme (string "Parameter:" <* eol)

openParenth :: Parser Char
openParenth = lexeme (char '(')

closingParenth :: Parser Char
closingParenth = lexeme (char ')')

-- | Do not consume whitespace, because string begins
openQuotes :: Parser Char
openQuotes = char '"'

closingQuotes :: Parser Char
closingQuotes = lexeme (char '"')

-- | Exact reverse to openQuotes: Consume whitespace
-- only on opening output, because after closing, the
-- string (parameter value) resumes and we want to conserve
-- whitespace
openOutput :: Parser String
openOutput = lexeme (string "{{")

closingOutput :: Parser String
closingOutput = string "}}"

point :: Parser Char
point = (lexeme . char) '.'

comma :: Parser Char
comma = (lexeme . char) ','

argChar :: Parser Char
argChar = alphaNumChar

argParser :: Parser String
argParser = (lexeme . some) argChar