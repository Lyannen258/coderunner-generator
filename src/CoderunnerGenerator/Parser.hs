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
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- * Parser type

type Parser = Parsec Void String

-- * Main Parser

-- | The main parser for a template file
coderunnerParser :: Parser Template
coderunnerParser =
  Template
    placeholder
    <$> parameterSectionParser
    <*> taskSectionParser
    <*> solutionSectionParser
    <*> preAllocationSectionParser
    <*> testSectionParser

-- ** Parameter Section

-- | Parser for the parameter section of a template
parameterSectionParser :: Parser ParameterSection
parameterSectionParser = do
  headline <- string "Parameter:"
  linebreak
  ParameterSection placeholder <$> parameterBodyParser

parameterBodyParser :: Parser ParameterBody
parameterBodyParser = do
  statement1 <- parameterStatementParser
  statements <- many (try (do linebreak; parameterStatementParser))
  return $ ParameterBody placeholder $ statement1 : statements

parameterStatementParser :: Parser ParameterStatement
parameterStatementParser =
  try enumerationParser
    <|> try blueprintUsageParser
    <|> try generationParser
    <|> blueprintParser

enumerationParser :: Parser ParameterStatement
enumerationParser = do
  enumPart1 <- enumerationPartParser
  optional requiresParser
  enumPart2 <- optional enumerationPartParser
  return $
    EnumerationStatement $
      Enumeration
        placeholder
        enumPart1
        enumPart2

requiresParser :: Parser ()
requiresParser = do
  some (oneOf " \t")
  requires <- string "REQUIRES"
  some (oneOf " \t")
  return ()

enumerationPartParser :: Parser EnumerationPart
enumerationPartParser = do
  identifier <- identifierParser
  char '('
  valueList <- valueListParser
  char ')'
  return $
    EnumerationPart
      identifier
      valueList

valueListParser :: Parser [String]
valueListParser =
  try valuesParser <|> valuesWithCommaParser

valuesParser :: Parser [String]
valuesParser = do
  sepBy1 valueParser (char ',')

valueParser :: Parser String
valueParser = do
  many $ char ' '
  value <- some valueCharacterParser
  many $ char ' '
  return value

valuesWithCommaParser :: Parser [String]
valuesWithCommaParser = do
  sepBy1 valueWithCommaParser (char ';')

valueWithCommaParser :: Parser String
valueWithCommaParser = do
  optionalWhitespace
  char '{'
  value <- some anyParser
  char '}'
  optionalWhitespace
  return value

generationParser :: Parser ParameterStatement
generationParser = do
  identifier <- identifierParser
  string "({"
  mixed <- mixedParser (char '}')
  string "})"
  return $
    GenerationStatement $
      Generation
        placeholder
        identifier
        mixed

blueprintParser :: Parser ParameterStatement
blueprintParser = do
  identifier <- identifierParser
  char '('
  optionalWhitespace
  property <- firstPropertyParser
  optionalWhitespace
  properties <- many $ try furtherPropertyParser
  ellipse <- try ellipseParser
  char ')'
  return $
    BlueprintStatement $
      Blueprint
        placeholder
        identifier
        (property : properties)
        ellipse

firstPropertyParser :: Parser Property
firstPropertyParser = do
  char '@'
  some upperChar

furtherPropertyParser :: Parser Property
furtherPropertyParser = do
  char ','
  optionalWhitespace
  property <- firstPropertyParser
  optionalWhitespace
  return property

ellipseParser :: Parser Bool
ellipseParser = do
  char ','
  optionalWhitespace
  string "..."
  optionalWhitespace
  return True

blueprintUsageParser :: Parser ParameterStatement
blueprintUsageParser = do
  identifier <- identifierParser
  char '('
  optionalWhitespace
  blueprint <- identifierParser
  char '('
  valueList <- valueListParser
  char ')'
  optionalWhitespace
  char ')'
  return $
    BlueprintUsageStatement $
      BlueprintUsage
        placeholder
        identifier
        blueprint
        valueList

-- ** Parameter Usage

parameterUsageParser :: Parser ParameterUsage
parameterUsageParser =
  ParameterUsage
    placeholder
    <$> identifierParser
    <*> optional propertyPartParser

identifierParser :: Parser Identifier
identifierParser = do
  dollar <- char '$'
  some upperChar

propertyPartParser :: Parser PropertyPart
propertyPartParser = do
  string "->"
  propertyName <- some (char '_' <|> upperChar)
  functionCallPart <- optional functionCallPartParser
  return $
    PropertyPart propertyName functionCallPart

functionCallPartParser :: Parser FunctionCallPart
functionCallPartParser = do
  string "("
  argument <- many valueCharacterParser
  string ")"
  return [argument]

-- ** Other Sections

taskSectionParser :: Parser TaskSection
taskSectionParser = do
  many linebreak
  string "Aufgabenstellung:"
  optionalWhitespace
  linebreak
  body <- mixedParser anyHeadline
  linebreak
  return $ TaskSection placeholder body

solutionSectionParser :: Parser SolutionSection
solutionSectionParser = do
  many linebreak
  string "Lösung:"
  optionalWhitespace
  linebreak
  body <- mixedParser anyHeadline
  linebreak
  return $ SolutionSection placeholder body

preAllocationSectionParser :: Parser PreAllocationSection
preAllocationSectionParser = do
  many linebreak
  string "Vorbelegung:"
  optionalWhitespace
  linebreak
  body <- mixedParser anyHeadline
  linebreak
  return $ PreAllocationSection placeholder body

mixedParser :: Parser a -> Parser Mixed
mixedParser mixedEndParser = do
  choice
    [ mixedConstantFirstElements mixedEndParser,
      mixedParameterFirstElements mixedEndParser
    ]

mixedConstantFirstElements :: Parser a -> Parser Mixed
mixedConstantFirstElements mixedEndParser = do
  c1 <- constantParser mixedEndParser
  elements <-
    many
      ( do
          parameterUsage <- parameterUsageParser
          constant <- optional $ constantParser mixedEndParser
          case constant of
            Nothing -> return [ParameterPart parameterUsage]
            Just a -> return [ParameterPart parameterUsage, a]
      )
  return (c1 : concat elements)

mixedParameterFirstElements :: Parser a -> Parser Mixed
mixedParameterFirstElements bodyEndParser = do
  p1 <- parameterUsageParser
  elements <-
    many
      ( do
          constant <- constantParser bodyEndParser
          parameterUsage <- optional parameterUsageParser
          case parameterUsage of
            Nothing -> return [constant]
            Just a -> return [constant, ParameterPart a]
      )
  return (ParameterPart p1 : concat elements)

constantParser :: Parser a -> Parser MixedPart
constantParser endParser = do
  c <- manyTill (printChar <|> spaceChar) $ lookAhead $ dollarOr endParser
  return $ ConstantPart c

dollarOr :: Parser a -> Parser [Char]
dollarOr end = do
  try (string "$")
    <|> try (do end; return "")

anyHeadline :: Parser [Char]
anyHeadline = do
  linebreak
  try (string "Aufgabenstellung:")
    <|> try (string "Lösung:")
    <|> try (string "Vorbelegung:")
    <|> try (string "Tests:")
    <|> string "Parameter:"

testSectionParser :: Parser TestSection
testSectionParser = do
  many linebreak
  string "Tests:"
  optionalWhitespace
  linebreak
  testBody <- testBodyParser
  eof
  return $ TestSection placeholder testBody

testBodyParser :: Parser TestBody
testBodyParser = do
  many testCaseParser

testCaseParser :: Parser TestCase
testCaseParser =
  TestCase placeholder
    <$> mixedParser testOutcomeParser
    <*> testOutcomeParser

testOutcomeParser :: Parser TestOutcome
testOutcomeParser = do
  linebreak
  string "Expected Outcome:"
  optionalWhitespace
  outcome <- some valueCharacterParser -- hier fehlt noch ParameterUsage (siehe EBNF)
  optionalWhitespace
  optional $ many linebreak
  return $ ConstantOutcome outcome

-- ** Characters

valueCharacterParser :: Parser Char
valueCharacterParser = do oneOf "!^°§%&/=?`´*+#'-.<>" <|> letterChar <|> digitChar

valueCharacterGenerationParser :: Parser Char
valueCharacterGenerationParser = do char ' ' <|> valueCharacterParser

anyParser :: Parser Char
anyParser = do noneOf "${}"

linebreak :: Parser [Char]
linebreak = do eol

optionalWhitespace :: Parser String
optionalWhitespace = do many (oneOf " \t")
