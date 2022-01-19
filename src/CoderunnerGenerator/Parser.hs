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
import Text.Parsec

-- * Main Parser

-- | The main parser for a template file
coderunnerParser :: Parsec String () Template
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
parameterSectionParser :: Parsec String () ParameterSection
parameterSectionParser = do
  headline <- string "Parameter:"
  linebreak
  ParameterSection placeholder <$> parameterBodyParser

parameterBodyParser :: Parsec String () ParameterBody
parameterBodyParser = do
  statement1 <- parameterStatementParser
  statements <- many (try (do linebreak; parameterStatementParser))
  return $ ParameterBody placeholder $ statement1 : statements

parameterStatementParser :: Parsec String () ParameterStatement
parameterStatementParser =
  try enumerationParser
    <|> try blueprintUsageParser
    <|> try generationParser
    <|> blueprintParser

enumerationParser :: Parsec String () ParameterStatement
enumerationParser = do
  enumPart1 <- enumerationPartParser
  requiresParser
  enumPart2 <- optionMaybe enumerationPartParser
  return $
    EnumerationStatement $
      Enumeration
        placeholder
        enumPart1
        enumPart2

requiresParser :: Parsec String () ()
requiresParser = do
  many1 (oneOf " \t")
  requires <- string "REQUIRES"
  many1 (oneOf " \t")
  return ()

enumerationPartParser :: Parsec String () EnumerationPart
enumerationPartParser = do
  identifier <- identifierParser
  char '('
  valueList <- valueListParser
  char ')'
  return $
    EnumerationPart
      identifier
      valueList

valueListParser :: Parsec String () [String]
valueListParser =
  try valuesParser <|> valuesWithCommaParser

valuesParser :: Parsec String () [String]
valuesParser = do
  sepBy1 valueParser (char ',')

valueParser :: Parsec String () String
valueParser = do
  many $ char ' '
  value <- many1 valueCharacterParser
  many $ char ' '
  return value

valuesWithCommaParser :: Parsec String () [String]
valuesWithCommaParser = do
  sepBy1 valueWithCommaParser (char ';')

valueWithCommaParser :: Parsec String () String
valueWithCommaParser = do
  optionalWhitespace
  char '{'
  value <- many1 anyParser
  char '}'
  optionalWhitespace
  return value

generationParser :: Parsec String () ParameterStatement
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

blueprintParser :: Parsec String () ParameterStatement
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

firstPropertyParser :: Parsec String () Property
firstPropertyParser = do
  char '@'
  many1 upper

furtherPropertyParser :: Parsec String () Property
furtherPropertyParser = do
  char ','
  optionalWhitespace
  property <- firstPropertyParser
  optionalWhitespace
  return property

ellipseParser :: Parsec String () Bool
ellipseParser = do
  char ','
  optionalWhitespace
  string "..."
  optionalWhitespace
  return True

blueprintUsageParser :: Parsec String () ParameterStatement
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

parameterUsageParser :: Parsec String () ParameterUsage
parameterUsageParser =
  ParameterUsage
    placeholder
    <$> identifierParser
    <*> optionMaybe propertyPartParser

identifierParser :: Parsec String () Identifier
identifierParser = do
  dollar <- char '$'
  many1 upper

propertyPartParser :: Parsec String () PropertyPart
propertyPartParser = do
  string "->"
  propertyName <- many1 (char '_' <|> upper)
  functionCallPart <- optionMaybe functionCallPartParser
  return $
    PropertyPart propertyName functionCallPart

functionCallPartParser :: Parsec String () FunctionCallPart
functionCallPartParser = do
  string "("
  argument <- many valueCharacterParser
  string ")"
  return [argument]

-- ** Other Sections

taskSectionParser :: Parsec String () TaskSection
taskSectionParser = do
  many linebreak
  string "Aufgabenstellung:"
  optionalWhitespace
  linebreak
  body <- mixedParser anyHeadline
  linebreak
  return $ TaskSection placeholder body

solutionSectionParser :: Parsec String () SolutionSection
solutionSectionParser = do
  many linebreak
  string "Lösung:"
  optionalWhitespace
  linebreak
  body <- mixedParser anyHeadline
  linebreak
  return $ SolutionSection placeholder body

preAllocationSectionParser :: Parsec String () PreAllocationSection
preAllocationSectionParser = do
  many linebreak
  string "Vorbelegung:"
  optionalWhitespace
  linebreak
  body <- mixedParser anyHeadline
  linebreak
  return $ PreAllocationSection placeholder body

mixedParser :: Parsec String () a -> Parsec String () Mixed
mixedParser mixedEndParser = do
  choice
    [ mixedConstantFirstElements mixedEndParser,
      mixedParameterFirstElements mixedEndParser
    ]

mixedConstantFirstElements :: Parsec String () a -> Parsec String () Mixed
mixedConstantFirstElements mixedEndParser = do
  c1 <- constantParser mixedEndParser
  elements <-
    many
      ( do
          parameterUsage <- parameterUsageParser
          constant <- optionMaybe $ constantParser mixedEndParser
          case constant of
            Nothing -> return [ParameterPart parameterUsage]
            Just a -> return [ParameterPart parameterUsage, a]
      )
  return (c1 : concat elements)

mixedParameterFirstElements :: Parsec String () a -> Parsec String () Mixed
mixedParameterFirstElements bodyEndParser = do
  p1 <- parameterUsageParser
  elements <-
    many
      ( do
          constant <- constantParser bodyEndParser
          parameterUsage <- optionMaybe parameterUsageParser
          case parameterUsage of
            Nothing -> return [constant]
            Just a -> return [constant, ParameterPart a]
      )
  return (ParameterPart p1 : concat elements)

constantParser :: Parsec String () a -> Parsec String () MixedPart
constantParser endParser = do
  c <- manyTill anyChar $ lookAhead $ dollarOr endParser
  return $ ConstantPart c

dollarOr :: Parsec String () a -> Parsec String () [Char]
dollarOr end = do
  try (string "$")
    <|> try (do end; return "")

anyHeadline :: Parsec String () [Char]
anyHeadline = do
  linebreak
  try (string "Aufgabenstellung:")
    <|> try (string "Lösung:")
    <|> try (string "Vorbelegung:")
    <|> try (string "Tests:")
    <|> string "Parameter:"

testSectionParser :: Parsec String () TestSection
testSectionParser = do
  many linebreak
  string "Tests:"
  optionalWhitespace
  linebreak
  testBody <- testBodyParser
  eof
  return $ TestSection placeholder testBody

testBodyParser :: Parsec String () TestBody
testBodyParser = do
  many testCaseParser

testCaseParser :: Parsec String () TestCase
testCaseParser =
  TestCase placeholder
    <$> mixedParser testOutcomeParser
    <*> testOutcomeParser

testOutcomeParser :: Parsec String () TestOutcome
testOutcomeParser = do
  linebreak
  string "Expected Outcome:"
  optionalWhitespace
  outcome <- many1 valueCharacterParser -- hier fehlt noch ParameterUsage (siehe EBNF)
  optionalWhitespace
  optional $ many linebreak
  return $ ConstantOutcome outcome

-- ** Characters

valueCharacterParser :: Parsec String () Char
valueCharacterParser = do oneOf "!^°§%&/=?`´*+#'-.<>" <|> letter <|> digit

valueCharacterGenerationParser :: Parsec String () Char
valueCharacterGenerationParser = do char ' ' <|> valueCharacterParser

anyParser :: Parsec String () Char
anyParser = do noneOf "${}"

linebreak :: Parsec String () Char
linebreak = do try newline <|> crlf

optionalWhitespace :: Parsec String () String
optionalWhitespace = do many (oneOf " \t")
