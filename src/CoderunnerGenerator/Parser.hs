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
import qualified Text.Megaparsec.Char.Lexer as L

-- * Parser type

type Parser = Parsec Void String

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

coderunnerParser :: [String] -> Parser Template
coderunnerParser otherHeadlines =
  Template
    placeholder
    <$> parameterSectionParser
    <*> otherSectionsParser otherHeadlines

-- * Parameter Section

parameterSectionParser :: Parser ParameterSection
parameterSectionParser =
  ParameterSection
  placeholder
  <$> parameterBodyParser

parameterBodyParser :: Parser ParameterBody
parameterBodyParser =
  ParameterBody
    placeholder
    <$> many parameterStatementParser

parameterStatementParser :: Parser ParameterStatement
parameterStatementParser = do
  firstParameterPart <- parameterPartParser
  requiresParser
  secondParameterPart <- optional parameterPartParser
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
  str <- someTill printChar (try (lookAhead openOutput))
  return $ Simple str

idUsageValuePartParser :: Parser ParameterValuePart
idUsageValuePartParser = do
  openOutput
  identifier <- identifierParser
  closingOutput
  return $ IdUsage identifier

-- * Other Section

otherSectionsParser :: [String] -> Parser [Section]
otherSectionsParser headlines = 
  some (otherSectionParser headlines)

otherSectionParser :: [String] -> Parser Section
otherSectionParser headlines =
  Section placeholder
    <$> otherHeadlineParser headlines
    <*> some sectionBodyComponentParser

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
  try textComponentParser <|> outputComponentParser

textComponentParser :: Parser SectionBodyComponent
textComponentParser =
  TextComponent <$> someTill textComponentChar (try (lookAhead openOutput))

textComponentChar :: Parser Char
textComponentChar = do
  c <- printChar
  optional blockComment
  return c

outputComponentParser :: Parser SectionBodyComponent
outputComponentParser =
  OutputComponent <$> outputParser

-- * Output / Parameter Usage

outputParser :: Parser Output
outputParser =
  openOutput
    *> stringOutputParser <|> parameterUsageParser
    <* closingOutput

stringOutputParser :: Parser Output
stringOutputParser =
  TextConstant <$> (openQuotes *> some printChar <* closingQuotes)

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

{-
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

-}