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

import Data.Tree (Tree (Node), drawTree)
import GHC.Show (Show)
import Text.Parsec

-- * Types

-- | Recursive data type for the abstract syntax tree
data AST = AST
  { -- | The node label
    label :: Label,
    -- | The node value
    --
    -- It is the corresponding string in the template code.
    -- Only leaf nodes have values.
    value :: String,
    -- | Child nodes of the node
    children :: [AST]
  }

instance Show AST where
  show ast = drawTree $ toDataTree ast

-- | All possible node labels for the AST
data Label
  = CoderunnerFile
  | ParameterSection
  | ParameterHeadline
  | ParameterBody
  | ParameterStatement
  | Requires
  | ParameterDefinition
  | ParameterInformation
  | Enumeration
  | Value
  | Generation
  | ArbitraryPart
  | Blueprint
  | Property
  | Ellipse
  | BlueprintUsage
  | ParameterUsage
  | Identifier
  | PropertyPart
  | FunctionCallPart
  | Argument
  | TaskSection
  | SolutionSection
  | PreAllocationSection
  | Body
  | TestSection
  | TestOutcome
  | TestBody
  | TestCode
  | TestCase
  | Constant
  deriving (Show, Eq)

-- | Convert an AST to the Data.Tree type
toDataTree :: AST -> Tree String
toDataTree (AST label value children) = Node (show label ++ " (\"" ++ value ++ "\")") (map toDataTree children)

-- * Parsers

-- | The main parser for a template file
coderunnerParser :: Parsec String () AST
coderunnerParser = do
  parameterSection <- parameterSectionParser
  taskSection <- taskSectionParser
  solutionSection <- solutionSectionParser
  preAllocationSection <- preAllocationSectionParser
  testSection <- testSectionParser
  return
    ( AST
        CoderunnerFile
        ""
        [ parameterSection,
          taskSection,
          solutionSection,
          preAllocationSection,
          testSection
        ]
    )

-- ** Parameter Section

-- | Parser for the parameter section of a template
parameterSectionParser :: Parsec String () AST
parameterSectionParser = do
  headline <- string "Parameter:"
  linebreak
  body <- parameterBodyParser
  return
    ( AST
        ParameterSection
        ""
        [ AST ParameterHeadline headline [],
          body
        ]
    )

parameterBodyParser :: Parsec String () AST
parameterBodyParser = do
  statement1 <- parameterStatementParser
  statements <- many (try (do linebreak; parameterStatementParser))
  return (AST ParameterBody "" $ statement1 : statements)

parameterStatementParser :: Parsec String () AST
parameterStatementParser = do
  definition <- parameterDefinitionParser
  astNodes <- option [] parameterStatementRequiresParser
  return (AST ParameterStatement "" (definition : astNodes))

parameterStatementRequiresParser :: Parsec String () [AST]
parameterStatementRequiresParser = do
  many1 (oneOf " \t")
  requires <- string "REQUIRES"
  many1 (oneOf " \t")
  definition <- parameterDefinitionParser
  let requiresNode = AST Requires requires []
  return
    [ AST Requires requires [],
      definition
    ]

parameterDefinitionParser :: Parsec String () AST
parameterDefinitionParser = do
  identifier <- identifierParser
  char '('
  information <- parameterInformationParser
  char ')'
  return
    ( AST
        ParameterDefinition
        ""
        [ identifier,
          information
        ]
    )

parameterInformationParser :: Parsec String () AST
parameterInformationParser = do
  information <-
    try enumerationParser
      <|> try blueprintUsageParser
      <|> try generationParser
      <|> blueprintParser -- Reihenfolge wichtig
  return (AST ParameterInformation "" [information])

enumerationParser :: Parsec String () AST
enumerationParser = do
  values <- try valuesParser <|> valuesWithCommaParser
  return (AST Enumeration "" values)

valuesParser :: Parsec String () [AST]
valuesParser = do
  sepBy1 valueParser (char ',')

valueParser :: Parsec String () AST
valueParser = do
  many $ char ' '
  value <- many1 valueCharacterParser
  many $ char ' '
  return $ AST Value value []

valuesWithCommaParser :: Parsec String () [AST]
valuesWithCommaParser = do
  sepBy1 valueWithCommaParser (char ';')

valueWithCommaParser :: Parsec String () AST
valueWithCommaParser = do
  optionalWhitespace
  char '{'
  value <- many1 anyParser
  char '}'
  optionalWhitespace
  return (AST Value value [])

generationParser :: Parsec String () AST
generationParser = do
  try $
    generatorParser valueCharacterGenerationParser
      <|> generatorWithCommaParser

generatorParser :: Parsec String () Char -> Parsec String () AST
generatorParser characterParser = do
  firstArbitraryPart <- many characterParser
  firstIdentifier <- identifierParser
  secondArbitraryPart <- many characterParser
  parts <- many $ generatorPartParser characterParser
  let list =
        [ AST ArbitraryPart firstArbitraryPart [],
          firstIdentifier,
          AST ArbitraryPart secondArbitraryPart []
        ]
          ++ concat parts
  return $ AST Generation "" list

generatorPartParser :: Parsec String () Char -> Parsec String () [AST]
generatorPartParser characterParser = do
  identifier <- identifierParser
  arbitraryPart <- many characterParser
  return
    [ identifier,
      AST ArbitraryPart arbitraryPart []
    ]

generatorWithCommaParser :: Parsec String () AST
generatorWithCommaParser = do
  char '{'
  generator <- generatorParser anyParser
  char '}'
  return generator

blueprintParser :: Parsec String () AST
blueprintParser = do
  optionalWhitespace
  property <- firstPropertyParser
  optionalWhitespace
  properties <- many $ try furtherPropertyParser
  ellipse <- option [] ellipseParser
  return $ AST Blueprint "" ([property] ++ properties ++ ellipse)

firstPropertyParser :: Parsec String () AST
firstPropertyParser = do
  char '@'
  propertyName <- many1 upper
  return $ AST Property propertyName []

furtherPropertyParser :: Parsec String () AST
furtherPropertyParser = do
  char ','
  optionalWhitespace
  property <- firstPropertyParser
  optionalWhitespace
  return property

ellipseParser :: Parsec String () [AST]
ellipseParser = do
  char ','
  optionalWhitespace
  string "..."
  optionalWhitespace
  return [AST Ellipse "..." []]

blueprintUsageParser :: Parsec String () AST
blueprintUsageParser = do
  optionalWhitespace
  identifier <- identifierParser
  char '('
  enumeration <- enumerationParser
  char ')'
  optionalWhitespace
  return $
    AST
      BlueprintUsage
      ""
      [ identifier,
        enumeration
      ]

-- ** Parameter Usage

parameterUsageParser :: Parsec String () AST
parameterUsageParser = do
  identifier <- identifierParser
  propertyPart <- optionMaybe propertyPartParser
  return $
    AST
      ParameterUsage
      ""
      ( case propertyPart of
          Nothing -> [identifier]
          Just x -> [identifier, x]
      )

identifierParser :: Parsec String () AST
identifierParser = do
  dollar <- char '$'
  identifier <- many1 upper
  return (AST Identifier (dollar : identifier) [])

propertyPartParser :: Parsec String () AST
propertyPartParser = do
  string "->"
  propertyName <- many1 (char '_' <|> upper)
  functionCallPart <- optionMaybe functionCallPartParser
  return $
    AST
      PropertyPart
      propertyName
      ( case functionCallPart of
          Nothing -> []
          Just x -> [x]
      )

functionCallPartParser :: Parsec String () AST
functionCallPartParser = do
  string "("
  argument <- many valueCharacterParser
  string ")"
  return $ AST FunctionCallPart "" ([AST Argument argument [] | not (null argument)])

-- ** Other Sections

taskSectionParser :: Parsec String () AST
taskSectionParser = do
  many linebreak
  string "Aufgabenstellung:"
  optionalWhitespace
  linebreak
  body <- bodyParser anyHeadline
  linebreak
  return $ AST TaskSection "" [body]

solutionSectionParser :: Parsec String () AST
solutionSectionParser = do
  many linebreak
  string "Lösung:"
  optionalWhitespace
  linebreak
  body <- bodyParser anyHeadline
  linebreak
  return $ AST SolutionSection "" [body]

preAllocationSectionParser :: Parsec String () AST
preAllocationSectionParser = do
  many linebreak
  string "Vorbelegung:"
  optionalWhitespace
  linebreak
  body <- bodyParser anyHeadline
  linebreak
  return $ AST PreAllocationSection "" [body]

bodyParser :: Parsec String () a -> Parsec String () AST
bodyParser bodyEndParser = do
  elements <-
    choice
      [ bodyConstantFirstElements bodyEndParser,
        bodyParameterFirstElements bodyEndParser
      ]
  return $ AST Body "" elements

bodyConstantFirstElements :: Parsec String () a -> Parsec String () [AST]
bodyConstantFirstElements bodyEndParser = do
  c1 <- constantParser bodyEndParser
  elements <-
    many
      ( do
          parameterUsage <- parameterUsageParser
          constant <- optionMaybe $ constantParser bodyEndParser
          case constant of
            Nothing -> return [parameterUsage]
            Just a -> return [parameterUsage, a]
      )
  return (c1 : concat elements)

bodyParameterFirstElements :: Parsec String () a -> Parsec String () [AST]
bodyParameterFirstElements bodyEndParser = do
  p1 <- parameterUsageParser
  elements <-
    many
      ( do
          constant <- constantParser bodyEndParser
          parameterUsage <- optionMaybe parameterUsageParser
          case parameterUsage of
            Nothing -> return [constant]
            Just a -> return [constant, a]
      )
  return (p1 : concat elements)

constantParser :: Parsec String () a -> Parsec String () AST
constantParser endParser = do
  c <- manyTill anyChar $ lookAhead $ dollarOr endParser
  return $ AST Constant c []

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

testSectionParser :: Parsec String () AST
testSectionParser = do
  many linebreak
  string "Tests:"
  optionalWhitespace
  linebreak
  testBody <- testBodyParser
  eof
  return $ AST TestSection "" [testBody]

testBodyParser :: Parsec String () AST
testBodyParser = do
  cases <- many testCaseParser
  return $ AST TestBody "" cases

testCaseParser :: Parsec String () AST
testCaseParser = do
  caseBody <- bodyParser testOutcomeParser
  outcome <- testOutcomeParser
  return $
    AST
      TestCase
      ""
      [ AST TestCode (value caseBody) (children caseBody),
        outcome
      ]

testOutcomeParser :: Parsec String () AST
testOutcomeParser = do
  linebreak
  string "Expected Outcome:"
  optionalWhitespace
  outcome <- many1 valueCharacterParser -- hier fehlt noch ParameterUsage (siehe EBNF)
  optionalWhitespace
  optional $ many linebreak
  return $ AST TestOutcome outcome []

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
