module CPPCoderunner.Parser (CPPCoderunner.Parser.parse) where

import CPPCoderunner.AbstractSyntaxTree
import qualified CPPCoderunner.AbstractSyntaxTree as PR
import CoderunnerGenerator.Helper (maybeToEither)
import CoderunnerGenerator.Types.ParseResult (ParseResult)
import qualified CoderunnerGenerator.Types.ParseResult as PR
import Control.Monad (foldM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Debug.Pretty.Simple (pTraceShowId)
import Lens.Micro ((^.))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

-- * Error Messages

valueMissing :: String -> String -> String -> String -> String
valueMissing firstId firstValue secondId secondValue =
  "Tried to add constraint to ParseResult but could not find one of the parameter values. "
    ++ "First Value: "
    ++ firstId
    ++ "->"
    ++ firstValue
    ++ "\n"
    ++ "Second Value: "
    ++ secondId
    ++ "->"
    ++ secondValue

valueAmountMismatch :: String -> String -> Int -> Int -> String
valueAmountMismatch fstId sndId fstAmount sndAmount =
  "Tried to add constraints between values of "
    ++ fstId
    ++ " and "
    ++ sndId
    ++ ", but amount of values does not match. "
    ++ fstId
    ++ " has "
    ++ show fstAmount
    ++ " and "
    ++ sndId
    ++ " has "
    ++ show sndAmount
    ++ "."

-- * Interface

parse :: String -> Either String (ParseResult, Template)
parse s = do
  tmpl <- parseTemplate s
  pr <- constructParseResult tmpl
  return (pr, tmpl)

constructParseResult :: Template -> Either String ParseResult
constructParseResult tem = foldM f PR.empty parameterStatements'
  where
    parameterStatements' :: [ParameterStatement]
    parameterStatements' = tem ^. parameterSection . parameterBody . parameterStatements

    f :: ParseResult -> ParameterStatement -> Either String ParseResult
    f pr ps =
      let psMain = ps ^. main
          psReq = ps ^. requires

          toPRValue :: ParameterValue -> PR.Value
          toPRValue (ParameterValue pvps) =
            if any isIdUsage pvps
              then PR.NeedsInput (map toPRValuePart pvps)
              else PR.Final (foldl' (\acc (Simple s) -> acc ++ s) "" pvps)

          isIDUsage :: ParameterValuePart -> Bool
          isIDUsage (IdUsage _) = True
          isIDUsage _ = False

          toPRValuePart :: ParameterValuePart -> PR.ValuePart
          toPRValuePart (Simple s) = PR.StringPart s
          toPRValuePart (IdUsage id) = PR.ParameterPart id
       in case psMain of
            SingleParameterPart mainId mainPVS ->
              case psReq of
                Nothing -> PR.addParameter pr $ PR.singleParam mainId (map toPRValue mainPVS)
                Just (SingleParameterPart reqId reqPVS) ->
                  if length mainPVS == length reqPVS
                    then do
                      pr' <- PR.addParameter pr $ PR.singleParam mainId (map toPRValue mainPVS)
                      pr'' <- PR.addParameter pr' $ PR.singleParam reqId (map toPRValue reqPVS)
                      let valuePairs = zip mainPVS reqPVS
                      foldM
                        ( \pr (m, r) ->
                            PR.addConstraint
                              pr
                              (mainId, (PR.singleValue . toPRValue) m)
                              (reqId, (PR.singleValue . toPRValue) r)
                        )
                        pr''
                        valuePairs
                    else Left ""
                Just (MultiParameterPart reqId reqPVSS) ->
                  if length mainPVS == length reqPVSS
                    then do
                      pr' <- PR.addParameter pr $ PR.singleParam mainId (map toPRValue mainPVS)
                      pr'' <- PR.addParameter pr' $ PR.multiParam reqId (map (map toPRValue) reqPVSS)
                      let pairs = zip mainPVS reqPVSS
                      foldM
                        ( \prP (m, r) ->
                            PR.addConstraint
                              prP
                              (mainId, (PR.singleValue . toPRValue) m)
                              (reqId, (PR.multiValue . map toPRValue) r)
                        )
                        pr''
                        pairs
                    else Left ""
            MultiParameterPart mainId mainPVSS ->
              case psReq of
                Nothing -> PR.addParameter pr $ PR.multiParam mainId (map (map toPRValue) mainPVSS)
                Just (SingleParameterPart reqId reqPVS) ->
                  if length mainPVSS == length reqPVS
                    then do
                      pr' <- PR.addParameter pr $ PR.multiParam mainId (map (map toPRValue) mainPVSS)
                      pr'' <- PR.addParameter pr' $ PR.singleParam reqId (map toPRValue reqPVS)
                      let valuePairs = zip mainPVSS reqPVS
                      foldM
                        ( \pr (m, r) ->
                            PR.addConstraint
                              pr
                              (mainId, (PR.multiValue . map toPRValue) m)
                              (reqId, (PR.singleValue . toPRValue) r)
                        )
                        pr''
                        valuePairs
                    else Left ""
                Just (MultiParameterPart reqId reqPVSS) ->
                  if length mainPVSS == length reqPVSS
                    then do
                      pr' <- PR.addParameter pr $ PR.multiParam mainId (map (map toPRValue) mainPVSS)
                      pr'' <- PR.addParameter pr' $ PR.multiParam reqId (map (map toPRValue) reqPVSS)
                      let pairs = zip mainPVSS reqPVSS
                      foldM
                        ( \pr (m, r) ->
                            PR.addConstraint
                              pr
                              (mainId, (PR.multiValue . map toPRValue) m)
                              (reqId, (PR.multiValue . map toPRValue) r)
                        )
                        pr''
                        pairs
                    else Left ""

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

headlines :: [String]
headlines = [parameter, task, solution, preAllocation, test, name]

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

coderunnerParser :: Parser Template
coderunnerParser =
  Template
    placeholder
    <$> simpleSectionParser name
    <*> parameterSectionParser
    <*> sectionParser task
    <*> sectionParser solution
    <*> sectionParser preAllocation
    <*> testSectionParser

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
  notFollowedBy $ headlineOneOfParser headlines
  firstParameterPart <- parameterPartParser
  secondParameterPart <- optional (requiresParser *> parameterPartParser)
  return $
    ParameterStatement placeholder firstParameterPart secondParameterPart

parameterPartParser :: Parser ParameterPart
parameterPartParser = do
  identifier <- identifierParser
  openParenth
  parameterPart <- valueListParser identifier
  closingParenth
  return parameterPart

valueListParser :: String -> Parser ParameterPart
valueListParser id =
  (MultiParameterPart id <$> multiValueListParser)
    <|> (SingleParameterPart id <$> singleValueListParser)

singleValueListParser :: Parser [ParameterValue]
singleValueListParser = sepBy valueParser comma

multiValueListParser :: Parser [[ParameterValue]]
multiValueListParser = sepBy1 valueRangeParser comma

valueRangeParser :: Parser [ParameterValue]
valueRangeParser =
  openSquare *> singleValueListParser <* closingSquare

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

-- * Other Sections

sectionParser :: String -> Parser Section
sectionParser headline = do
  Section placeholder
    <$> headlineParser headline
    <*> some (sectionBodyComponentParser $ headlineOneOfParser headlines)

headlineParser :: String -> Parser String
headlineParser headline =
  let headlineWithColon = headline ++ ":"
      headlineParser :: Parser String
      headlineParser = (try . hlexeme . string) headlineWithColon
   in do
        headline <- headlineParser
        eol
        return $ init headline -- Do not take the colon

headlineOneOfParser :: [String] -> Parser String
headlineOneOfParser headlines =
  let hParsers :: [Parser String]
      hParsers = map headlineParser headlines
   in choice hParsers

sectionBodyParser :: Parser a -> Parser [SectionBodyComponent]
sectionBodyParser textEnd =
  some $ sectionBodyComponentParser textEnd

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
  optional blockComment
  return c

textComponentEndParser :: Parser a -> Parser ()
textComponentEndParser additionalEndParser = do
  (() <$ additionalEndParser) <|> (() <$ openOutput) <|> blockComment <|> eof

outputComponentParser :: Parser SectionBodyComponent
outputComponentParser =
  OutputComponent <$> outputParser

testSectionParser :: Parser TestSection
testSectionParser = do
  headlineParser test
  testcases <- some testCaseParser
  return $ TestSection placeholder testcases

testCaseParser :: Parser TestCase
testCaseParser = do
  codeHeadlineParser
  code <- some $ sectionBodyComponentParser outcomeHeadlineParser
  outcomeHeadlineParser
  outcome <- some $ sectionBodyComponentParser $ outcomeHeadlineParser <|> headlineOneOfParser headlines
  return $ TestCase placeholder code outcome

codeHeadlineParser :: Parser String
codeHeadlineParser = headlineParser "Code"

outcomeHeadlineParser :: Parser String
outcomeHeadlineParser = headlineParser "Outcome"

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

openSquare :: Parser Char
openSquare = lexeme (char '[')

closingSquare :: Parser Char
closingSquare = lexeme (char ']')

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