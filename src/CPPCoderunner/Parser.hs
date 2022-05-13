module CPPCoderunner.Parser (CPPCoderunner.Parser.parse) where

import CPPCoderunner.AbstractSyntaxTree
import CoderunnerGenerator.Helper (maybeToEither)
import CoderunnerGenerator.Types.ParseResult (ParseResult, addMultiConstraint)
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

          mainPRValues :: [PR.Value]
          mainPRValues = map toPRValue (psMain ^. values)

          pr' :: ParseResult
          pr' = PR.addValues pr (psMain ^. identifier) mainPRValues

          pr'' :: Either String ParseResult
          pr'' = case psReq of
            Nothing -> Right pr'
            Just pp ->
              let reqPRValues :: [PR.Value]
                  reqPRValues = map toPRValue (pp ^. values)

                  prTemp :: Either String ParseResult
                  prTemp
                    | length mainPRValues == length reqPRValues =
                      return $ PR.addValues pr' (pp ^. identifier) reqPRValues
                    | length mainPRValues == 1 && not (null reqPRValues) =
                      return $ PR.addMultiValues pr' (pp ^. identifier) reqPRValues
                    | otherwise =
                      Left $ valueAmountMismatch (psMain ^. identifier) (pp ^. identifier) (length mainPRValues) (length reqPRValues)

                  addConstraints :: Either String ParseResult
                  addConstraints
                    | length mainPRValues == length reqPRValues =
                      foldl' f prTemp (zip mainPRValues reqPRValues)
                    | length mainPRValues == 1 && not (null reqPRValues) =
                      do
                        prTemp' <- prTemp
                        maybeToEither
                          (addMultiConstraint prTemp' (psMain ^. identifier, head mainPRValues) (pp ^. identifier, reqPRValues))
                          "Cannot add multi constraint"
                    | otherwise =
                      Left $ valueAmountMismatch (psMain ^. identifier) (pp ^. identifier) (length mainPRValues) (length reqPRValues)

                  f :: Either String ParseResult -> (PR.Value, PR.Value) -> Either String ParseResult
                  f prE v = do
                    pr <- prE
                    case PR.addConstraint
                      pr
                      (psMain ^. identifier, fst v)
                      (pp ^. identifier, snd v) of
                      Nothing -> Left $ valueMissing (psMain ^. identifier) (show . fst $ v) (pp ^. identifier) (show . snd $ v)
                      Just pr -> Right pr
               in addConstraints

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
       in pr''

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

headlines :: [String]
headlines = [parameter, task, solution, preAllocation, test]

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
    <$> parameterSectionParser
    <*> simpleSectionParser task
    <*> simpleSectionParser solution
    <*> simpleSectionParser preAllocation
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

simpleSectionParser :: String -> Parser Section
simpleSectionParser headline = do
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