module Generator.ParseResult.Parser (parser) where

import Generator.ParserUtils
import Generator.ParseResult.Parser.AST
import Text.Megaparsec
import Text.Megaparsec.Char

-- * Main parsers

parser :: Parser a -> Parser ParameterAST
parser stopParser =
  ParameterAST
    placeholder
    <$> many (parameterStatementParser stopParser)

parameterStatementParser :: Parser a -> Parser ParameterStatement
parameterStatementParser stopParser = do
  notFollowedBy stopParser
  firstParameterPart <- parameterPartParser
  secondParameterPart <- optional (requiresParser *> parameterPartParser)
  return $
    ParameterStatement placeholder firstParameterPart secondParameterPart

parameterPartParser :: Parser ParameterPart
parameterPartParser = do
  i <- identifierParser
  _ <- openParenth
  parameterPart <- valueListParser i
  _ <- closingParenth
  return parameterPart

valueListParser :: String -> Parser ParameterPart
valueListParser i =
  (MultiParameterPart i <$> multiValueListParser)
    <|> (SingleParameterPart i <$> singleValueListParser)

singleValueListParser :: Parser [ParameterValue]
singleValueListParser = sepBy valueParser comma

multiValueListParser :: Parser [[ParameterValue]]
multiValueListParser = sepBy1 valueRangeParser comma

valueRangeParser :: Parser [ParameterValue]
valueRangeParser =
  openSquare *> singleValueListParser <* closingSquare

valueParser :: Parser ParameterValue
valueParser = do
  _ <- openQuotes
  valueParts <- some valuePartParser
  _ <- closingQuotes
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
  _ <- (try . lookAhead) (fmap (: []) closingQuotes <|> openOutput)
  return ()

idUsageValuePartParser :: Parser ParameterValuePart
idUsageValuePartParser = do
  _ <- openOutput
  i <- identifierParser
  _ <- closingOutput
  return $ IdUsage i