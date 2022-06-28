module Generator.ParameterParser (parser) where

import Generator.ParameterParser.AST
import Generator.ParserUtils
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
valueParser = valueParserDouble <|> valueParserSingle

valueParserDouble :: Parser ParameterValue
valueParserDouble = do
  _ <- openQuotes
  valueParts <- some $ valuePartParser closingQuotes
  _ <- closingQuotes
  return $ ParameterValue valueParts

valueParserSingle :: Parser ParameterValue
valueParserSingle = do
  _ <- openSingle
  valueParts <- some $ valuePartParser closingSingle
  _ <- closingSingle
  return $ ParameterValue valueParts

valuePartParser :: Parser Char -> Parser ParameterValuePart
valuePartParser endParser =
  try (simpleValuePartParser endParser) <|> idUsageValuePartParser

simpleValuePartParser :: Parser Char -> Parser ParameterValuePart
simpleValuePartParser endParser = do
  notFollowedBy $ stringEndLookAhead endParser
  firstChar <- printChar
  rest <- manyTill printChar (stringEndLookAhead endParser)
  return $ Simple (firstChar : rest)

stringEndLookAhead :: Parser Char -> Parser ()
stringEndLookAhead endParser = do
  _ <- (try . lookAhead) (fmap (: []) endParser <|> openOutput)
  return ()

idUsageValuePartParser :: Parser ParameterValuePart
idUsageValuePartParser = do
  _ <- openOutput
  i <- identifierParser
  _ <- closingOutput
  return $ IdUsage i