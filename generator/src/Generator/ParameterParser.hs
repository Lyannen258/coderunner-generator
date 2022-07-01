{-# LANGUAGE LambdaCase #-}
module Generator.ParameterParser (parser) where

import Generator.ParameterParser.AST
import Generator.ParserUtils
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

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
valueParser = regularValueParser <|> tupleValueParser

regularValueParser :: Parser ParameterValue
regularValueParser = valueParserDouble <|> valueParserSingle

valueParserDouble :: Parser ParameterValue
valueParserDouble = do
  _ <- openQuotes
  valueParts <- some $ valuePartParser closingQuotes
  _ <- closingQuotes
  return $ Regular valueParts

valueParserSingle :: Parser ParameterValue
valueParserSingle = do
  _ <- openSingle
  valueParts <- some $ valuePartParser closingSingle
  _ <- closingSingle
  return $ Regular valueParts

tupleValueParser :: Parser ParameterValue
tupleValueParser = do
  _ <- openParenth
  regularValues <- singleValueListParser
  _ <- closingParenth
  let valueParts = map (\case
        Regular pvps -> Just pvps
        Tuple _ -> Nothing) regularValues
  return $ Tuple (catMaybes valueParts)

valuePartParser :: Parser Char -> Parser ParameterValuePart
valuePartParser endParser =
  try (simpleValuePartParser endParser) <|> idUsageOrTupleSelectValuePartParser

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

idUsageOrTupleSelectValuePartParser :: Parser ParameterValuePart
idUsageOrTupleSelectValuePartParser = do
  _ <- openOutput
  pvp <- try tupleSelectValuePartParser <|> idUsageValuePartParser 
  _ <- closingOutput
  return pvp

idUsageValuePartParser :: Parser ParameterValuePart
idUsageValuePartParser = do
  IdUsage <$> identifierParser

tupleSelectValuePartParser :: Parser ParameterValuePart
tupleSelectValuePartParser = do
  i <- identifierParser
  _ <- point
  _ <- string "get"
  _ <- openParenth
  n <- hlexeme $ some numberChar
  _ <- closingParenth
  case readMaybe n of
    Nothing -> fail "Tried to use a non-int argument in 'get'"
    Just int -> return $ TupleSelect i int
