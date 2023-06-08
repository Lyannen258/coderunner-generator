module Generator.ParameterParser (parser) where

import Generator.Atoms
import Generator.ParameterParser.AST
import Generator.ParserUtils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)
import Data.List (singleton)
import Control.Monad (join)

-- * Main parsers

parser :: Parser a -> Parser ParameterAST
parser stopParser =
  ParameterAST
    <$> many (parameterStatementParser stopParser)

parameterStatementParser :: Parser a -> Parser ParameterStatement
parameterStatementParser stopParser = do
  notFollowedBy stopParser
  firstParameterPart <- parameterPartParser
  secondParameterPart <- optional (requiresParser *> parameterPartParser)
  return $
    ParameterStatement firstParameterPart secondParameterPart

parameterPartParser :: Parser ParameterPart
parameterPartParser = do
  is <- identifierParser
  let i = ParameterName is
  _ <- openParenth
  valueRange <- valueRangeParser
  _ <- closingParenth
  return $ ParameterPart i valueRange

valueRangeParser :: Parser (Range IncompleteAtomicValue)
valueRangeParser = do
  choice
    [ try $ fmap makeSingleRange singleRangeParser,
      try $ fmap makeSingleTupleRange singleTupleRangeParser,
      try $ fmap makeMultiRange multiRangeParser,
      try $ fmap makeMultiTupleRange multiTupleRangeParser
    ]

singleRangeParser :: Parser [IncompleteAtomicValue]
singleRangeParser = sepBy1 regularValueParser comma

singleTupleRangeParser :: Parser [[IncompleteAtomicValue]]
singleTupleRangeParser = sepBy1 tupleValueParser comma

multiRangeParser :: Parser [[IncompleteAtomicValue]]
multiRangeParser = sepBy1 multiValueParser comma

multiTupleRangeParser :: Parser [[[IncompleteAtomicValue]]]
multiTupleRangeParser = sepBy1 multiTupleValueParser comma

regularValueParser :: Parser IncompleteAtomicValue
regularValueParser = valueParserDouble <|> valueParserSingle

valueParserDouble :: Parser IncompleteAtomicValue
valueParserDouble = do
  _ <- openQuotes
  valueParts <- many $ valuePartParser closingQuotes
  _ <- closingQuotes
  return $ IncompleteAtomicValue valueParts

valueParserSingle :: Parser IncompleteAtomicValue
valueParserSingle = do
  _ <- openSingle
  valueParts <- many $ valuePartParser closingSingle
  _ <- closingSingle
  return $ IncompleteAtomicValue valueParts

tupleValueParser :: Parser [IncompleteAtomicValue]
tupleValueParser = do
  openParenth *> singleRangeParser <* closingParenth

multiValueParser :: Parser [IncompleteAtomicValue]
multiValueParser =
  openSquare *> singleRangeParser <* closingSquare

multiTupleValueParser :: Parser [[IncompleteAtomicValue]]
multiTupleValueParser =
  openSquare *> sepBy1 tupleValueParser comma <* closingSquare

valuePartParser :: Parser Char -> Parser ValuePart
valuePartParser endParser =
  try (simpleValuePartParser endParser) <|> idUsageOrTupleSelectValuePartParser

simpleValuePartParser :: Parser Char -> Parser ValuePart
simpleValuePartParser endParser = do
  notFollowedBy $ stringEndLookAhead endParser
  firstChar <- ch
  rest <- manyTill ch (stringEndLookAhead endParser)
  let rest' = concat rest
  return $ StringPart (firstChar ++ rest')
  where
    ch = fmap singleton printChar <|> eol

stringEndLookAhead :: Parser Char -> Parser ()
stringEndLookAhead endParser = do
  _ <- (try . lookAhead) (fmap (: []) endParser <|> openOutput)
  return ()

idUsageOrTupleSelectValuePartParser :: Parser ValuePart
idUsageOrTupleSelectValuePartParser = do
  _ <- openOutput
  pvp <- try tupleSelectValuePartParser <|> idUsageValuePartParser
  _ <- closingOutput
  return pvp

idUsageValuePartParser :: Parser ValuePart
idUsageValuePartParser = do
  IdUsage . ParameterName <$> identifierParser

tupleSelectValuePartParser :: Parser ValuePart
tupleSelectValuePartParser = do
  i <- ParameterName <$> identifierParser
  _ <- point
  _ <- string "get"
  _ <- openParenth
  n <- hlexeme $ some numberChar
  _ <- closingParenth
  case readMaybe n of
    Nothing -> fail "Tried to use a non-int argument in 'get'"
    Just int -> return $ TupleSelect i int
