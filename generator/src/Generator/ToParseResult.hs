module Generator.ToParseResult where

import Generator.ParameterParser.AST (ParameterAST)
import Generator.ParseResult (ParseResult)
import Generator.ParseResult.FromAST (fromParameterAST)

-- | Overloading the constructParseResult function for different
-- return types
class Show return => ToParseResult return where
  toParseResult :: return -> Either String ParseResult

-- | Instance of ToParseResult for ParseResult (trivial)
instance ToParseResult ParseResult where
  toParseResult parseResult = Right parseResult

-- | Instance of ToParseResult for a string
--
-- The string must contain the definition of parameters in a
-- specified syntax. It should be the parameter section of the
-- template file without the headline.
instance ToParseResult ParameterAST where
  toParseResult = fromParameterAST 