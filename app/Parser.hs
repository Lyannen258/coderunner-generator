module Parser where

import Text.Parsec
import GHC.Show (Show)

data AST = AST {
    label    :: String,
    value    :: String,
    children :: [AST]
    } deriving (Show)


coderunnerParser :: Parsec String () AST
coderunnerParser = do
    parameterSection <- parameterSectionParser
    return (AST "CoderunnerFile" "" [
            parameterSection
        ])


parameterSectionParser :: Parsec String () AST
parameterSectionParser = do
    p <- string "Parameter:"
    newline
    --statements <- many parameterStatementParser
    return (AST "ParameterSection" "" 
        [
            AST "ParameterHeadline" "Parameter:" []
        ])
