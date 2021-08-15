module Parser where

import Text.Parsec
import GHC.Show (Show)
import Data.Tree (Tree(Node))

data AST = AST {
    label    :: String,
    value    :: String,
    children :: [AST]
    } deriving (Show)

toDataTree :: AST -> Tree String
toDataTree (AST label value children) = Node (label ++ "(" ++ value ++ ")") (map toDataTree children)

coderunnerParser :: Parsec String () AST
coderunnerParser = do
    parameterSection <- parameterSectionParser
    return (AST "CoderunnerFile" "" [
            parameterSection
        ])


-- Parameter Section Parsers

parameterSectionParser :: Parsec String () AST
parameterSectionParser = do
    headline <- string "Parameter:"
    newline
    body <- parameterBodyParser
    return (AST "ParameterSection" "" 
        [
            AST "ParameterHeadline" headline [],
            body
        ])


parameterBodyParser :: Parsec String () AST
parameterBodyParser = do
    statements <- many parameterStatementParser
    return (AST "ParameterBody" "" statements)
    
parameterStatementParser :: Parsec String () AST
parameterStatementParser = do
    definition <- parameterDefinitionParser
    astNodes <- try parameterStatementRequiresParser
    return (AST "ParameterStatement" "" (definition : astNodes))

parameterStatementRequiresParser :: Parsec String () [AST]
parameterStatementRequiresParser = do
    many1 (oneOf " \t")
    requires <- string "REQUIRES"
    many1 (oneOf " \t")
    definition <- parameterDefinitionParser
    let requiresNode = AST "Requires" requires []
    return 
        [
            AST "Requires" requires [],
            definition
        ]


parameterDefinitionParser :: Parsec String () AST
parameterDefinitionParser = do
    identifier <- identifierParser
    char '('
    -- information <- parameterInformationParser
    -- char ")"
    return (AST "ParameterDefinition" "" 
        [
            identifier
            -- ,information
        ])


{- parameterInformationParser :: Parsec String () AST
parameterInformationParser = do -}


-- Parameter Usage Parsers

identifierParser :: Parsec String () AST
identifierParser = do
    dollar <- char '$'
    identifier <- many1 upper
    return (AST "Identifier" (dollar : identifier) [])
