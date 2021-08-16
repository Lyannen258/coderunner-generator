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
toDataTree (AST label value children) = Node (label ++ " (\"" ++ value ++ "\")") (map toDataTree children)

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
    linebreak
    body <- parameterBodyParser
    return (AST "ParameterSection" ""
        [
            AST "ParameterHeadline" headline [],
            body
        ])


parameterBodyParser :: Parsec String () AST
parameterBodyParser = do
    statements <- sepBy parameterStatementParser linebreak
    return (AST "ParameterBody" "" statements)

parameterStatementParser :: Parsec String () AST
parameterStatementParser = do
    definition <- parameterDefinitionParser
    astNodes <- option [] parameterStatementRequiresParser
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
    information <- parameterInformationParser
    char ')'
    return (AST "ParameterDefinition" ""
        [
            identifier,
            information
        ])


parameterInformationParser :: Parsec String () AST
parameterInformationParser = do
    information <- enumerationParser {- <|>
                   generationParser <|>
                   blueprintParser <|>
                   blueprintUsageParser -}
    return (AST "ParameterInformation" "" [information])

enumerationParser :: Parsec String () AST
enumerationParser = do
    values <- try valuesWithCommaParser <|> valuesParser
    return (AST "Enumeration" "" values)

valuesParser :: Parsec String () [AST]
valuesParser = do
    sepBy valueParser (char ',')

valueParser :: Parsec String () AST
valueParser = do
    many $ char ' '
    value <- many1 valueCharacterParser
    many $ char ' '
    return $ AST "Value" value []

valuesWithCommaParser :: Parsec String () [AST]
valuesWithCommaParser = do
    sepBy valueWithCommaParser (char ';')

valueWithCommaParser :: Parsec String () AST
valueWithCommaParser = do
    many $ oneOf " \t"
    char '{'
    value <- many1 $ noneOf "${}"
    char '}'
    many $ oneOf " \t"
    return (AST "Value" value [])

-- Parameter Usage Parsers

identifierParser :: Parsec String () AST
identifierParser = do
    dollar <- char '$'
    identifier <- many1 upper
    return (AST "Identifier" (dollar : identifier) [])


-- Character Parsers
valueCharacterParser :: Parsec String () Char
valueCharacterParser = do oneOf "!^°§%&/=?`´*+#'-.<>" <|> letter

linebreak :: Parsec String () Char
linebreak = do newline <|> crlf