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


parameterSectionParser :: Parsec String () AST
parameterSectionParser = do
    p <- string "Parameter:"
    newline
    --statements <- many parameterStatementParser
    return (AST "ParameterSection" "" 
        [
            AST "ParameterHeadline" "Parameter:" []
        ])
