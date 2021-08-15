module Parser where

import Text.Parsec
import GHC.Show (Show)

data AST = AST {
    label    :: String,
    value     :: String,
    children :: [AST]
    } deriving (Show)


coderunnerParser :: Parsec String () AST
coderunnerParser = do
    p <- string "Parameter:"
    newline
    return (AST p [])