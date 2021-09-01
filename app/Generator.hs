module Generator where

import Parser
import SemanticAnalyzer
import Interaction

generateOutput :: AST -> SymbolTable -> InteractionResult -> Either String String
generateOutput ast st vt = Left $ show vt