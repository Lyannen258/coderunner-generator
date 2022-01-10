module CoderunnerGenerator.Types.AbstractSyntaxTree where

import Data.Tree (Tree (Node), drawTree)

-- | Recursive data type for the abstract syntax tree
data AST = AST
  { -- | The node label
    label :: Label,
    -- | The node value
    --
    -- It is the corresponding string in the template code.
    -- Only leaf nodes have values.
    value :: String,
    -- | Child nodes of the node
    children :: [AST]
  }

instance Show AST where
  show ast = drawTree $ toDataTree ast

-- | All possible node labels for the AST
data Label
  = CoderunnerFile
  | ParameterSection
  | ParameterHeadline
  | ParameterBody
  | ParameterStatement
  | Requires
  | ParameterDefinition
  | ParameterInformation
  | Enumeration
  | Value
  | Generation
  | ArbitraryPart
  | Blueprint
  | Property
  | Ellipse
  | BlueprintUsage
  | ParameterUsage
  | Identifier
  | PropertyPart
  | FunctionCallPart
  | Argument
  | TaskSection
  | SolutionSection
  | PreAllocationSection
  | Body
  | TestSection
  | TestOutcome
  | TestBody
  | TestCode
  | TestCase
  | Constant
  deriving (Show, Eq)

-- | Convert an AST to the Data.Tree type
toDataTree :: AST -> Tree String
toDataTree (AST label value children) = Node (show label ++ " (\"" ++ value ++ "\")") (map toDataTree children)

-- * AST Information

-- | Accepts an AST which must be a ParameterStatement or a children of it. Returns the statement type.
statementType :: AST -> Either String Label
statementType (AST Enumeration _ _) = Right Enumeration
statementType (AST Generation _ _) = Right Generation
statementType (AST Blueprint _ _) = Right Blueprint
statementType (AST BlueprintUsage _ _) = Right BlueprintUsage
statementType (AST ParameterStatement _ children) = statementType (head children)
statementType (AST ParameterDefinition _ children) = statementType $ children !! 1
statementType (AST ParameterInformation _ children) = statementType $ head children
statementType _ =
  Left "ParameterStatement does not contain a parameter definition"

-- | Returns, if the given AST has the structure of an enumeration __with__ requires relation
isEnumerationWithRequires :: AST -> Bool
isEnumerationWithRequires
  ( AST
      ParameterStatement
      _
      [ def1@(AST ParameterDefinition _ _),
        req@(AST Requires _ _),
        def2@(AST ParameterDefinition _ _)
        ]
    ) = True
isEnumerationWithRequires _ = False

-- | Returns, if the given AST has the structure of an enumeration with__out__ requires relation
isEnumerationWithoutRequires :: AST -> Bool
isEnumerationWithoutRequires
  ( AST
      ParameterStatement
      _
      [ def@(AST ParameterDefinition _ _)
        ]
    ) = True
isEnumerationWithoutRequires _ = False