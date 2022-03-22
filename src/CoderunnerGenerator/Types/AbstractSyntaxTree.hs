{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : CoderunnerGenerator.Types.AbstractSyntaxTree2
-- Description : Contains the components that represent an abstract syntax tree.
--
-- Contains the components that represent an abstract syntax tree. They are correlated to the rules in grammar.ebnf
--
-- The root node is 'Template'.
--
-- This module makes use of the microlens package and the module 'Lens.Micro.TH' to generate lenses. To use makeFields, all record accessor functions must be prefixed with the class name. See the documentation on hackage for further information.
module CoderunnerGenerator.Types.AbstractSyntaxTree where

import Lens.Micro.Extras (view)
import Lens.Micro.TH

-- * Pos type class and type

data Position = Position
  { _lineStart :: Int,
    _lineEnd :: Int,
    _colStart :: Int,
    _colEnd :: Int
  }
  deriving (Show)

placeholder :: Position -- TO BE REMOVED
placeholder = Position 0 0 0 0

-- * AST components

-- | Represents a template file as an abstract syntax tree
data Template = Template
  { templatePosition :: Position,
    templateParameterSection :: ParameterSection,
    templateOtherSections :: [Section]
  }
  deriving (Show)

-- * Parameter section components

-- | Represents the parameter section
data ParameterSection = ParameterSection
  { parameterSectionPosition :: Position,
    parameterSectionParameterBody :: ParameterBody
  }
  deriving (Show)

-- | Represents the parameter body
data ParameterBody = ParameterBody
  { parameterBodyPosition :: Position,
    parameterBodyParameterStatements :: [ParameterStatement]
  }
  deriving (Show)

-- | Represents an enumeration statement
data ParameterStatement = ParameterStatement
  { parameterStatementPosition :: Position,
    parameterStatementMain :: ParameterPart,
    parameterStatementRequires :: Maybe ParameterPart
  }
  deriving (Show)

-- | Represents an enumeration part
--
-- Consists of an identifier and a value list
data ParameterPart = ParameterPart
  { parameterPartIdentifier :: Identifier,
    parameterPartValues :: [ParameterValue]
  }
  deriving (Show)

-- | Represents a Parameter Value
--
-- Consists of a list of parameter parts
newtype ParameterValue = ParameterValue [ParameterValuePart]
  deriving (Show)

-- | Represents a component of a parameter value
--
-- Possible components are simple strings and usages of identifiers
data ParameterValuePart = Simple String | IdUsage Identifier
  deriving (Show)


type Identifier = String

-- ** Output

-- | Represents an output using double curly brackets
data Output
  = Parameter ParameterUsage
  | TextConstant String
  deriving (Show)

-- | Represents the usage of a parameter
data ParameterUsage = ParameterUsage
  { parameterUsagePosition :: Position,
    parameterUsageIdentifier :: Identifier,
    parameterUsagePropertyPart :: Maybe PropertyPart
  }
  deriving (Show)

-- | Represents the usage of a property in a parameter usage
data PropertyPart = PropertyPart
  { -- | Property name (without @)
    propertyPartProperty :: String,
    propertyPartArguments :: Maybe FunctionCallPart
  }
  deriving (Show)

-- | Represents a function call part a parameter usage
type FunctionCallPart = [String]

-- ** Other Section Definitions

-- | Represents a generic section
data Section = Section
  { sectionPosition :: Position,
    sectionHeadline :: String,
    sectionBody :: [SectionBodyComponent]
  }
  deriving (Show)

-- | Represents a component of a generic section
data SectionBodyComponent
  = TextComponent String
  | OutputComponent Output
  | CommentComponent String
  deriving (Show)

-- Generate Lenses

makeLenses ''Position

makeFields ''Template
makeFields ''ParameterSection
makeFields ''ParameterBody
makeFields ''ParameterStatement
makeFields ''ParameterPart
makeFields ''ParameterUsage
makeFields ''PropertyPart
makeFields ''Section

-- * Functions

instance HasParameterStatements Template [ParameterStatement] where
  parameterStatements = parameterSection . parameterBody . parameterStatements