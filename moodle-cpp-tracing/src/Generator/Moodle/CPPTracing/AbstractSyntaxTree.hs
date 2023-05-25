{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Generator.Moodle.CPPFunction.AbstractSyntaxTree2
-- Description : Contains the components that represent an abstract syntax tree for C++ coderunner templates.
--
-- Contains the components that represent an abstract syntax tree. They are correlated to the rules in grammar.ebnf
--
-- The root node is 'Template'.
--
-- This module makes use of the microlens package and the module 'Lens.Micro.TH' to generate lenses. To use makeFields, all record accessor functions must be prefixed with the class name. See the documentation on hackage for further information.
module Generator.Moodle.CPPTracing.AbstractSyntaxTree where

import Generator.Atoms (ParameterName)
import Generator.ParameterParser.AST (ParameterAST)
import Lens.Micro.TH

-- * AST components

-- | Represents a template file as an abstract syntax tree
data Template = Template
  { titleSection :: SimpleSection,
    traceType :: TraceType,
    parameterSection :: ParameterAST,
    codeSection :: Section,
    feedbackSection :: Section
  }
  deriving (Show)

-- ** Output

-- | Represents the usage of a parameter
data ParameterUsage = ParameterUsage
  { identifier :: ParameterName,
    callPart :: Maybe CallPart
  }
  deriving (Show)

-- | Represents the usage of a call in a parameter usage
data CallPart = CallPart
  { -- | Property name
    identifier :: String,
    arguments :: [String]
  }
  deriving (Show)

-- ** Section Definitions

-- | Represents a generic section
data Section = Section
  { headline :: String,
    body :: [SectionBodyComponent]
  }
  deriving (Show)

-- | Represents a component of a generic section
data SectionBodyComponent
  = TextComponent String
  | OutputComponent Output
  deriving (Show)

-- | Represents an output using double curly brackets
data Output
  = Parameter ParameterUsage
  | TextConstant String
  deriving (Show)

-- ** Simple Section Definitions

-- | Represents a simple section where no parameter usages are allowed.
data SimpleSection = SimpleSection
  { headline :: String,
    body :: String
  }
  deriving (Show)

-- ** Trace type

data TraceType = Compile | Output
  deriving (Show)

-- Generate Lenses

makeFields ''Template
makeFields ''ParameterUsage
makeFields ''CallPart
makeFields ''Section
makeFields ''SimpleSection