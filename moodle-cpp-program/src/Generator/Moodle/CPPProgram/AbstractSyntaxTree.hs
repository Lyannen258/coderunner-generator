{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Generator.Moodle.CPPProgram.AbstractSyntaxTree2
-- Description : Contains the components that represent an abstract syntax tree for C++ coderunner templates.
--
-- Contains the components that represent an abstract syntax tree. They are correlated to the rules in grammar.ebnf
--
-- The root node is 'Template'.
--
-- This module makes use of the microlens package and the module 'Lens.Micro.TH' to generate lenses. To use makeFields, all record accessor functions must be prefixed with the class name. See the documentation on hackage for further information.
module Generator.Moodle.CPPProgram.AbstractSyntaxTree where

import Generator.ParameterParser.AST (ParameterAST)
import Lens.Micro.TH

-- * Pos type class and type

data Position = Position
  { _line :: Int,
    _column :: Int
  }
  deriving (Show)

placeholder :: Position -- TO BE REMOVED
placeholder = Position  0  0

-- * AST components

-- | Represents a template file as an abstract syntax tree
data Template = Template
  { templatePosition :: Position,
    templateNameSection :: SimpleSection,
    templateAuthorSection :: SimpleSection,
    templateParameterSection :: Maybe ParameterSection,
    templateTaskSection :: Section,
    templateSolutionSection :: Maybe Section,
    templatePreAllocationSection :: Maybe Section,
    templateTestSection :: Maybe TestSection
  }
  deriving (Show)

-- * Parameter section components

-- | Represents the parameter section
data ParameterSection = ParameterSection
  { parameterSectionPosition :: Position,
    parameterSectionParameterBody :: ParameterAST
  }
  deriving (Show)

type Identifier = String

-- ** Output

-- | Represents an output using double curly brackets
data Output = Output
  { outputPosition :: Position,
    outputInner :: OutputInner
  }
  deriving (Show)

-- | Decides between parameter usage and text constant
data OutputInner
  = Parameter ParameterUsage
  | TextConstant String
  deriving (Show)

-- | Represents the usage of a parameter
data ParameterUsage = ParameterUsage
  { parameterUsageIdentifier :: Identifier,
    parameterUsageCallPart :: Maybe CallPart
  }
  deriving (Show)

-- | Represents the usage of a call in a parameter usage
data CallPart = CallPart
  { -- | Property name
    callPartIdentifier :: String,
    callPartArguments :: [String]
  }
  deriving (Show)

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
  deriving (Show)

-- | Represents a test section
data TestSection = TestSection
  { testSectionPosition :: Position,
    testSectionTestCases :: [TestCase]
  }
  deriving (Show)

-- | Represents a test case
data TestCase = TestCase
  { testCasePosition :: Position,
    testCaseInput :: [SectionBodyComponent],
    testCaseExpected :: [SectionBodyComponent]
  }
  deriving (Show)

-- ** Simple Section Definitions

-- | Represents a simple section where no parameter usages are allowed.
data SimpleSection = SimpleSection
  { simpleSectionPosition :: Position,
    simpleSectionHeadline :: String,
    simpleSectionBody :: String
  }
  deriving (Show)

-- Generate Lenses

makeLenses ''Position

makeFields ''Template
makeFields ''ParameterSection
makeFields ''ParameterUsage
makeFields ''CallPart
makeFields ''Section
makeFields ''TestSection
makeFields ''TestCase
makeFields ''SimpleSection