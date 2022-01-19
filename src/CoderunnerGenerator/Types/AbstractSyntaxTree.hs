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

import Lens.Micro.TH
import Lens.Micro.Extras (view)

-- * Pos type class and type

data Position = Position
  { _lineStart :: Int,
    _lineEnd :: Int,
    _colStart :: Int,
    _colEnd :: Int
  }

placeholder :: Position -- TO BE REMOVED
placeholder = Position 0 0 0 0

-- * AST components

-- | Represents a template file as an abstract syntax tree
data Template = Template
  { templatePosition :: Position,
    templateParameterSection :: ParameterSection,
    templateTaskSection :: TaskSection,
    templateSolutionSection :: SolutionSection,
    templatePreAllocationSection :: PreAllocationSection,
    templateTestSection :: TestSection
  }

-- * Parameter section components

-- | Represents the parameter section
data ParameterSection = ParameterSection
  { parameterSectionPosition :: Position,
    parameterSectionParameterBody :: ParameterBody
  }

-- | Represents the parameter body
data ParameterBody = ParameterBody
  { parameterBodyPosition :: Position,
    parameterBodyParameterStatements :: [ParameterStatement]
  }

-- | Represents a parameter statement
data ParameterStatement
  = EnumerationStatement Enumeration
  | GenerationStatement Generation
  | BlueprintStatement Blueprint
  | BlueprintUsageStatement BlueprintUsage

-- | Represents an enumeration statement
data Enumeration = Enumeration
  { enumerationPosition :: Position,
    enumerationMain :: EnumerationPart,
    enumerationRequires :: Maybe EnumerationPart
  }

-- | Represents an enumeration part
--
-- Consists of an identifier and a value list
data EnumerationPart = EnumerationPart
  { enumerationPartIdentifier :: Identifier,
    enumerationPartValues :: [String]
  }

-- | Represents a generation statement
data Generation = Generation
  { generationPosition :: Position,
    generationIdentifier :: Identifier,
    generationBody :: Mixed
  }

-- | Represents a blueprint statement
data Blueprint = -- | Has ellipse?
  Blueprint
  { blueprintPosition :: Position,
    blueprintIdentifier :: Identifier,
    -- | Properties of the blueprint
    blueprintProperties :: [Property],
    blueprintHasEllipse :: Bool
  }

-- | Represents a property of a blueprint. Just an alias for @String@.
type Property = String

-- | Represents a blueprint usage statements
data BlueprintUsage = BlueprintUsage
  { blueprintUsagePosition :: Position,
    -- | name of the blueprint usage
    blueprintUsageIdentifier :: Identifier,
    -- | name of the used blueprint
    blueprintUsageblueprintUsed :: Identifier,
    -- | List of values for the blueprint properties
    blueprintUsageValues :: [String]
  }

-- | Represents an identifier. Just an alias for @String@.
type Identifier = String

-- ** Parameter Usage

-- | Represents the usage of a parameter
data ParameterUsage = ParameterUsage
  { parameterUsagePosition :: Position,
    parameterUsageIdentifier :: Identifier,
    parameterUsagePropertyPart :: Maybe PropertyPart
  }

-- | Represents the usage of a blueprint property in a parameter usage
data PropertyPart = PropertyPart
  { -- | Property name (without @)
    propertyPartProperty :: String,
    propertyPartArguments :: Maybe FunctionCallPart
  }

-- | Represents a function call part when using a blueprint property in a parameter usage
type FunctionCallPart = [String]

-- ** Other Section Definitions

-- | Represents a mixture of constants and parameter usages
type Mixed = [MixedPart]

-- | Represents a part of 'Mixed', either a constant or a parameter usage
data MixedPart
  = ParameterPart ParameterUsage
  | ConstantPart String

-- | Represents the task section
data TaskSection = TaskSection
  { taskSectionPosition :: Position,
    taskSectionBody :: Mixed
  }

-- | Represents the solution section
data SolutionSection = SolutionSection
  { solutionSectionPosition :: Position,
    solutionSectionBody :: Mixed
  }

-- | Represents the pre allocation section
data PreAllocationSection = PreAllocationSection
  { preAllocationSectionPosition :: Position,
    preAllocationSectionBody :: Mixed
  }

-- | Represents the test section
data TestSection = TestSection
  { testSectionPosition :: Position,
    testSectionBody :: TestBody
  }

-- Represents the test section body
type TestBody = [TestCase]

-- | Represents a test case in the test section
data TestCase = TestCase
  { testCasePosition :: Position,
    testCaseCode :: TestCode,
    testCaseOutcome :: TestOutcome
  }

-- | Represents the test code of a test case
type TestCode = Mixed

-- | Represents the expected test outcome
data TestOutcome
  = ConstantOutcome String
  | ParameterOutcome ParameterUsage

-- Generate Lenses

makeLenses ''Position

makeFields ''Template
makeFields ''ParameterSection
makeFields ''ParameterBody
makeFields ''Enumeration
makeFields ''EnumerationPart
makeFields ''Generation
makeFields ''Blueprint
makeFields ''BlueprintUsage
makeFields ''ParameterUsage
makeFields ''PropertyPart
makeFields ''TaskSection
makeFields ''SolutionSection
makeFields ''PreAllocationSection
makeFields ''TestSection
makeFields ''TestCase

-- * Functions

instance HasParameterStatements Template [ParameterStatement] where
  parameterStatements = parameterSection . parameterBody . parameterStatements