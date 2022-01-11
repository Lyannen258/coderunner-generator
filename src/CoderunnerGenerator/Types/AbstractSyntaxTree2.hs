module CoderunnerGenerator.Types.AbstractSyntaxTree2 where

-- * Pos type class and type

-- | Class for all types, that have a position in a source file
class Pos a where
  position :: a -> Position

data Position = Position
  { lineStart :: Int,
    lineEnd :: Int,
    colStart :: Int,
    colEnd :: Int
  }

-- * AST components

-- | Represents a template file as an abstract syntax tree
data Template
  = Template
      Position
      ParameterSection
      TaskSection
      SolutionSection
      PreAllocationSection
      TestSection

instance Pos Template where
  position (Template p _ _ _ _ _) = p

-- * Parameter section components

-- | Represents the parameter section
data ParameterSection = ParameterSection Position ParameterBody

-- | Represents the parameter body
data ParameterBody = ParameterBody Position [ParameterStatement]

-- | Represents a parameter statement
data ParameterStatement
  = -- | optional requires relation
    ParameterStatement
      Position
      ParameterDefinition
      (Maybe ParameterDefinition)

instance Pos ParameterStatement where
  position (ParameterStatement p _ _) = p

-- | Represents a parameter definition
data ParameterDefinition
  = ParameterDefinition
      Position
      Identifier
      ParameterInformation

instance Pos ParameterDefinition where
  position (ParameterDefinition p _ _) = p

-- | Represents one of the 4 types of parameter information
data ParameterInformation
  = EnumerationInformation Enumeration
  | GenerationInformation Generation
  | BlueprintInformation Blueprint
  | BlueprintUsageInformation BlueprintUsage

-- | Represents an enumeration parameter information
data Enumeration = Enumeration Position [String]

instance Pos Enumeration where
  position (Enumeration p _) = p

-- | Represents a generation parameter information
data Generation
  = -- | Identifiers of the Generator
    Generation
      Position
      [Identifier]

instance Pos Generation where
  position (Generation p _) = p

-- | Represents a blueprint parameter information
data Blueprint
  = Blueprint
      Position
      [Property]
      -- ^ Properties of the blueprint
      Bool
      -- ^ Has ellipse?

instance Pos Blueprint where
  position (Blueprint p _ _) = p

-- | Represents a property of a blueprint. Just an alias for @String@.
type Property = String

-- | Represents a blueprint usage parameter information
data BlueprintUsage
  = BlueprintUsage
      Position
      Identifier
      -- ^ name of the used blueprint
      [String]
      -- ^ List of values for the blueprint properties

instance Pos BlueprintUsage where
  position (BlueprintUsage p _ _) = p

-- | Represents an identifier. Just an alias for @String@.
type Identifier = String

-- ** Parameter Usage

-- | Represents the usage of a parameter
data ParameterUsage
  = ParameterUsage
      Position
      Identifier
      (Maybe PropertyPart)

instance Pos ParameterUsage where
  position (ParameterUsage p _ _) = p

-- | Represents the usage of a blueprint property in a parameter usage
data PropertyPart
  = PropertyPart
      String
      -- ^ Property name (without @)
      (Maybe FunctionCallPart)

-- | Represents a function call part when using a blueprint property in a parameter usage
type FunctionCallPart = [String]

-- ** Other Section Definitions

-- | Represents a body for the task, solution and pre-allocation section.
type Body = [BodyPart]

-- | Represents a part for the section body. It can only be a constant text part or a parameter usage
data BodyPart
  = ParameterPart ParameterUsage
  | ConstantPart String

-- | Represents the task section
data TaskSection
  = TaskSection
      Position
      Body

instance Pos TaskSection where
  position (TaskSection p _) = p

-- | Represents the solution section
data SolutionSection
  = SolutionSection
      Position
      Body

instance Pos SolutionSection where
  position (SolutionSection p _) = p

-- | Represents the pre allocation section
data PreAllocationSection
  = PreAllocationSection
      Position
      Body

instance Pos PreAllocationSection where
  position (PreAllocationSection p _) = p

-- | Represents the test section
data TestSection
  = TestSection
      Position
      [TestCase]

instance Pos TestSection where
  position (TestSection p _) = p

-- | Represents a test case in the test section
data TestCase
  = TestCase
      Position
      TestCode
      TestOutcome

instance Pos TestCase where
  position (TestCase p _ _) = p

-- | Represents the test code of a test case
type TestCode = Body

-- | Represents the expected test outcome
data TestOutcome
  = ConstantOutcome String
  | ParameterOutcome ParameterUsage
