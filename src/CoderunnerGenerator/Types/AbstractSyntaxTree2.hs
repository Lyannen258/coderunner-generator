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

-- | Represents a parameter definition
data ParameterDefinition
  = ParameterDefinition
      Position
      Identifier
      ParameterInformation

-- | Represents one of the 4 types of parameter information
data ParameterInformation
  = EnumerationInformation Enumeration
  | GenerationInformation Generation
  | BlueprintInformation Blueprint
  | BlueprintUsageInformation BlueprintUsage

-- | Represents an enumeration parameter information
data Enumeration = Enumeration Position [String]

-- | Represents a generation parameter information
data Generation
  = -- | Identifiers of the Generator
    Generation
      Position
      [Identifier]

-- | Represents a blueprint parameter information
data Blueprint
  = Blueprint
      Position
      [Property]
      -- ^ Properties of the blueprint
      Bool
      -- ^ Has ellipse?

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

-- | Represents an identifier. Just an alias for @String@.
type Identifier = String

-- ** Parameter Usage

-- | Represents the usage of a parameter
data ParameterUsage
  = ParameterUsage
      Position
      Identifier
      (Maybe PropertyPart)

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
data TaskSection =
  TaskSection
    Position
    Body

-- | Represents the solution section
data SolutionSection = 
  SolutionSection
    Position
    Body

-- | Represents the pre allocation section
data PreAllocationSection = 
  PreAllocationSection
    Position
    Body

-- | Represents the test section
data TestSection = 
  TestSection 
    Position
    [TestCase]

-- | Represents a test case in the test section
data TestCase = 
  TestCase
    Position
    TestCode
    TestOutcome

-- | Represents the test code of a test case
type TestCode = Body

-- | Represents the expected test outcome
data TestOutcome =
  ConstantOutcome String |
  ParameterOutcome ParameterUsage
