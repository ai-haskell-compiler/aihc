-- | Data types representing extracted interface information, serializable to YAML.
module Aihc.Dev.ExtractHi.Types
  ( PackageInterface (..),
    ModuleInterface (..),
    ExportedType (..),
    ExportedValue (..),
    ExportedClass (..),
    ClassMethod (..),
    FixityInfo (..),
    FixityDirection (..),
  )
where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Text (Text)

-- | Complete interface for a package: all exposed modules.
data PackageInterface = PackageInterface
  { piPackage :: Text,
    piModules :: [ModuleInterface]
  }
  deriving (Show)

instance ToJSON PackageInterface where
  toJSON pi' =
    object
      [ "package" .= piPackage pi',
        "modules" .= piModules pi'
      ]

-- | Interface for a single module.
data ModuleInterface = ModuleInterface
  { miModule :: Text,
    miTypes :: [ExportedType],
    miValues :: [ExportedValue],
    miClasses :: [ExportedClass],
    miFixities :: [FixityInfo]
  }
  deriving (Show)

instance ToJSON ModuleInterface where
  toJSON mi =
    object
      [ "module" .= miModule mi,
        "types" .= miTypes mi,
        "values" .= miValues mi,
        "classes" .= miClasses mi,
        "fixities" .= miFixities mi
      ]

-- | An exported type or data declaration.
data ExportedType = ExportedType
  { etName :: Text,
    etKind :: Text,
    etConstructors :: [Text]
  }
  deriving (Show)

instance ToJSON ExportedType where
  toJSON et =
    object
      [ "name" .= etName et,
        "kind" .= etKind et,
        "constructors" .= etConstructors et
      ]

-- | An exported value (function or variable).
data ExportedValue = ExportedValue
  { evName :: Text,
    evType :: Text
  }
  deriving (Show)

instance ToJSON ExportedValue where
  toJSON ev =
    object
      [ "name" .= evName ev,
        "type" .= evType ev
      ]

-- | An exported type class.
data ExportedClass = ExportedClass
  { ecName :: Text,
    ecMethods :: [ClassMethod]
  }
  deriving (Show)

instance ToJSON ExportedClass where
  toJSON ec =
    object
      [ "name" .= ecName ec,
        "methods" .= ecMethods ec
      ]

-- | A method within a type class.
data ClassMethod = ClassMethod
  { cmName :: Text,
    cmType :: Text
  }
  deriving (Show)

instance ToJSON ClassMethod where
  toJSON cm =
    object
      [ "name" .= cmName cm,
        "type" .= cmType cm
      ]

-- | Fixity declaration for an operator.
data FixityInfo = FixityInfo
  { fiName :: Text,
    fiDirection :: FixityDirection,
    fiPrecedence :: Int
  }
  deriving (Show)

instance ToJSON FixityInfo where
  toJSON fi =
    object
      [ "name" .= fiName fi,
        "direction" .= fiDirection fi,
        "precedence" .= fiPrecedence fi
      ]

-- | Direction of a fixity declaration.
data FixityDirection = InfixL | InfixR | InfixN
  deriving (Show)

instance ToJSON FixityDirection where
  toJSON InfixL = String "infixl"
  toJSON InfixR = String "infixr"
  toJSON InfixN = String "infix"
