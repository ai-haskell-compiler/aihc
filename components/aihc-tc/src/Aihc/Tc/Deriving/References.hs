-- | The library names that generated deriving code refers to, and the
-- classes that stock deriving knows.
--
-- A derived instance is ordinary surface syntax, so its method bodies
-- mention library values and types such as @True@, @(:)@, or @Int#@. The
-- type checker does not know where those live: the compiler that embeds it
-- says so through a 'DerivingReferences' table in its configuration. Class
-- methods such as @(==)@ or @showsPrec@ need no entry, because the class
-- being derived says where they are.
--
-- A reference names its module and gives the /source/ of its package rather
-- than a package identity, because the configuration is written once and a
-- package identity is only known while compiling: the primitive package is
-- the one of the configuration, and a helper that belongs to a class comes
-- from wherever that class was found. Only the first kind exists today, but
-- the classes of GHC that aihc does not generate code for yet -- @Lift@ in
-- the Template Haskell package, @Generic@ and @Data@ in the base one --
-- have their helpers next to themselves, outside the primitive package.
module Aihc.Tc.Deriving.References
  ( DerivingReference (..),
    ReferencePackage (..),
    DerivingReferences (..),
    StockClassLocation (..),
    referenceIdentity,
    stockClassLocationMatches,
    derivingReferenceList,
  )
where

import Aihc.Parser.Syntax (NameType (..))
import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Data.Text (Text)

-- | The resolved identity of one library name.
data DerivingReference = DerivingReference
  { referencePackage :: !ReferencePackage,
    referenceModule :: !Text,
    referenceName :: !Text,
    referenceNameType :: !NameType,
    referenceNamespace :: !ResolutionNamespace
  }
  deriving (Eq, Show)

-- | Which package a reference comes from. A configuration cannot spell a
-- package identity, which carries a version and a fingerprint that only the
-- compilation knows, so it says where to take one from.
data ReferencePackage
  = -- | The primitive package of the configuration. Every module can see
    -- it, so a generated body may always name one of these.
    ReferencePrimPackage
  | -- | The package that declares the class being derived. A body generated
    -- for a class outside the primitive package reaches its helpers this
    -- way; a module that derives the class has the class in scope, and the
    -- helpers sit beside it.
    ReferenceClassPackage
  deriving (Eq, Show)

-- | Where a class that stock deriving generates code for is declared.
data StockClassLocation = StockClassLocation
  { -- | The package the class must come from, or 'Nothing' for a class whose
    -- package the configuration cannot name because it is not the primitive
    -- one. A class matched without its package can be a user class of the
    -- same module and name; the generated body then names helpers that the
    -- user module does not have, which the generator reports before writing
    -- anything.
    stockLocationPackage :: !(Maybe PackageId),
    stockLocationModule :: !Text,
    stockLocationName :: !Text
  }
  deriving (Eq, Show)

-- | The identity a reference denotes, given the primitive package of the
-- configuration and the package of the class being derived.
referenceIdentity :: PackageId -> PackageId -> DerivingReference -> (PackageId, Text, Text)
referenceIdentity primPackage classPackage reference =
  (package, referenceModule reference, referenceName reference)
  where
    package =
      case referencePackage reference of
        ReferencePrimPackage -> primPackage
        ReferenceClassPackage -> classPackage

-- | Whether a class declared in one package and module is the located one.
stockClassLocationMatches :: (Text, Text) -> StockClassLocation -> Text -> Bool
stockClassLocationMatches (packageIdentity, moduleName) location className =
  maybe True (== PackageId packageIdentity) (stockLocationPackage location)
    && moduleName == stockLocationModule location
    && className == stockLocationName location

-- | Every library name that a generated instance body may mention.
data DerivingReferences = DerivingReferences
  { -- | The @True@ constructor of @Bool@.
    derivingTrue :: !DerivingReference,
    -- | The @False@ constructor of @Bool@.
    derivingFalse :: !DerivingReference,
    -- | The @LT@ constructor of @Ordering@.
    derivingLT :: !DerivingReference,
    -- | The @EQ@ constructor of @Ordering@.
    derivingEQ :: !DerivingReference,
    -- | The @GT@ constructor of @Ordering@.
    derivingGT :: !DerivingReference,
    -- | The @I#@ constructor that boxes an @Int#@ into an @Int@.
    derivingIntCon :: !DerivingReference,
    -- | The primitive @Int#@ type, which types the precedence literals of
    -- derived @Show@ instances.
    derivingIntPrimType :: !DerivingReference,
    -- | The @(>=)@ method of @Ord@, compared on @Int@ precedences.
    derivingGreaterOrEqual :: !DerivingReference,
    -- | The list constructor @(:)@, which derived @Show@ renders through.
    derivingCons :: !DerivingReference,
    -- | The @(>>=)@ method of @Monad@, which sequences a derived @Read@
    -- parser that keeps its result.
    derivingBind :: !DerivingReference,
    -- | The @(>>)@ method of @Monad@, which sequences a derived @Read@
    -- parser that drops its result.
    derivingThen :: !DerivingReference,
    -- | The @return@ method of @Monad@, which delivers the parsed value.
    derivingReturn :: !DerivingReference,
    -- | @parens@, which accepts the optional parentheses around a value.
    derivingReadParens :: !DerivingReference,
    -- | @prec@, which sets the precedence context of one alternative.
    derivingReadPrecContext :: !DerivingReference,
    -- | @step@, which reads one field above the constructor precedence.
    derivingReadStep :: !DerivingReference,
    -- | @reset@, which reads a record field at the lowest precedence.
    derivingReadReset :: !DerivingReference,
    -- | @(+++)@, which offers the alternatives of a datatype.
    derivingReadAlternative :: !DerivingReference,
    -- | @pfail@, the parser of a datatype without constructors.
    derivingReadFail :: !DerivingReference,
    -- | @expectP@, which accepts one expected lexeme.
    derivingReadExpect :: !DerivingReference,
    -- | @readField@, which accepts @label =@ before a record field.
    derivingReadField :: !DerivingReference,
    -- | @readSymField@, which accepts @(op) =@ before a record field with
    -- a symbolic label.
    derivingReadSymField :: !DerivingReference,
    -- | The @Ident@ lexeme constructor, for a constructor name.
    derivingLexemeIdent :: !DerivingReference,
    -- | The @Symbol@ lexeme constructor, for an operator name.
    derivingLexemeSymbol :: !DerivingReference,
    -- | The @Punc@ lexeme constructor, for punctuation.
    derivingLexemePunc :: !DerivingReference,
    -- | The classes that stock deriving writes code for, and where each is
    -- declared. A location that names a package makes all three agree, so a
    -- user module that repeats a core-library module name does not make its
    -- own class stock.
    derivingStockClasses :: ![StockClassLocation],
    -- | The remaining stock classes of GHC, as the module that defines each
    -- and its name. The generator writes no code for them: it reports that
    -- stock deriving of the class is not supported and produces no
    -- instance. A wrong match therefore cannot produce wrong code, so these
    -- entries need no package and the table stays free of base-library
    -- package identities.
    derivingRecognizedClasses :: ![(Text, Text)]
  }
  deriving (Eq, Show)

-- | Every reference in the table, for callers that make the names visible
-- to later compiler phases.
derivingReferenceList :: DerivingReferences -> [DerivingReference]
derivingReferenceList references =
  [ derivingTrue references,
    derivingFalse references,
    derivingLT references,
    derivingEQ references,
    derivingGT references,
    derivingIntCon references,
    derivingIntPrimType references,
    derivingGreaterOrEqual references,
    derivingCons references,
    derivingBind references,
    derivingThen references,
    derivingReturn references,
    derivingReadParens references,
    derivingReadPrecContext references,
    derivingReadStep references,
    derivingReadReset references,
    derivingReadAlternative references,
    derivingReadFail references,
    derivingReadExpect references,
    derivingReadField references,
    derivingReadSymField references,
    derivingLexemeIdent references,
    derivingLexemeSymbol references,
    derivingLexemePunc references
  ]
