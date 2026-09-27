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
--
-- A reference of that kind has to be one the module deriving the class can
-- already see. Only an identity a module imports, or that a module it
-- imports re-exports, is in its type environment, so these name the types
-- and the constructors a user of the class handles rather than the
-- combinators of a library module they would have no reason to import.
module Aihc.Tc.Deriving.References
  ( DerivingReference (..),
    ReferencePackage (..),
    DerivingReferences (..),
    GenericReferences (..),
    UnliftedFieldReferences (..),
    StockClassLocation (..),
    referenceIdentity,
    unliftedFieldReferenceList,
    stockClassLocationMatches,
    derivingReferenceList,
    genericTermReferences,
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
    -- | The unlifted field types that a derived @Eq@ or @Ord@ compares
    -- with primitive operators, because no instance exists at them.
    derivingUnliftedFields :: ![UnliftedFieldReferences],
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
    -- | The @pure@ method of @Applicative@, which a derived @traverse@
    -- delivers a constructor and its unvisited fields with.
    derivingPure :: !DerivingReference,
    -- | The @(\<*\>)@ method of @Applicative@, which a derived @traverse@
    -- applies a constructor to one visited field with.
    derivingApply :: !DerivingReference,
    -- | The @ConE@ expression, which names a data constructor.
    derivingLiftConE :: !DerivingReference,
    -- | The @AppE@ expression, which applies one expression to another.
    derivingLiftAppE :: !DerivingReference,
    -- | @mkNameG_d@, which names a data constructor by its package, its
    -- module, and its spelling.
    derivingLiftDataConName :: !DerivingReference,
    -- | @unsafeCodeCoerce@, which makes a typed lift out of an untyped one.
    derivingLiftCodeCoerce :: !DerivingReference,
    -- | The representation types and the metadata of @GHC.Generics@, which
    -- a derived @Generic@ instance is written entirely out of.
    derivingGeneric :: !GenericReferences,
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

-- | The names that compare one unlifted field type. A class has no
-- instance at an unlifted type, so a derived @Eq@ or @Ord@ compares such a
-- field with the primitive operators of its type, as GHC does for @Int#@,
-- @Word#@, @Char#@, @Double#@, @Float#@, and @Addr#@. The operators return
-- an @Int#@ that is @0#@ for false.
data UnliftedFieldReferences = UnliftedFieldReferences
  { -- | The primitive type, such as @Addr#@.
    unliftedFieldType :: !DerivingReference,
    -- | The equality operator of the type, such as @eqAddr#@.
    unliftedFieldEq :: !DerivingReference,
    -- | The less-than operator of the type, such as @ltAddr#@.
    unliftedFieldLt :: !DerivingReference
  }
  deriving (Eq, Show)

-- | Every name of one 'UnliftedFieldReferences'.
unliftedFieldReferenceList :: UnliftedFieldReferences -> [DerivingReference]
unliftedFieldReferenceList field = [unliftedFieldType field, unliftedFieldEq field, unliftedFieldLt field]

-- | The names of @GHC.Generics@ that a derived @Generic@ instance mentions.
--
-- They are grouped apart from the rest because there are so many of them:
-- the representation of a datatype spells out its whole shape, and its
-- metadata spells out one promoted constructor for every strictness and
-- fixity mark a field or a constructor can carry.
--
-- The three namespaces are kept apart, because @U1@ and @(:*:)@ name both a
-- type and a constructor, and a promoted constructor is a term name used in
-- a type. Only 'genericTermReferences' are checked for availability: a type
-- name is not in the term environment, and the module that derives the class
-- imports the module all of these live in.
data GenericReferences = GenericReferences
  { -- | The @M1@ constructor, which wraps a node in its metadata.
    genericM1 :: !DerivingReference,
    -- | @unM1@, which strips one metadata node.
    genericUnM1 :: !DerivingReference,
    -- | The @K1@ constructor, which holds one field.
    genericK1 :: !DerivingReference,
    -- | @unK1@, which takes the field back out.
    genericUnK1 :: !DerivingReference,
    -- | The @U1@ constructor of a constructor without fields.
    genericU1 :: !DerivingReference,
    -- | The @L1@ constructor, which selects the left half of a sum.
    genericL1 :: !DerivingReference,
    -- | The @R1@ constructor, which selects the right half of a sum.
    genericR1 :: !DerivingReference,
    -- | The @(:*:)@ constructor, which pairs two fields.
    genericProduct :: !DerivingReference,
    -- | The @V1@ type of a datatype without constructors.
    genericV1Type :: !DerivingReference,
    -- | The @U1@ type.
    genericU1Type :: !DerivingReference,
    -- | The @(:+:)@ type of a choice between constructors.
    genericSumType :: !DerivingReference,
    -- | The @(:*:)@ type of a pair of fields.
    genericProductType :: !DerivingReference,
    -- | The @D1@ type, which carries the metadata of the datatype.
    genericD1Type :: !DerivingReference,
    -- | The @C1@ type, which carries the metadata of one constructor.
    genericC1Type :: !DerivingReference,
    -- | The @S1@ type, which carries the metadata of one field.
    genericS1Type :: !DerivingReference,
    -- | The @Rec0@ type of a field that is not the datatype parameter.
    genericRec0Type :: !DerivingReference,
    -- | The @MetaData@ constructor, promoted into the metadata of @D1@.
    genericMetaData :: !DerivingReference,
    -- | The @MetaCons@ constructor, promoted into the metadata of @C1@.
    genericMetaCons :: !DerivingReference,
    -- | The @MetaSel@ constructor, promoted into the metadata of @S1@.
    genericMetaSel :: !DerivingReference,
    -- | @PrefixI@, the fixity of a constructor written prefix.
    genericPrefixI :: !DerivingReference,
    -- | @InfixI@, the fixity of a constructor written infix.
    genericInfixI :: !DerivingReference,
    -- | @LeftAssociative@.
    genericLeftAssociative :: !DerivingReference,
    -- | @RightAssociative@.
    genericRightAssociative :: !DerivingReference,
    -- | @NotAssociative@.
    genericNotAssociative :: !DerivingReference,
    -- | @NoSourceUnpackedness@, a field without an unpack pragma.
    genericNoSourceUnpackedness :: !DerivingReference,
    -- | @SourceNoUnpack@, a field marked @{-\# NOUNPACK \#-}@.
    genericSourceNoUnpack :: !DerivingReference,
    -- | @SourceUnpack@, a field marked @{-\# UNPACK \#-}@.
    genericSourceUnpack :: !DerivingReference,
    -- | @NoSourceStrictness@, a field without a strictness mark.
    genericNoSourceStrictness :: !DerivingReference,
    -- | @SourceLazy@, a field marked @~@.
    genericSourceLazy :: !DerivingReference,
    -- | @SourceStrict@, a field marked @!@.
    genericSourceStrict :: !DerivingReference,
    -- | @DecidedLazy@, a field the compiler leaves lazy.
    genericDecidedLazy :: !DerivingReference,
    -- | @DecidedStrict@, a field the compiler makes strict.
    genericDecidedStrict :: !DerivingReference,
    -- | @DecidedUnpack@, a field the compiler unpacks.
    genericDecidedUnpack :: !DerivingReference
  }
  deriving (Eq, Show)

-- | The names of a derived @Generic@ body that stand in a term position, so
-- that the generator can check that the module has them.
genericTermReferences :: [GenericReferences -> DerivingReference]
genericTermReferences =
  [genericM1, genericUnM1, genericK1, genericUnK1, genericU1, genericL1, genericR1, genericProduct]

-- | Every name of 'GenericReferences', in every namespace.
genericReferenceList :: GenericReferences -> [DerivingReference]
genericReferenceList references =
  map ($ references) (genericTermReferences <> types <> promoted)
  where
    types =
      [ genericV1Type,
        genericU1Type,
        genericSumType,
        genericProductType,
        genericD1Type,
        genericC1Type,
        genericS1Type,
        genericRec0Type
      ]
    promoted =
      [ genericMetaData,
        genericMetaCons,
        genericMetaSel,
        genericPrefixI,
        genericInfixI,
        genericLeftAssociative,
        genericRightAssociative,
        genericNotAssociative,
        genericNoSourceUnpackedness,
        genericSourceNoUnpack,
        genericSourceUnpack,
        genericNoSourceStrictness,
        genericSourceLazy,
        genericSourceStrict,
        genericDecidedLazy,
        genericDecidedStrict,
        genericDecidedUnpack
      ]

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
    derivingLexemePunc references,
    derivingPure references,
    derivingApply references,
    derivingLiftConE references,
    derivingLiftAppE references,
    derivingLiftDataConName references,
    derivingLiftCodeCoerce references
  ]
    <> concatMap unliftedFieldReferenceList (derivingUnliftedFields references)
    <> genericReferenceList (derivingGeneric references)
