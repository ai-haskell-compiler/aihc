{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | The type checker configuration of the aihc core libraries.
--
-- The type checker knows no library. Every fact it needs about one -- the
-- type constructors that built-in syntax denotes, the names that generated
-- deriving code mentions, the classes stock deriving writes code for --
-- reaches it as a table in its 'TcConfig'. This module holds those tables
-- for @aihc-prim@ and the core libraries built on it, so that the layout of
-- those packages is stated in one place outside the type checker.
--
-- This is not a library component of any package. The compiler and the
-- type checker test suites each compile this directory in place, so the
-- wiring is shared without becoming part of a released interface.
module Aihc.Prim.Wiring
  ( primTcConfig,
    primTcWiring,
    primDerivingReferences,
    boxedTupleTyConName,
    boxedTupleDataConName,
    unboxedTupleTyConName,
    constraintTupleTyConName,
    unboxedSumTyConName,
    unboxedSumDataConName,
  )
where

import Aihc.Parser.Syntax (NameType (..))
import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Aihc.Tc
  ( DerivingReference (..),
    DerivingReferences (..),
    GenericReferences (..),
    ReferencePackage (..),
    StockClassLocation (..),
    TcConfig,
    TcWiring (..),
    TyCon,
    mkTcConfig,
    mkTyConWithNamespace,
  )
import Aihc.Tc.TypeLitFamily (typeLitFamilyModules)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | The configuration of a compiler whose core libraries are the aihc ones,
-- given the identity of the primitive package.
primTcConfig :: PackageId -> TcConfig
primTcConfig prim =
  mkTcConfig prim (primDerivingReferences prim) (primTcWiring prim)

-- | The names that the aihc core libraries give the type checker.
--
-- @aihc-prim@ declares the boxed tuples in @GHC.Tuple@ under GHC\'s current
-- names -- @Unit@, @Solo@, @Tuple2@, and so on -- and the unboxed ones in
-- @GHC.Types@, which also holds the list, the arrow, the kinds, and the
-- equality constraint. The unlifted primitives live in @GHC.Prim@, the
-- implicit-parameter constraints in @GHC.Classes@, and the application
-- operator in @GHC.Base@. The Template Haskell @Lift@ class lives in
-- aihc-internal, the standin for ghc-internal, as it does in GHC 9.12
-- and later.
--
-- The @Natural@ that is the kind of a type-level natural literal is
-- declared in @GHC.Prim.Natural@, beside @Integer@, rather than in the
-- @GHC.Num.Natural@ where GHC's @ghc-bignum@ puts it: everything the
-- compiler is built on is an asset of the primitive package.
primTcWiring :: PackageId -> TcWiring
primTcWiring prim =
  TcWiring
    { tcWiringBoxedTupleTyCon = \arity ->
        tyCon ResolutionNamespaceType "GHC.Tuple" (boxedTupleTyConName arity) arity,
      tcWiringBoxedTupleDataCon = \arity ->
        tyCon ResolutionNamespaceTerm "GHC.Tuple" (boxedTupleDataConName arity) arity,
      tcWiringUnboxedTupleTyCon = unboxedTuple ResolutionNamespaceType,
      tcWiringUnboxedTupleDataCon = unboxedTuple ResolutionNamespaceTerm,
      tcWiringUnboxedSumTyCon = \arity ->
        types ResolutionNamespaceType (unboxedSumTyConName arity) arity,
      tcWiringUnboxedSumDataCon = \alternative arity ->
        types ResolutionNamespaceTerm (unboxedSumDataConName alternative arity) 1,
      tcWiringListTyCon = types ResolutionNamespaceType "[]" 1,
      tcWiringListDeclaration = types ResolutionNamespaceType "List" 1,
      tcWiringNilDataCon = types ResolutionNamespaceTerm "[]" 0,
      tcWiringConsDataCon = types ResolutionNamespaceTerm ":" 2,
      tcWiringArrowTyCon = types ResolutionNamespaceType "(->)" 2,
      tcWiringTypeTyCon = types ResolutionNamespaceType "Type" 0,
      tcWiringConstraintTyCon = types ResolutionNamespaceType "Constraint" 0,
      tcWiringConstraintTupleTyCon = \arity ->
        tyCon ResolutionNamespaceType "GHC.Classes" (constraintTupleTyConName arity) arity,
      tcWiringBoolTyCon = types ResolutionNamespaceType "Bool" 0,
      tcWiringCharTyCon = types ResolutionNamespaceType "Char" 0,
      tcWiringNaturalTyCon = tyCon ResolutionNamespaceType "GHC.Prim.Natural" "Natural" 0,
      tcWiringSymbolTyCon = types ResolutionNamespaceType "Symbol" 0,
      tcWiringAnyTyCon = types ResolutionNamespaceType "Any" 0,
      tcWiringEqualityTyCon = types ResolutionNamespaceType "~" 2,
      tcWiringCoercibleTyCon = types ResolutionNamespaceType "Coercible" 2,
      tcWiringImplicitParamTyCon = \name ->
        tyCon ResolutionNamespaceType "GHC.Classes" name 1,
      tcWiringPrimitiveTyCon = \name ->
        tyCon ResolutionNamespaceType "GHC.Prim" name 0,
      tcWiringPrimitiveTerm = (prim,"GHC.Prim",),
      tcWiringRestrictedPrimitiveTerms = Set.singleton (prim, "GHC.Prim", "seq"),
      tcWiringKindTyCon = types ResolutionNamespaceType,
      tcWiringKindDataCon = types ResolutionNamespaceTerm,
      tcWiringTypeLitFamilyModules = typeLitFamilyModules,
      tcWiringTypeErrorFamily = ("GHC.TypeError", "TypeError"),
      tcWiringErrorMessageCons = ("Text", "ShowType", ":<>:", ":$$:"),
      tcWiringLiftClass = ("GHC.Internal.TH.Lift", "Lift")
    }
  where
    unboxedTuple namespace arity =
      types namespace (unboxedTupleTyConName arity) arity
    types namespace = tyCon namespace "GHC.Types"
    tyCon :: ResolutionNamespace -> Text -> Text -> Int -> TyCon
    tyCon namespace = mkTyConWithNamespace namespace prim

-- | The name of the boxed tuple of one arity.
boxedTupleTyConName :: Int -> Text
boxedTupleTyConName arity =
  case arity of
    0 -> "Unit"
    1 -> "Solo"
    _ -> "Tuple" <> T.pack (show arity)

-- | The name of the boxed tuple data constructor of one arity, as
-- @GHC.Tuple@ declares it: @()@, @MkSolo@, @(,)@ and so on.
boxedTupleDataConName :: Int -> Text
boxedTupleDataConName arity =
  case arity of
    1 -> "MkSolo"
    _ -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"

-- | The name of the unboxed tuple of one arity. The data constructor takes
-- the name of its type constructor: the comma spelling cannot tell the
-- empty tuple from the one-element one, because neither holds a comma, and
-- GHC only separates them by spelling the empty one @(# #)@, whose space an
-- FC name cannot hold. The FC desugarer and the GRIN lowering spell the
-- same name.
unboxedTupleTyConName :: Int -> Text
unboxedTupleTyConName arity = "Tuple" <> T.pack (show arity) <> "#"

-- | The class in @GHC.Classes@ that is the constraint tuple of one arity.
constraintTupleTyConName :: Int -> Text
constraintTupleTyConName arity = "CTuple" <> T.pack (show arity)

-- | The primitive declaration names the unboxed sum type.
unboxedSumTyConName :: Int -> Text
unboxedSumTyConName arity = "Sum" <> T.pack (show arity) <> "#"

-- | Each alternative has a distinct name that System FC can print.
unboxedSumDataConName :: Int -> Int -> Text
unboxedSumDataConName alternative arity =
  "Sum" <> T.pack (show arity) <> "_" <> T.pack (show alternative) <> "#"

-- | The deriving-reference table of the aihc core libraries, given the
-- identity of the @aihc-prim@ package.
--
-- Most generated bodies name values of the primitive package, which every
-- module can see. The Template Haskell names of a derived @Lift@ are the
-- exception: they live beside the class, in the package a module that
-- derives @Lift@ already depends on. Each of them is exported by
-- @Language.Haskell.TH.Syntax@, which such a module imports, because a
-- derived body builds the expression from the constructors of @Exp@ rather
-- than from the combinators of @GHC.Internal.TH.Lib@, which it would have
-- no reason to import.
primDerivingReferences :: PackageId -> DerivingReferences
primDerivingReferences prim =
  DerivingReferences
    { derivingTrue = term "GHC.Types" NameConId "True",
      derivingFalse = term "GHC.Types" NameConId "False",
      derivingLT = term "GHC.Types" NameConId "LT",
      derivingEQ = term "GHC.Types" NameConId "EQ",
      derivingGT = term "GHC.Types" NameConId "GT",
      derivingIntCon = term "GHC.Types" NameConId "I#",
      derivingIntPrimType = DerivingReference ReferencePrimPackage "GHC.Prim" "Int#" NameConId ResolutionNamespaceType,
      derivingGreaterOrEqual = term "GHC.Classes" NameVarSym ">=",
      derivingCons = term "GHC.Types" NameConSym ":",
      derivingBind = term "GHC.Prim.Base" NameVarSym ">>=",
      derivingThen = term "GHC.Prim.Base" NameVarSym ">>",
      derivingReturn = term "GHC.Prim.Base" NameVarId "return",
      derivingReadParens = term readModule NameVarId "parens",
      derivingReadPrecContext = term readModule NameVarId "prec",
      derivingReadStep = term readModule NameVarId "step",
      derivingReadReset = term readModule NameVarId "reset",
      derivingReadAlternative = term readModule NameVarSym "+++",
      derivingReadFail = term readModule NameVarId "pfail",
      derivingReadExpect = term readModule NameVarId "expectP",
      derivingReadField = term readModule NameVarId "readField",
      derivingReadSymField = term readModule NameVarId "readSymField",
      derivingLexemeIdent = term readModule NameConId "Ident",
      derivingLexemeSymbol = term readModule NameConId "Symbol",
      derivingLexemePunc = term readModule NameConId "Punc",
      derivingPure = term "GHC.Prim.Base" NameVarId "pure",
      derivingApply = term "GHC.Prim.Base" NameVarSym "<*>",
      derivingLiftConE = classTerm thSyntaxModule NameConId "ConE",
      derivingLiftAppE = classTerm thSyntaxModule NameConId "AppE",
      derivingLiftDataConName = classTerm thSyntaxModule NameVarId "mkNameG_d",
      derivingLiftCodeCoerce = classTerm thSyntaxModule NameVarId "unsafeCodeCoerce",
      derivingGeneric = genericReferences,
      derivingStockClasses = coreStockClasses prim,
      derivingRecognizedClasses = coreRecognizedClasses
    }
  where
    readModule = "GHC.Prim.Read"
    thSyntaxModule = "GHC.Internal.TH.Syntax"
    term moduleName nameType name =
      DerivingReference ReferencePrimPackage moduleName name nameType ResolutionNamespaceTerm
    -- The Template Haskell helpers live beside the Lift class, in a package
    -- whose identity this table cannot name.
    classTerm moduleName nameType name =
      DerivingReference ReferenceClassPackage moduleName name nameType ResolutionNamespaceTerm

-- | The names of @GHC.Generics@ that a derived @Generic@ instance is built
-- from. They sit beside the class, so the table cannot name their package.
genericReferences :: GenericReferences
genericReferences =
  GenericReferences
    { genericM1 = constructor "M1",
      genericUnM1 = value "unM1",
      genericK1 = constructor "K1",
      genericUnK1 = value "unK1",
      genericU1 = constructor "U1",
      genericL1 = constructor "L1",
      genericR1 = constructor "R1",
      genericProduct = operator ":*:",
      genericV1Type = tyCon "V1",
      genericU1Type = tyCon "U1",
      genericSumType = tyOperator ":+:",
      genericProductType = tyOperator ":*:",
      genericD1Type = tyCon "D1",
      genericC1Type = tyCon "C1",
      genericS1Type = tyCon "S1",
      genericRec0Type = tyCon "Rec0",
      genericMetaData = constructor "MetaData",
      genericMetaCons = constructor "MetaCons",
      genericMetaSel = constructor "MetaSel",
      genericPrefixI = constructor "PrefixI",
      genericInfixI = constructor "InfixI",
      genericLeftAssociative = constructor "LeftAssociative",
      genericRightAssociative = constructor "RightAssociative",
      genericNotAssociative = constructor "NotAssociative",
      genericNoSourceUnpackedness = constructor "NoSourceUnpackedness",
      genericSourceNoUnpack = constructor "SourceNoUnpack",
      genericSourceUnpack = constructor "SourceUnpack",
      genericNoSourceStrictness = constructor "NoSourceStrictness",
      genericSourceLazy = constructor "SourceLazy",
      genericSourceStrict = constructor "SourceStrict",
      genericDecidedLazy = constructor "DecidedLazy",
      genericDecidedStrict = constructor "DecidedStrict",
      genericDecidedUnpack = constructor "DecidedUnpack"
    }
  where
    genericsModule = "GHC.Generics"
    reference nameType namespace name =
      DerivingReference ReferenceClassPackage genericsModule name nameType namespace
    constructor = reference NameConId ResolutionNamespaceTerm
    operator = reference NameConSym ResolutionNamespaceTerm
    value = reference NameVarId ResolutionNamespaceTerm
    tyCon = reference NameConId ResolutionNamespaceType
    tyOperator = reference NameConSym ResolutionNamespaceType

-- | The stock classes that the aihc core libraries declare in the primitive
-- package, where each is defined. GHC keeps the same list as known-key
-- names, which carry a unit id; the package here plays that part.
--
-- A class of another core-library package is located without one, because
-- the identity of that package is not known until it is compiled against.
coreStockClasses :: PackageId -> [StockClassLocation]
coreStockClasses prim =
  [ primClass "GHC.Classes" "Eq",
    primClass "GHC.Classes" "Ord",
    primClass "GHC.Prim.Enum" "Enum",
    primClass "GHC.Prim.Enum" "Bounded",
    primClass "GHC.Prim.Show" "Show",
    primClass "GHC.Prim.Read" "Read",
    primClass "GHC.Prim.Base" "Functor",
    coreClass "GHC.Internal.Foldable" "Foldable",
    coreClass "GHC.Internal.Traversable" "Traversable",
    coreClass "GHC.Internal.TH.Lift" "Lift",
    coreClass "GHC.Generics" "Generic"
  ]
  where
    primClass = StockClassLocation (Just prim)
    coreClass = StockClassLocation Nothing

-- | The stock classes of GHC that the core libraries declare outside the
-- primitive package. The generator writes no code for them, so they carry
-- no package.
coreRecognizedClasses :: [(Text, Text)]
coreRecognizedClasses =
  [ ("GHC.Ix", "Ix"),
    ("Data.Data", "Data"),
    ("Type.Reflection", "Typeable"),
    ("Type.Reflection.Internal", "Typeable"),
    ("GHC.Generics", "Generic1")
  ]
