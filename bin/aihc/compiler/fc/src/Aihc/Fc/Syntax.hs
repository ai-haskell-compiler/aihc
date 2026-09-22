{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | System FC abstract syntax.
module Aihc.Fc.Syntax
  ( Type (..),
    TyLit (..),
    Binder (..),
    Expr (..),
    Bind (..),
    Alt (..),
    AltCon (..),
    Literal (..),
    Role (..),
    Coercion (..),
    Program (..),
    Imports (..),
    Decl (..),
    TypeDecl (..),
    ConDecl (..),
    ConRepresentation (..),
    SynonymDecl (..),
    AxiomDecl (..),
    ValDecl (..),
    InlineSpec (..),
    RuleDecl (..),
    RuleActivation (..),
    ForeignCall (..),
    ForeignImportDependency (..),
    CallingConvention (..),
    CCallSpec (..),
    CCallTarget (..),
    CAbiType (..),
    ForeignEffect (..),
    ForeignSafety (..),
  )
where

import Aihc.Fc.Name
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A type. Kinds are types.
data Type
  = TyVar Name
  | TyCon Name
  | TyApp Type Type
  | -- | @FUN r1 r2 a b@.
    TyFun Type Type Type Type
  | TyForAll Binder Type
  | TyEq Type Type
  | -- | A type-level literal, and the type constructor that is its kind:
    -- @Natural@, @Symbol@ or @Char@. It is a form of its own rather than
    -- a nullary 'TyCon' named after the value, so that no synthesized
    -- declaration has to stand behind each distinct literal and a
    -- consumer reads the value rather than parsing a name. The kind is
    -- named rather than derived so that a literal refers to its sort the
    -- way every other type refers to a constructor, and the scope,
    -- import and lint machinery needs no special case.
    TyLit Name TyLit
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | A type-level literal, by sort: a natural at kind @Natural@, a string
-- at kind @Symbol@ and a character at kind @Char@.
data TyLit
  = TyLitNat Integer
  | TyLitSymbol Text
  | TyLitChar Char
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Binder = Binder
  { binderName :: Name,
    binderType :: Type
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Expr
  = ExVar Name
  | ExLit Literal
  | ExApp Expr Expr
  | ExTyApp Expr Type
  | ExLam Binder Expr
  | ExTyLam Binder Expr
  | ExLet Bind Expr
  | ExRec [Bind] Expr
  | ExCase Expr Binder Type [Alt]
  | ExCast Expr Coercion
  | -- | Equality evidence has no runtime fields.
    ExCoercion Coercion
  | -- | A saturated call of a foreign import. The type arguments instantiate
    -- the leading binders of the foreign type. The value arguments fill every
    -- arrow of the foreign type.
    ExForeignCall ForeignCall [Type] [Expr]
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | The foreign import that a call names, with the facts that lower it.
data ForeignCall = ForeignCall
  { foreignCallName :: Name,
    foreignCallConvention :: CallingConvention,
    foreignCallDependencies :: [ForeignImportDependency],
    foreignCallType :: Type
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Bind = Bind
  { bindBinder :: Binder,
    bindRhs :: Expr
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Alt = Alt
  { altCon :: AltCon,
    altTypeBinders :: [Binder],
    altBinders :: [Binder],
    altRhs :: Expr
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data AltCon
  = AltData Name
  | AltLit Literal
  | AltDefault
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | A literal. Integer, character, and address store the representation type.
data Literal
  = LitInt Type Integer
  | LitChar Type Char
  | LitAddr Type ByteString
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Role
  = Nominal
  | Representational
  | Phantom
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Coercion
  = CoVar Name
  | CoRefl Type
  | CoSym Coercion
  | CoTrans Coercion Coercion
  | CoApp Coercion Coercion
  | CoFun Coercion Coercion
  | CoNth Int Coercion
  | CoTyConApp Name [Coercion]
  | CoAxiom Name [Type]
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Program = Program
  { programScopes :: ScopeTable,
    programImports :: Imports,
    programDecls :: [Decl]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Imports = Imports
  { importHeaders :: Map Name Type,
    importSynonyms :: Map Name Type,
    importAxioms :: Map Name AxiomDecl,
    importBinders :: Map Name Type,
    importConRepresentations :: Map Name ConRepresentation
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data Decl
  = DeclType TypeDecl
  | DeclSynonym SynonymDecl
  | DeclAxiom AxiomDecl
  | DeclVal ValDecl
  | DeclRule RuleDecl
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | A rewrite rule. The type binders and the value binders, which include
-- the dictionaries of the rule's constraints, are the pattern variables of
-- the left-hand side, and the two sides share the rule type. A rule
-- declares no name: it is kept while the head of its left-hand side is.
data RuleDecl = RuleDecl
  { ruleName :: Text,
    ruleActivation :: RuleActivation,
    ruleTypeBinders :: [Binder],
    ruleBinders :: [Binder],
    ruleType :: Type,
    ruleLhs :: Expr,
    ruleRhs :: Expr
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | The phases in which a rule fires, as the source pragma gives them.
data RuleActivation
  = AlwaysActive
  | -- | @[n]@: phase @n@ and later phases.
    ActiveAfter Int
  | -- | @[~n]@: the phases before phase @n@.
    ActiveBefore Int
  | -- | @[~]@: never.
    NeverActive
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data TypeDecl = TypeDecl
  { typeVis :: Vis,
    typeName :: Name,
    typeBinders :: [Binder],
    typeResult :: Type,
    typeRoles :: [Role],
    typeCons :: [ConDecl]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | Constructor semantics remain independent of the constructor name.
data ConRepresentation
  = HeapConstructor
  | UnboxedTupleConstructor
  | -- | The one-based alternative and the sum arity.
    UnboxedSumConstructor Int Int
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data ConDecl = ConDecl
  { conVis :: Vis,
    conName :: Name,
    conType :: Type,
    conRepresentation :: ConRepresentation
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data SynonymDecl = SynonymDecl
  { synVis :: Vis,
    synName :: Name,
    synBinders :: [Binder],
    synResult :: Type,
    synBody :: Type
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data AxiomDecl = AxiomDecl
  { axiomVis :: Vis,
    axiomName :: Name,
    axiomBinders :: [Binder],
    axiomRole :: Role,
    axiomLeft :: Type,
    axiomRight :: Type
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data ValDecl = ValDecl
  { valVis :: Vis,
    valName :: Name,
    valType :: Type,
    valBody :: Expr,
    -- | What the source said about inlining the value.
    valInline :: InlineSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | The @INLINE@, @INLINABLE@ or @NOINLINE@ pragma of a value, with the
-- phases in which inlining is allowed. Without a pragma the inliner
-- decides by its policy.
data InlineSpec
  = InlineDefault
  | -- | @INLINE@: copy the value at every saturated call in the phases the
    -- activation names, whatever the size, and never before them.
    InlineAlways RuleActivation
  | -- | @INLINABLE@: the usual policy, in the phases the activation names.
    InlineWhenUseful RuleActivation
  | -- | @NOINLINE@: no copy until the phases the activation names; a plain
    -- @NOINLINE@ names none.
    InlineNever RuleActivation
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data ForeignImportDependency
  = ForeignAxiom Name
  | ForeignConstructor Name
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data CallingConvention
  = Prim
  | CCall CCallSpec
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data CCallSpec = CCallSpec
  { ccallSymbol :: Text,
    ccallTarget :: CCallTarget,
    ccallSafety :: ForeignSafety,
    ccallArgumentTypes :: [CAbiType],
    ccallResultType :: CAbiType,
    ccallEffect :: ForeignEffect
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | What a C entity string names: a function to call, or a static symbol
-- whose address is the imported value (@foreign import ccall "&sym"@).
data CCallTarget
  = CCallFunction
  | CCallAddress
  | CCallDynamic
  | CCallWrapper
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data CAbiType
  = CAbiInt
  | CAbiInt8
  | CAbiInt16
  | CAbiInt32
  | CAbiInt64
  | CAbiWord
  | CAbiWord8
  | CAbiWord16
  | CAbiWord32
  | CAbiWord64
  | CAbiFloat
  | CAbiDouble
  | CAbiAddr
  | -- | The result of a C procedure, which has no value.
    CAbiVoid
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data ForeignEffect
  = ForeignPure
  | ForeignRealWorld
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

-- | Safety of a foreign call. The runtime is single-threaded, so every mark
-- is lowered the same way; the mark is kept for fidelity. An @interruptible@
-- call is a safe call whose blocked Haskell thread may receive an
-- asynchronous exception; with one thread nothing can raise it during the
-- call, so the call behaves as @safe@.
data ForeignSafety
  = ForeignUnsafe
  | ForeignSafe
  | ForeignInterruptible
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)
