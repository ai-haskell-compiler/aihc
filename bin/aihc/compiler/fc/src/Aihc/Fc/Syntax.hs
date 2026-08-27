-- | System FC abstract syntax.
module Aihc.Fc.Syntax
  ( Type (..),
    Binder (..),
    Expr (..),
    Bind (..),
    Alt (..),
    AltCon (..),
    Literal (..),
    Role (..),
    Coercion (..),
    Program (..),
    Decl (..),
    TypeDecl (..),
    ConDecl (..),
    SynonymDecl (..),
    AxiomDecl (..),
    ValDecl (..),
    ForeignImportDecl (..),
    CallingConvention (..),
    CCallSpec (..),
    CAbiType (..),
    ForeignEffect (..),
  )
where

import Aihc.Fc.Name
import Data.ByteString (ByteString)
import Data.Text (Text)

-- | A type. Kinds are types.
data Type
  = TyVar Name
  | TyCon Name
  | TyApp Type Type
  | -- | @FUN r1 r2 a b@.
    TyFun Type Type Type Type
  | TyForAll Binder Type
  | TyEq Type Type
  deriving (Eq, Ord, Show, Read)

data Binder = Binder
  { binderName :: Name,
    binderType :: Type
  }
  deriving (Eq, Ord, Show, Read)

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
  deriving (Eq, Ord, Show, Read)

data Bind = Bind
  { bindBinder :: Binder,
    bindRhs :: Expr
  }
  deriving (Eq, Ord, Show, Read)

data Alt = Alt
  { altCon :: AltCon,
    altTypeBinders :: [Binder],
    altBinders :: [Binder],
    altRhs :: Expr
  }
  deriving (Eq, Ord, Show, Read)

data AltCon
  = AltData Name
  | AltLit Literal
  | AltDefault
  deriving (Eq, Ord, Show, Read)

-- | A literal. Integer, character, and address store the representation type.
data Literal
  = LitInt Type Integer
  | LitChar Type Char
  | LitAddr Type ByteString
  deriving (Eq, Ord, Show, Read)

data Role
  = Nominal
  | Representational
  | Phantom
  deriving (Eq, Ord, Show, Read)

data Coercion
  = CoVar Name
  | CoRefl Type
  | CoSym Coercion
  | CoTrans Coercion Coercion
  | CoTyConApp Name [Coercion]
  | CoAxiom Name [Type]
  deriving (Eq, Ord, Show, Read)

data Program = Program
  { programScopes :: ScopeTable,
    programDecls :: [Decl]
  }
  deriving (Eq, Ord, Show, Read)

data Decl
  = DeclType TypeDecl
  | DeclSynonym SynonymDecl
  | DeclAxiom AxiomDecl
  | DeclVal ValDecl
  | DeclForeignImport ForeignImportDecl
  deriving (Eq, Ord, Show, Read)

data TypeDecl = TypeDecl
  { typeVis :: Vis,
    typeName :: Name,
    typeBinders :: [Binder],
    typeResult :: Type,
    typeRoles :: [Role],
    typeCons :: [ConDecl]
  }
  deriving (Eq, Ord, Show, Read)

data ConDecl = ConDecl
  { conVis :: Vis,
    conName :: Name,
    conType :: Type
  }
  deriving (Eq, Ord, Show, Read)

data SynonymDecl = SynonymDecl
  { synVis :: Vis,
    synName :: Name,
    synBinders :: [Binder],
    synResult :: Type,
    synBody :: Type
  }
  deriving (Eq, Ord, Show, Read)

data AxiomDecl = AxiomDecl
  { axiomVis :: Vis,
    axiomName :: Name,
    axiomBinders :: [Binder],
    axiomRole :: Role,
    axiomLeft :: Type,
    axiomRight :: Type
  }
  deriving (Eq, Ord, Show, Read)

data ValDecl = ValDecl
  { valVis :: Vis,
    valName :: Name,
    valType :: Type,
    valBody :: Expr
  }
  deriving (Eq, Ord, Show, Read)

data ForeignImportDecl = ForeignImportDecl
  { foreignImportVis :: Vis,
    foreignImportName :: Name,
    foreignImportCallingConvention :: CallingConvention,
    foreignImportType :: Type
  }
  deriving (Eq, Ord, Show, Read)

data CallingConvention
  = Prim
  | CCall CCallSpec
  deriving (Eq, Ord, Show, Read)

data CCallSpec = CCallSpec
  { ccallSymbol :: Text,
    ccallArgumentTypes :: [CAbiType],
    ccallResultType :: CAbiType,
    ccallEffect :: ForeignEffect
  }
  deriving (Eq, Ord, Show, Read)

data CAbiType
  = CAbiInt
  | CAbiInt32
  | CAbiWord64
  | CAbiAddr
  deriving (Eq, Ord, Show, Read)

data ForeignEffect
  = ForeignPure
  | ForeignRealWorld
  deriving (Eq, Ord, Show, Read)
