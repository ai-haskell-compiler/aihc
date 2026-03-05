{-# LANGUAGE LambdaCase #-}

module Parser.Ast
  ( ArithSeq (..),
    CallConv (..),
    CaseAlt (..),
    CompStmt (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    ForeignDirection (..),
    ForeignSafety (..),
    Module (..),
  )
where

import Data.Text (Text)
import GHC.Hs (GhcPs, HsDecl)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)

data Module = Module
  { moduleName :: Maybe Text,
    moduleDecls :: [Decl]
  }
  deriving (Eq)

instance Show Module where
  show modu = "Module " <> show (moduleName modu, moduleDecls modu)

data Decl
  = Decl
      { declName :: Text,
        declExpr :: Expr
      }
  | PatternDecl
      { patternLhs :: Text
      }
  | TypeSigDecl
      { typeSigName :: Text
      }
  | FunctionDecl
      { functionName :: Text
      }
  | TypeDecl
      { typeName :: Text
      }
  | DataDecl
      { dataTypeName :: Text,
        dataConstructors :: [Text]
      }
  | NewtypeDecl
      { newtypeName :: Text,
        newtypeConstructor :: Maybe Text
      }
  | ClassDecl
      { className :: Text
      }
  | InstanceDecl
      { instanceClassName :: Text
      }
  | FixityDecl
      { fixityAssoc :: Text,
        fixityPrecedence :: Maybe Int,
        fixityOperator :: Text
      }
  | DefaultDecl
      { defaultTypes :: [Text]
      }
  | ForeignDecl
      { foreignDirection :: ForeignDirection,
        foreignCallConv :: CallConv,
        foreignSafety :: Maybe ForeignSafety,
        foreignEntity :: Maybe Text,
        foreignName :: Text
      }
  | GhcDecl
      { ghcDecl :: HsDecl GhcPs
      }

instance Eq Decl where
  lhs == rhs =
    case (lhs, rhs) of
      (Decl n1 e1, Decl n2 e2) -> n1 == n2 && e1 == e2
      (PatternDecl l1, PatternDecl l2) -> l1 == l2
      (TypeSigDecl n1, TypeSigDecl n2) -> n1 == n2
      (FunctionDecl n1, FunctionDecl n2) -> n1 == n2
      (TypeDecl n1, TypeDecl n2) -> n1 == n2
      (DataDecl t1 c1, DataDecl t2 c2) -> t1 == t2 && c1 == c2
      (NewtypeDecl n1 c1, NewtypeDecl n2 c2) -> n1 == n2 && c1 == c2
      (ClassDecl c1, ClassDecl c2) -> c1 == c2
      (InstanceDecl c1, InstanceDecl c2) -> c1 == c2
      (FixityDecl a1 p1 o1, FixityDecl a2 p2 o2) -> a1 == a2 && p1 == p2 && o1 == o2
      (DefaultDecl t1, DefaultDecl t2) -> t1 == t2
      (ForeignDecl d1 c1 s1 e1 n1, ForeignDecl d2 c2 s2 e2 n2) ->
        d1 == d2 && c1 == c2 && s1 == s2 && e1 == e2 && n1 == n2
      (GhcDecl d1, GhcDecl d2) -> showSDocUnsafe (ppr d1) == showSDocUnsafe (ppr d2)
      _ -> False

instance Show Decl where
  show = \case
    Decl n e -> "Decl " <> show (n, e)
    PatternDecl lhs -> "PatternDecl " <> show lhs
    TypeSigDecl n -> "TypeSigDecl " <> show n
    FunctionDecl n -> "FunctionDecl " <> show n
    TypeDecl n -> "TypeDecl " <> show n
    DataDecl t cs -> "DataDecl " <> show (t, cs)
    NewtypeDecl n c -> "NewtypeDecl " <> show (n, c)
    ClassDecl n -> "ClassDecl " <> show n
    InstanceDecl n -> "InstanceDecl " <> show n
    FixityDecl a p o -> "FixityDecl " <> show (a, p, o)
    DefaultDecl ts -> "DefaultDecl " <> show ts
    ForeignDecl d c s e n -> "ForeignDecl " <> show (d, c, s, e, n)
    GhcDecl d -> "GhcDecl " <> showSDocUnsafe (ppr d)

data ForeignDirection
  = ForeignImport
  | ForeignExport
  deriving (Eq, Show)

data CallConv
  = CCall
  | StdCall
  deriving (Eq, Show)

data ForeignSafety
  = Safe
  | Unsafe
  deriving (Eq, Show)

data Expr
  = EVar Text
  | EInt Integer
  | EFloat Double
  | EChar Char
  | EString Text
  | EIf Expr Expr Expr
  | ELambda [Text] Expr
  | EInfix Expr Text Expr
  | ENegate Expr
  | ESectionL Expr Text
  | ESectionR Text Expr
  | ELet [(Text, Expr)] Expr
  | ECase Expr [CaseAlt]
  | EDo [DoStmt]
  | EListComp Expr [CompStmt]
  | EArithSeq ArithSeq
  | ERecordCon Text [(Text, Expr)]
  | ERecordUpd Expr [(Text, Expr)]
  | ETypeSig Expr Text
  | EList [Expr]
  | ETuple [Expr]
  | ETupleCon Int
  | EApp Expr Expr
  deriving (Eq, Show)

data CaseAlt = CaseAlt
  { caseAltPattern :: Text,
    caseAltExpr :: Expr
  }
  deriving (Eq, Show)

data DoStmt
  = DoBind Text Expr
  | DoLet [(Text, Expr)]
  | DoExpr Expr
  deriving (Eq, Show)

data CompStmt
  = CompGen Text Expr
  | CompGuard Expr
  | CompLet [(Text, Expr)]
  deriving (Eq, Show)

data ArithSeq
  = ArithSeqFrom Expr
  | ArithSeqFromThen Expr Expr
  | ArithSeqFromTo Expr Expr
  | ArithSeqFromThenTo Expr Expr Expr
  deriving (Eq, Show)
