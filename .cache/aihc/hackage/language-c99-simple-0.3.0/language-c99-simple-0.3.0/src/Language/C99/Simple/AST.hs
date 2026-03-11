{- This module implements a simplified version of a C99 AST. It omits a lot of
 - specific and rarely used language constructs and features, which typically
 - are not used by code generators anyway. Some parts of the AST differ quite a
 - lot from the C99 one, and do not necessarily mimic their counterparts, even
 - if the names are similar.
 -
 - A total translation function proves that the simplified AST can be rewritten
 - in terms of the full AST, and thus is a subset.
-}

module Language.C99.Simple.AST where

import Prelude hiding (LT, GT)
import Data.List.NonEmpty (NonEmpty)

type Ident    = String

data TransUnit = TransUnit [Decln] [FunDef]

data FunDef = FunDef (Maybe StorageSpec) Type Ident [Param] [Decln] [Stmt]

data Param = Param Type Ident

data Decln = VarDecln (Maybe StorageSpec) Type Ident (Maybe Init)
           | FunDecln (Maybe StorageSpec) Type Ident [Param]
           | TypeDecln Type

data StorageSpec = Typedef
                 | Extern
                 | Static
                 | Auto
                 | Register

data Type = Type     Type
          | TypeSpec TypeSpec
          | Ptr      Type
          | Array    Type (Maybe Expr)

          | Const    Type
          | Restrict Type
          | Volatile Type

data TypeSpec = Void
              | Char
              | Signed_Char
              | Unsigned_Char

              | Short
              | Signed_Short
              | Short_Int
              | Signed_Short_Int

              | Unsigned_Short
              | Unsigned_Short_Int

              | Int
              | Signed
              | Signed_Int

              | Unsigned
              | Unsigned_Int

              | Long
              | Signed_Long
              | Long_Int
              | Signed_Long_Int

              | Unsigned_Long
              | Unsgined_Long_Int

              | Long_Long
              | Signed_Long_Long
              | Long_Long_Int
              | Signed_Long_Long_Int

              | Unsigned_Long_Long
              | Unsigned_Long_Long_Int

              | Float
              | Double
              | Long_Double
              | Bool
              | Float_Complex
              | Double_Complex
              | Long_Double_Complex
              | TypedefName Ident

              | Struct      Ident
              | StructDecln (Maybe Ident) (NonEmpty FieldDecln)

              | Union      Ident
              | UnionDecln (Maybe Ident) (NonEmpty FieldDecln)

              | Enum      Ident
              | EnumDecln (Maybe Ident) (NonEmpty Ident)

data FieldDecln = FieldDecln Type Ident

data Init = InitExpr Expr
          | InitList (NonEmpty InitItem)

data InitItem = InitItem (Maybe Ident) Init

data Expr = Ident     Ident
          | LitBool   Bool
          | LitInt    Integer
          | LitFloat  Float
          | LitDouble Double
          | LitString String

          | Index   Expr Expr
          | Funcall Expr [Expr]
          | Dot     Expr Ident
          | Arrow   Expr Ident
          | InitVal TypeName (NonEmpty InitItem)

          | UnaryOp UnaryOp Expr

          | Cast  TypeName Expr

          | BinaryOp BinaryOp Expr Expr

          | Cond Expr Expr Expr

          | AssignOp AssignOp Expr Expr

          | SizeOf     Expr
          | SizeOfType TypeName

data UnaryOp = Inc
             | Dec
             | Ref
             | DeRef
             | Plus
             | Min
             | BoolNot
             | Not

data BinaryOp = Mult
              | Div
              | Mod
              | Add
              | Sub
              | ShiftL
              | ShiftR
              | LT
              | GT
              | LE
              | GE
              | Eq
              | NEq
              | And
              | XOr
              | Or
              | LAnd
              | LOr

data AssignOp = Assign
              | AssignMult
              | AssignDiv
              | AssignMod
              | AssignAdd
              | AssignSub
              | AssignShiftL
              | AssignShiftR
              | AssignAnd
              | AssignXOr
              | AssignOr

data TypeName = TypeName Type

data Case = Case    Expr Stmt
          | Default      Stmt

data Stmt = Expr     Expr
          | If       Expr [Stmt]
          | IfElse   Expr [Stmt] [Stmt]
          | Switch   Expr [Case]
          | While    Expr [Stmt]
          | For      Expr Expr Expr [Stmt]
          | ForInf   [Stmt]
          | Continue
          | Break
          | Label    String Stmt
          | Return   (Maybe Expr)
