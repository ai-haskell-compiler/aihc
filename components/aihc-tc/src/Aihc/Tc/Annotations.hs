{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Type checker annotations for AST nodes.
--
-- Following the pattern established by @aihc-resolve@, the type checker
-- attaches its results as 'Annotation' values on AST nodes using the
-- existing 'DeclAnn'/'EAnn'/'PAnn'/'TAnn' wrappers.
module Aihc.Tc.Annotations
  ( -- * Annotation type
    TcAnnotation (..),

    -- * Pattern synonyms for extracting annotations
    pattern ETcAnn,
    pattern DTcAnn,
    pattern PTcAnn,
    pattern TTcAnn,

    -- * Helpers
    annotateExpr,
    annotateDecl,

    -- * Pretty-printing
    renderTcType,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Pattern (..),
    Type (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Data.Typeable (Typeable)

-- | Annotation attached to AST nodes by the type checker.
--
-- Not every field is populated for every node. A variable reference gets
-- a type; a top-level binding gets the generalized scheme, etc.
newtype TcAnnotation = TcAnnotation
  { -- | The inferred/checked type of this node.
    tcAnnType :: TcType
  }
  deriving (Eq, Show, Typeable)

-- | Extract a 'TcAnnotation' from an 'Expr'.
pattern ETcAnn :: TcAnnotation -> Expr -> Expr
pattern ETcAnn ann inner <- EAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Decl'.
pattern DTcAnn :: TcAnnotation -> Decl -> Decl
pattern DTcAnn ann inner <- DeclAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Pattern'.
pattern PTcAnn :: TcAnnotation -> Pattern -> Pattern
pattern PTcAnn ann inner <- PAnn (fromAnnotation -> Just ann) inner

-- | Extract a 'TcAnnotation' from a 'Type'.
pattern TTcAnn :: TcAnnotation -> Type -> Type
pattern TTcAnn ann inner <- TAnn (fromAnnotation -> Just ann) inner

-- | Wrap an expression with a type annotation.
annotateExpr :: TcAnnotation -> Expr -> Expr
annotateExpr ann = EAnn (mkAnnotation ann)

-- | Wrap a declaration with a type annotation.
annotateDecl :: TcAnnotation -> Decl -> Decl
annotateDecl ann = DeclAnn (mkAnnotation ann)

-- | Render a 'TcType' as a human-readable string.
renderTcType :: TcType -> String
renderTcType = go False
  where
    go :: Bool -> TcType -> String
    go _ (TcTyVar tv) = show (tvName tv)
    go _ (TcMetaTv (Unique u)) = "?" ++ show u
    go _ (TcTyCon tc []) = show (tyConName tc)
    go parens (TcTyCon tc args) =
      paren parens $
        unwords (show (tyConName tc) : map (go True) args)
    go parens (TcFunTy a b) =
      paren parens $
        go True a ++ " -> " ++ go False b
    go parens (TcForAllTy tv body) =
      paren parens $
        "forall " ++ show (tvName tv) ++ ". " ++ go False body
    go parens (TcQualTy preds body) =
      paren parens $
        "(" ++ unwords (map showPred preds) ++ ") => " ++ go False body
    go parens (TcAppTy f a) =
      paren parens $
        go True f ++ " " ++ go True a

    showPred (ClassPred cls args) =
      show cls ++ " " ++ unwords (map (go True) args)
    showPred (EqPred t1 t2) =
      go True t1 ++ " ~ " ++ go True t2

    paren False s = s
    paren True s = "(" ++ s ++ ")"
