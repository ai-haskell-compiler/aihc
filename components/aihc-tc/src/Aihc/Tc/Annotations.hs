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
import Data.Text qualified as T

-- | Annotation attached to AST nodes by the type checker.
--
-- Not every field is populated for every node. A variable reference gets
-- a type; a top-level binding gets the generalized scheme, etc.
newtype TcAnnotation = TcAnnotation
  { -- | The inferred/checked type of this node.
    tcAnnType :: TcType
  }
  deriving (Eq, Show)

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
--
-- Uses a precedence level to decide when to insert parentheses:
--   0 = no parens needed (top level or right of ->)
--   1 = parens needed for function types (left of ->)
--   2 = parens needed for function types and type applications (inside type con args)
renderTcType :: TcType -> String
renderTcType = go 0
  where
    go :: Int -> TcType -> String
    go _ (TcTyVar tv) = T.unpack (tvName tv)
    go _ (TcMetaTv (Unique u)) = "?" ++ show u
    go _ (TcTyCon tc []) = T.unpack (tyConName tc)
    go p (TcTyCon tc args) =
      parenIf (p >= 2) $
        unwords (T.unpack (tyConName tc) : map (go 2) args)
    go p (TcFunTy a b) =
      parenIf (p >= 1) $
        go 1 a ++ " -> " ++ go 0 b
    go p (TcForAllTy tv body) =
      let (tvs, inner) = collectForAlls body
       in parenIf (p >= 1) $
            "forall " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ go 0 inner
    go p (TcQualTy preds body) =
      parenIf (p >= 1) $
        "(" ++ unwords (map showPred preds) ++ ") => " ++ go 0 body
    go p (TcAppTy f a) =
      parenIf (p >= 2) $
        go 1 f ++ " " ++ go 2 a

    showPred (ClassPred cls args) =
      T.unpack cls ++ " " ++ unwords (map (go 2) args)
    showPred (EqPred t1 t2) =
      go 2 t1 ++ " ~ " ++ go 2 t2

    parenIf False s = s
    parenIf True s = "(" ++ s ++ ")"

-- | Collect nested forall binders into a list.
collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)
