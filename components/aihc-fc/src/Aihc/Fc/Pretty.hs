{-# LANGUAGE OverloadedStrings #-}

-- | Pretty-printer for System FC core.
--
-- Produces multi-line, nested output with unicode characters:
--
-- * \x03bb (\955) for term lambda
-- * \x039b (\923) for type lambda
-- * \x2192 (\8594) for function arrows
-- * \x2200 (\8704) for forall
-- * \x21d2 (\8658) for double arrow (=>)
-- * \x25b7 (\9655) for cast
module Aihc.Fc.Pretty
  ( renderProgram,
    renderExpr,
    renderType,
    renderTopBind,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..))
import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as T

-- | Unicode characters for pretty-printing.
lamChar, bigLamChar, arrowChar, forallChar, fatArrowChar, castChar :: Char
lamChar = '\x03bb' -- λ
bigLamChar = '\x039b' -- Λ
arrowChar = '\x2192' -- →
forallChar = '\x2200' -- ∀
fatArrowChar = '\x21d2' -- ⇒
castChar = '\x25b7' -- ▷

-- | Render a complete program.
renderProgram :: FcProgram -> String
renderProgram prog =
  intercalate "\n\n" (map renderTopBind (fcTopBinds prog))

-- | Render a top-level binding.
renderTopBind :: FcTopBind -> String
renderTopBind (FcData tyName tyVars cons) =
  "data "
    ++ T.unpack tyName
    ++ concatMap (\tv -> " " ++ T.unpack (tvName tv)) tyVars
    ++ renderDataCons cons
renderTopBind (FcTopBind bind) = renderBind 0 bind

-- | Render data constructors.
renderDataCons :: [(Text, [TcType])] -> String
renderDataCons [] = ""
renderDataCons cons =
  "\n" ++ intercalate "\n" (zipWith renderOne (" = " : repeat " | ") cons)
  where
    renderOne prefix (name, args) =
      prefix
        ++ T.unpack name
        ++ concatMap (\ty -> " " ++ renderTypePrec True ty) args

-- | Render a binding at a given indentation level.
renderBind :: Int -> FcBind -> String
renderBind n (FcNonRec v e) =
  indent n
    ++ renderVar v
    ++ " : "
    ++ renderType (varType v)
    ++ " =\n"
    ++ renderExprIndented (n + 2) e
renderBind n (FcRec binds) =
  indent n
    ++ "rec\n"
    ++ intercalate "\n" (map renderRecBind binds)
  where
    renderRecBind (v, e) =
      indent (n + 2)
        ++ renderVar v
        ++ " : "
        ++ renderType (varType v)
        ++ " =\n"
        ++ renderExprIndented (n + 4) e

-- | Render a single expression (for debugging).
renderExpr :: FcExpr -> String
renderExpr = renderExprPrec 0 False

-- | Render a variable name.
renderVar :: Var -> String
renderVar v = T.unpack (varName v)

-- | Render an expression at a given indentation level.
renderExprIndented :: Int -> FcExpr -> String
renderExprIndented n expr = indent n ++ renderExprPrec n False expr

-- | Render an expression with optional parenthesization.
renderExprPrec :: Int -> Bool -> FcExpr -> String
renderExprPrec _ _ (FcVar v) = renderVar v
renderExprPrec _ _ (FcLit lit) = renderLiteral lit
renderExprPrec n parens (FcApp f a) =
  paren parens $
    renderExprPrec n False f ++ " " ++ renderExprPrec n True a
renderExprPrec n parens (FcTyApp e ty) =
  paren parens $
    renderExprPrec n False e ++ " @" ++ renderTypePrec True ty
renderExprPrec n parens (FcLam v body) =
  paren parens $
    [lamChar]
      ++ renderVar v
      ++ " : "
      ++ renderType (varType v)
      ++ ".\n"
      ++ renderExprIndented (n + 2) body
renderExprPrec n parens (FcTyLam tv body) =
  paren parens $
    [bigLamChar]
      ++ T.unpack (tvName tv)
      ++ ".\n"
      ++ renderExprIndented (n + 2) body
renderExprPrec n parens (FcLet bind body) =
  paren parens $
    "let\n"
      ++ renderBind (n + 2) bind
      ++ "\n"
      ++ indent n
      ++ "in\n"
      ++ renderExprIndented (n + 2) body
renderExprPrec n parens (FcCase scrut binder resTy alts) =
  paren parens $
    "case "
      ++ renderExprPrec n False scrut
      ++ " as "
      ++ renderVar binder
      ++ " : "
      ++ renderType resTy
      ++ " of\n"
      ++ intercalate "\n" (map (renderAlt (n + 2)) alts)
renderExprPrec n parens (FcCast e _co) =
  paren parens $
    renderExprPrec n True e ++ " " ++ [castChar] ++ " <co>"

-- | Render a case alternative.
renderAlt :: Int -> FcAlt -> String
renderAlt n (FcAlt con binders rhs) =
  indent n
    ++ renderAltCon con
    ++ concatMap (\v -> " " ++ renderVar v) binders
    ++ " "
    ++ [arrowChar]
    ++ "\n"
    ++ renderExprIndented (n + 2) rhs

-- | Render an alternative constructor.
renderAltCon :: FcAltCon -> String
renderAltCon (DataAlt name) = T.unpack name
renderAltCon (LitAlt lit) = renderLiteral lit
renderAltCon DefaultAlt = "_"

-- | Render a literal.
renderLiteral :: Literal -> String
renderLiteral (LitInt i) = show i
renderLiteral (LitChar c) = show c
renderLiteral (LitString s) = show (T.unpack s)

-- | Render a type.
renderType :: TcType -> String
renderType = renderTypePrec False

-- | Render a type with optional parenthesization.
renderTypePrec :: Bool -> TcType -> String
renderTypePrec _ (TcTyVar tv) = T.unpack (tvName tv)
renderTypePrec _ (TcMetaTv _) = "?"
renderTypePrec _ (TcTyCon tc []) = T.unpack (tyConName tc)
renderTypePrec parens (TcTyCon tc args) =
  paren parens $
    unwords (T.unpack (tyConName tc) : map (renderTypePrec True) args)
renderTypePrec parens (TcFunTy a b) =
  paren parens $
    renderTypePrec True a ++ " " ++ [arrowChar] ++ " " ++ renderTypePrec False b
renderTypePrec parens (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in paren parens $
        [forallChar] ++ " " ++ unwords (map (T.unpack . tvName) (tv : tvs)) ++ ". " ++ renderTypePrec False inner
renderTypePrec parens (TcQualTy preds body) =
  paren parens $
    "(" ++ unwords (map renderPred preds) ++ ") " ++ [fatArrowChar] ++ " " ++ renderTypePrec False body
renderTypePrec parens (TcAppTy f a) =
  paren parens $
    renderTypePrec True f ++ " " ++ renderTypePrec True a

-- | Collect nested forall binders.
collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)

-- | Render a predicate.
renderPred :: Pred -> String
renderPred (ClassPred cls args) =
  T.unpack cls ++ " " ++ unwords (map (renderTypePrec True) args)
renderPred (EqPred t1 t2) =
  renderTypePrec True t1 ++ " ~ " ++ renderTypePrec True t2

-- | Parenthesize if needed.
paren :: Bool -> String -> String
paren False s = s
paren True s = "(" ++ s ++ ")"

-- | Produce indentation.
indent :: Int -> String
indent n = replicate n ' '
