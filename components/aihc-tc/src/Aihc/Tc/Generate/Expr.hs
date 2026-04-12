{-# LANGUAGE OverloadedStrings #-}

-- | Constraint generation for expressions.
--
-- This module implements bidirectional type inference/checking for the
-- surface expression language. It walks the surface AST and returns:
--
--   * An inferred type
--   * A list of wanted constraints (equalities)
--
-- The generated constraints are then passed to the solver.
module Aihc.Tc.Generate.Expr
  ( inferExpr,
  )
where

import Aihc.Parser.Syntax
  ( Expr (..),
    Name (..),
    SourceSpan (..),
    getSourceSpan,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Instantiate (instantiate)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the inferred type and a list of wanted constraints.
inferExpr :: Expr -> TcM (TcType, [Ct])
inferExpr expr = case expr of
  -- Variables: look up in environment, instantiate if polymorphic.
  EVar sp name -> inferVar sp (nameToText name)
  -- Integer literals: monomorphic Int for MVP.
  -- (Full version would use Num constraint.)
  EInt sp _ _ -> pure (intTyCon, [])
    where
      _ = sp
  EIntBase sp _ _ -> pure (intTyCon, [])
    where
      _ = sp
  -- Float literals.
  EFloat sp _ _ -> pure (doubleTyCon, [])
    where
      _ = sp
  -- Char literals.
  EChar sp _ _ -> pure (charTyCon, [])
    where
      _ = sp
  -- String literals.
  EString sp _ _ -> pure (stringTyCon, [])
    where
      _ = sp
  -- Lambda: \x -> body
  ELambdaPats sp pats body -> inferLambda sp pats body
  -- Application: f x
  EApp sp fun arg -> inferApp sp fun arg
  -- If-then-else
  EIf sp cond thenE elseE -> inferIf sp cond thenE elseE
  -- Let expression
  ELetDecls sp _decls body -> do
    -- MVP: infer body only (let bindings not yet processed).
    -- Full version would typecheck declarations and extend env.
    inferExpr body
    where
      _ = sp
  -- Parenthesized expression
  EParen _sp inner -> inferExpr inner
  -- Type signature: (e :: T)
  ETypeSig sp inner _ty -> do
    -- MVP: just infer the inner expression.
    -- Full version would check against the given type.
    inferExpr inner
    where
      _ = sp
  -- Negation
  ENegate sp inner -> do
    (innerTy, cs) <- inferExpr inner
    -- For MVP, just return the inner type.
    pure (innerTy, cs)
    where
      _ = sp
  -- Annotated expression (from other passes, e.g. resolve).
  EAnn _ann inner -> inferExpr inner
  -- Tuple
  ETuple sp _flavor elems -> inferTuple sp elems
  -- List
  EList sp elems -> inferList sp elems
  -- Unsupported expression forms for MVP.
  other -> do
    let sp = getSourceSpan other
    emitError sp (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (ty, [])

-- | Infer the type of a variable reference.
inferVar :: SourceSpan -> Text -> TcM (TcType, [Ct])
inferVar sp name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme) -> do
      (ty, preds) <- instantiate scheme
      cts <- mapM (predToCt sp name) preds
      pure (ty, cts)
    Just (TcMonoIdBinder _ ty) ->
      pure (ty, [])
    Nothing -> do
      emitError sp (UnboundVariable (T.unpack name))
      ty <- freshMetaTv
      pure (ty, [])

-- | Convert a predicate to a wanted constraint.
predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  pure $
    mkWantedCt p ev (OccurrenceOf name) sp

-- | Infer the type of a lambda expression.
inferLambda :: SourceSpan -> [a] -> Expr -> TcM (TcType, [Ct])
inferLambda _sp pats body = do
  -- Create a fresh meta-variable for each pattern.
  argTys <- mapM (const freshMetaTv) pats
  -- For MVP, we don't bind pattern variables (would need pattern analysis).
  -- Just infer the body with a fresh result type.
  (bodyTy, bodyCts) <- inferExpr body
  let funTy = foldr TcFunTy bodyTy argTys
  pure (funTy, bodyCts)

-- | Infer the type of a function application.
inferApp :: SourceSpan -> Expr -> Expr -> TcM (TcType, [Ct])
inferApp sp fun arg = do
  (funTy, funCts) <- inferExpr fun
  (argTy, argCts) <- inferExpr arg
  resTy <- freshMetaTv
  -- Emit equality: funTy ~ argTy -> resTy
  ev <- freshEvVar
  let eqCt = mkWantedCt (EqPred funTy (TcFunTy argTy resTy)) ev (AppOrigin sp) sp
  pure (resTy, funCts ++ argCts ++ [eqCt])

-- | Infer the type of if-then-else.
inferIf :: SourceSpan -> Expr -> Expr -> Expr -> TcM (TcType, [Ct])
inferIf sp cond thenE elseE = do
  (condTy, condCts) <- inferExpr cond
  (thenTy, thenCts) <- inferExpr thenE
  (elseTy, elseCts) <- inferExpr elseE
  -- Condition must be Bool.
  condEv <- freshEvVar
  let condCt = mkWantedCt (EqPred condTy boolTyCon) condEv (AppOrigin sp) sp
  -- Then and else branches must have the same type.
  branchEv <- freshEvVar
  let branchCt = mkWantedCt (EqPred thenTy elseTy) branchEv (AppOrigin sp) sp
  pure (thenTy, condCts ++ thenCts ++ elseCts ++ [condCt, branchCt])

-- | Infer the type of a tuple.
inferTuple :: SourceSpan -> [Maybe Expr] -> TcM (TcType, [Ct])
inferTuple _sp elems = do
  results <- mapM inferElem elems
  let tys = map fst results
  let cts = concatMap snd results
  -- Represent tuples as TcTyCon with a tuple type constructor.
  let n = length tys
  let tc = TyCon {tyConName = "(" <> T.replicate (n - 1) "," <> ")", tyConArity = n}
  pure (TcTyCon tc tys, cts)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (ty, [])
    inferElem (Just e) = inferExpr e

-- | Infer the type of a list literal.
inferList :: SourceSpan -> [Expr] -> TcM (TcType, [Ct])
inferList sp elems = case elems of
  [] -> do
    elemTy <- freshMetaTv
    pure (TcTyCon listTyCon' [elemTy], [])
  (e : es) -> do
    (headTy, headCts) <- inferExpr e
    results <- mapM inferExpr es
    let tailCts = concatMap snd results
    -- All elements must have the same type.
    eqCts <- mapM (mkElemEq headTy) results
    pure (TcTyCon listTyCon' [headTy], headCts ++ tailCts ++ eqCts)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
    mkElemEq headTy (elemTy, _) = do
      ev <- freshEvVar
      pure $ mkWantedCt (EqPred headTy elemTy) ev (AppOrigin sp) sp

-- | Convert a surface Name to Text for lookup.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

-- | Built-in type constructors (MVP).
intTyCon :: TcType
intTyCon = TcTyCon (TyCon "Int" 0) []

doubleTyCon :: TcType
doubleTyCon = TcTyCon (TyCon "Double" 0) []

charTyCon :: TcType
charTyCon = TcTyCon (TyCon "Char" 0) []

stringTyCon :: TcType
stringTyCon = TcTyCon (TyCon "String" 0) []

boolTyCon :: TcType
boolTyCon = TcTyCon (TyCon "Bool" 0) []
