{-# LANGUAGE OverloadedStrings #-}

-- | Move invariant computations outside function parameters.
module Aihc.Fc.FullLaziness (fullLazinessProgram) where

import Aihc.Fc.Imports (exprReferences, typeReferences)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, isConstructorName, isTrivial, maxLocalUnique, substExpr)
import Aihc.Fc.Size (isLiftedType)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), exprType, extendBinder, lookupBinderType, reduceType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types (Unique (..))
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, get, modify', runState)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

data FloatState = FloatState
  { floatNext :: !Int,
    floatTaken :: !(Set Name),
    floatDecls :: ![Decl],
    floatCount :: !Int,
    floatLocals :: !(Set Name)
  }

type FloatM = State FloatState

-- | Keep each computation lazy at its new scope.
-- Free term, type, and coercion names must remain in scope.
-- Explicit foreign effects and state-token operations prevent a move.
fullLazinessProgram :: PackageId -> Program -> (Program, Int)
fullLazinessProgram primPackage program =
  let env = typeEnvFromProgram primPackage program
      initial = FloatState (maxLocalUnique program + 1) (Map.keysSet (teHeaders env)) [] 0 Set.empty
      (decls, final) = runState (mapM (declaration env) (programDecls program)) initial
   in (tidyProgram program {programDecls = decls <> reverse (floatDecls final)}, floatCount final)

declaration :: TypeEnv -> Decl -> FloatM Decl
declaration env decl = case decl of
  DeclVal value -> do
    body <- walk env (valBody value)
    body' <- topFloats (valName value) body
    pure (DeclVal value {valBody = body'})
  _ -> pure decl

-- | Move a closed outer let to a private top-level thunk.
-- NOINLINE keeps a later module pass from copying the computation back.
topFloats :: Name -> Expr -> FloatM Expr
topFloats owner expression = do
  state <- get
  case expression of
    ExLet bind body
      | nameOf bind `Set.member` floatLocals state,
        all isTop (freeReferences (bindRhs bind) <> typeReferences (binderType (bindBinder bind))) -> do
          name <- freshTop owner
          let named = bindBinder bind
              decl = DeclVal (ValDecl Private name (binderType named) (bindRhs bind) (InlineNever NeverActive))
          modify' (\current -> current {floatDecls = decl : floatDecls current})
          topFloats owner (substExpr (Map.singleton (binderName named) (ExVar name)) body)
    _ -> pure expression
  where
    nameOf = binderName . bindBinder
    isTop name = case nameOrigin name of
      OriginTop {} -> True
      OriginLocal {} -> False

walk :: TypeEnv -> Expr -> FloatM Expr
walk env expr = case expr of
  ExLam {} -> boundary expr
  ExTyLam {} -> boundary expr
  _ -> fst <$> children (\inner _ body -> (\value -> (value, [])) <$> walk inner body) env Set.empty expr
  where
    -- Keep consecutive parameters together to preserve direct-call arity.
    boundary expression = do
      let (parameters, body) = lambdas expression
          inner = List.foldl' extendBinder env (map snd parameters)
          blocked = Set.fromList (map (binderName . snd) parameters)
      body' <- walk inner body
      (moved, binds) <- floatOut inner blocked body'
      pure (foldr ExLet (foldr rebuild moved parameters) binds)
    rebuild (True, binder) = ExTyLam binder
    rebuild (False, binder) = ExLam binder
    lambdas expression = case expression of
      ExLam binder body -> prepend (False, binder) (lambdas body)
      ExTyLam binder body -> prepend (True, binder) (lambdas body)
      _ -> ([], expression)
    prepend parameter (parameters, body) = (parameter : parameters, body)

floatOut :: TypeEnv -> Set Name -> Expr -> FloatM (Expr, [Bind])
floatOut env blocked expr = case candidateType env blocked expr of
  Just ty -> do
    name <- freshLocal
    modify' (\state -> state {floatCount = floatCount state + 1})
    pure (ExVar name, [Bind (Binder name ty) expr])
  Nothing -> children floatOut env blocked expr

-- | Extract maximal lifted computations that do not depend on this scope.
-- Functions and trivial expressions already need no computation thunk.
candidateType :: TypeEnv -> Set Name -> Expr -> Maybe Type
candidateType env blocked expr = do
  guard (not (isTrivial expr) && computation expr && pureExpr env expr)
  let refs = freeReferences expr
  guard (Set.disjoint blocked refs)
  guard (not (any (maybe False (stateType env) . lookupBinderType env) refs))
  ty <- exprType env expr
  guard (isLiftedType env ty && isNothing (viewFun env ty) && isNothing (viewForAll env ty))
  guard (Set.disjoint blocked (typeReferences ty))
  pure ty
  where
    computation ExLam {} = False
    computation ExTyLam {} = False
    computation expression = case collectSpine expression of
      (ExVar name, _) | isConstructorName name -> False
      _ -> True

-- | Visit children with every local binder in the exclusion set.
-- A float must not escape a let, recursive group, or case field.
children :: (TypeEnv -> Set Name -> Expr -> FloatM (Expr, [Bind])) -> TypeEnv -> Set Name -> Expr -> FloatM (Expr, [Bind])
children visit env blocked expr = case expr of
  ExApp function argument -> pair ExApp function argument
  ExTyApp function ty -> unary (`ExTyApp` ty) function
  ExCast body proof -> unary (`ExCast` proof) body
  ExAbsurd body ty -> unary (`ExAbsurd` ty) body
  ExLam {} -> pure (expr, [])
  ExTyLam {} -> pure (expr, [])
  ExLet bind body -> do
    (rhs, first) <- visit env blocked (bindRhs bind)
    (body', rest) <- visit (extendBinder env named) (Set.insert (binderName named) blocked) body
    pure (ExLet bind {bindRhs = rhs} body', first <> rest)
    where
      named = bindBinder bind
  ExRec binds body -> do
    let named = map bindBinder binds
        inner = List.foldl' extendBinder env named
        excluded = blocked <> Set.fromList (map binderName named)
    values <- mapM (visit inner excluded . bindRhs) binds
    (body', rest) <- visit inner excluded body
    pure (ExRec (zipWith (\bind (rhs, _) -> bind {bindRhs = rhs}) binds values) body', concatMap snd values <> rest)
  ExCase scrutinee binder alternatives -> do
    (scrutinee', first) <- visit env blocked scrutinee
    values <- mapM alternative alternatives
    pure (ExCase scrutinee' binder (fmap fst values), first <> foldMap snd values)
    where
      alternative alt = do
        let named = foldr (:) [] binder <> altTypeBinders alt <> altBinders alt
        (rhs, binds) <- visit (List.foldl' extendBinder env named) (blocked <> Set.fromList (map binderName named)) (altRhs alt)
        pure (alt {altRhs = rhs}, binds)
  ExForeignCall call types arguments -> do
    values <- mapM (visit env blocked) arguments
    pure (ExForeignCall call types (map fst values), concatMap snd values)
  _ -> pure (expr, [])
  where
    unary constructor body = do
      (body', binds) <- visit env blocked body
      pure (constructor body', binds)
    pair constructor left right = do
      (left', first) <- visit env blocked left
      (right', rest) <- visit env blocked right
      pure (constructor left' right', first <> rest)

-- | Remove a binder only from the expression that it scopes over.
-- Type and coercion references also constrain the new scope.
freeReferences :: Expr -> Set Name
freeReferences expr = case expr of
  ExLam binder body -> scoped [binder] (freeReferences body)
  ExTyLam binder body -> scoped [binder] (freeReferences body)
  ExLet bind body -> freeReferences (bindRhs bind) <> scoped [bindBinder bind] (freeReferences body)
  ExRec binds body -> scoped (map bindBinder binds) (foldMap (freeReferences . bindRhs) binds <> freeReferences body)
  ExCase scrutinee binder alternatives -> freeReferences scrutinee <> scoped (foldr (:) [] binder) (foldMap alternative alternatives)
  ExApp function argument -> freeReferences function <> freeReferences argument
  ExTyApp function ty -> freeReferences function <> typeReferences ty
  ExCast body proof -> freeReferences body <> exprReferences (ExCoercion proof)
  ExAbsurd body ty -> freeReferences body <> typeReferences ty
  ExForeignCall call types arguments -> exprReferences (ExForeignCall call types []) <> foldMap freeReferences arguments
  _ -> exprReferences expr
  where
    alternative alt = patternReferences (altCon alt) <> scoped (altTypeBinders alt <> altBinders alt) (freeReferences (altRhs alt))
    patternReferences pattern = case pattern of
      AltData name -> Set.singleton name
      AltLit literal -> case literal of
        LitInt rep _ -> typeReferences rep
        LitChar rep _ -> typeReferences rep
        LitAddr rep _ -> typeReferences rep
      AltDefault -> Set.empty
    scoped binders refs = foldr (\binder rest -> typeReferences (binderType binder) <> Set.delete (binderName binder) rest) refs binders

pureExpr :: TypeEnv -> Expr -> Bool
pureExpr env expr = case expr of
  ExForeignCall call _ arguments -> foreignCallConvention call == Prim && not (stateType env (foreignCallType call)) && all pureChild arguments
  ExApp function argument -> pureChild function && pureChild argument
  ExTyApp function _ -> pureChild function
  ExLam _ body -> pureChild body
  ExTyLam _ body -> pureChild body
  ExLet bind body -> pureChild (bindRhs bind) && pureChild body
  ExRec binds body -> all (pureChild . bindRhs) binds && pureChild body
  ExCase scrutinee _ alternatives -> pureChild scrutinee && all (pureChild . altRhs) alternatives
  ExCast body _ -> pureChild body
  ExAbsurd body _ -> pureChild body
  _ -> True
  where
    pureChild = pureExpr env

-- | Expand type synonyms before the state-token check.
stateType :: TypeEnv -> Type -> Bool
stateType env ty = case reduceType env ty of
  TyCon name -> nameText name == "State#"
  TyApp function argument -> stateType env function || stateType env argument
  TyFun _ _ argument result -> stateType env argument || stateType env result
  TyForAll binder body -> stateType env (binderType binder) || stateType env body
  TyEq left right -> stateType env left || stateType env right
  _ -> False

freshLocal :: FloatM Name
freshLocal = do
  state <- get
  let next = floatNext state
  modify' (\current -> current {floatNext = next + 1})
  let name = Name "$float" SortValue (OriginLocal (Unique next))
  modify' (\current -> current {floatLocals = Set.insert name (floatLocals current)})
  pure name

freshTop :: Name -> FloatM Name
freshTop owner = do
  state <- get
  let next = floatNext state
      name = owner {nameText = nameText owner <> "$float$" <> T.pack (show next)}
  modify' (\current -> current {floatNext = next + 1})
  if Set.member name (floatTaken state)
    then freshTop owner
    else do
      modify' (\current -> current {floatTaken = Set.insert name (floatTaken current)})
      pure name
