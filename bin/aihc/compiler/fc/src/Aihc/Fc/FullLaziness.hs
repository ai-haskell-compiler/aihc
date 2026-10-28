{-# LANGUAGE OverloadedStrings #-}

-- | Share closed computations across function calls.
module Aihc.Fc.FullLaziness (fullLazinessProgram, sharedAtTop) where

import Aihc.Fc.Imports (exprReferences, typeReferences)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, isConstructorName, isTrivial, maxLocalUnique)
import Aihc.Fc.Size (isLiftedType)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), exprType, extendBinder, reduceType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Resolve (PackageId)
import Control.Monad (guard)
import Control.Monad.Trans.State.Strict (State, get, modify', runState)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

data FloatState = FloatState
  { -- | The module that the new thunks belong to.
    floatHome :: !Home,
    floatNext :: !Int,
    floatTaken :: !(Set Name),
    floatKnown :: !(Map Expr Name),
    floatDecls :: ![Decl],
    floatCount :: !Int
  }

type FloatM = State FloatState

-- | Move closed lifted computations under lambdas to private lazy thunks.
-- Captured computations stay local because a new captured thunk can cost more than its saved work.
-- A move does not insert a let between parameters, including parameters under casts.
fullLazinessProgram :: PackageId -> Home -> Program -> (Program, Int)
fullLazinessProgram primPackage home program =
  let env = typeEnvFromProgram primPackage program
      initial = FloatState home (maxLocalUnique program + 1) (Map.keysSet (teHeaders env)) Map.empty [] 0
      (decls, final) = runState (mapM (declaration env) (programDecls program)) initial
   in (tidyProgram program {programDecls = decls <> reverse (floatDecls final)}, floatCount final)

declaration :: TypeEnv -> Decl -> FloatM Decl
declaration env decl = case decl of
  DeclVal value -> do
    body <- walk env (valName value) False (valBody value)
    pure (DeclVal value {valBody = body})
  _ -> pure decl

-- | Select maximal closed computations only below a function parameter.
-- The root of a declaration already has a shared result.
walk :: TypeEnv -> Name -> Bool -> Expr -> FloatM Expr
walk env owner inside expr
  | inside,
    Just ty <- candidateType env expr = do
      state <- get
      modify' (\current -> current {floatCount = floatCount current + 1})
      case Map.lookup expr (floatKnown state) of
        Just name -> pure (ExVar name)
        Nothing -> do
          name <- freshTop owner
          let decl = DeclVal (ValDecl Private name ty expr (InlineNever NeverActive))
          modify' (\current -> current {floatKnown = Map.insert expr name (floatKnown current), floatDecls = decl : floatDecls current})
          pure (ExVar name)
  | otherwise = case expr of
      ExLam binder body -> ExLam binder <$> walk (extendBinder env binder) owner True body
      ExTyLam binder body -> ExTyLam binder <$> walk (extendBinder env binder) owner inside body
      ExApp function argument -> ExApp <$> child function <*> child argument
      ExTyApp function ty -> (`ExTyApp` ty) <$> child function
      ExCast body proof -> (`ExCast` proof) <$> child body
      ExAbsurd body ty -> (`ExAbsurd` ty) <$> child body
      ExLet bind body -> do
        rhs <- child (bindRhs bind)
        body' <- walk (extendBinder env (bindBinder bind)) owner inside body
        pure (ExLet bind {bindRhs = rhs} body')
      ExRec binds body -> do
        let inner = List.foldl' extendBinder env (map bindBinder binds)
        binds' <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> walk inner owner inside (bindRhs bind)) binds
        ExRec binds' <$> walk inner owner inside body
      ExCase scrutinee binder alternatives -> ExCase <$> child scrutinee <*> pure binder <*> mapM alternative alternatives
        where
          alternative alt = do
            let inner = List.foldl' extendBinder env (foldr (:) [] binder <> altTypeBinders alt <> altBinders alt)
            rhs <- walk inner owner inside (altRhs alt)
            pure alt {altRhs = rhs}
      ExForeignCall call types arguments -> ExForeignCall call types <$> mapM child arguments
      _ -> pure expr
  where
    child = walk env owner inside

-- | Whether the pass moves an expression under a lambda to a shared
-- thunk.
sharedAtTop :: TypeEnv -> Expr -> Bool
sharedAtTop env = isJust . candidateType env

-- | A shared thunk needs a lifted result and no free local names.
-- Constructor expressions remain with the constant lift pass.
candidateType :: TypeEnv -> Expr -> Maybe Type
candidateType env expr = do
  guard (not (isTrivial expr) && computation expr)
  guard (all isTop (freeReferences expr))
  guard (pureExpr env expr)
  ty <- exprType env expr
  guard (isLiftedType env ty && isNothing (viewFun env ty) && isNothing (viewForAll env ty))
  guard (all isTop (typeReferences ty))
  pure ty
  where
    computation ExLam {} = False
    computation ExTyLam {} = False
    computation (ExCast body _) = computation body
    computation expression = case collectSpine expression of
      (ExVar name, _) | isConstructorName name -> False
      _ -> True
    isTop name = case nameOrigin name of
      OriginTop {} -> True
      OriginLocal {} -> False

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
    patternReferences con = case con of
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

freshTop :: Name -> FloatM Name
freshTop owner = do
  state <- get
  let next = floatNext state
      name = derivedTopName (floatHome state) owner (nameText owner <> "$float$" <> T.pack (show next))
  modify' (\current -> current {floatNext = next + 1})
  if Set.member name (floatTaken state)
    then freshTop owner
    else do
      modify' (\current -> current {floatTaken = Set.insert name (floatTaken current)})
      pure name
