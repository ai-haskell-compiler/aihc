{-# LANGUAGE OverloadedStrings #-}

-- | Lift closed constructor expressions into private constants.
module Aihc.Fc.ConstantLift (liftConstants) where

import Aihc.Fc.Imports (exprReferences, typeReferences)
import Aihc.Fc.Name
import Aihc.Fc.Simplify (collectSpine, isConstructorName)
import Aihc.Fc.Size (isLiftedType)
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv (..), lookupHeaderType, substType, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Fc.Wired (primPackageFromScopes)
import Control.Monad (foldM, guard)
import Control.Monad.Trans.State.Strict (State, get, modify', put, runState)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

data LiftState = LiftState
  { liftNext :: !Int,
    liftNames :: !(Set Name),
    liftKnown :: !(Map Expr Name),
    liftDecls :: ![Decl],
    liftSites :: !Int
  }

-- | Return the program, the number of new constants, and the number of sites.
-- The pass preserves lazy fields. It does not evaluate a constant early.
-- Local term, type, and coercion references prevent a lift.
liftConstants :: Program -> (Program, Int, Int)
liftConstants program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, 0, 0)
    Just package ->
      let env = typeEnvFromProgram package program
          initial = LiftState 0 (Map.keysSet (teHeaders env)) Map.empty [] 0
          (decls, final) = runState (mapM (liftDecl env) (programDecls program)) initial
       in ( program {programDecls = decls <> reverse (liftDecls final)},
            length (liftDecls final),
            liftSites final
          )

liftDecl :: TypeEnv -> Decl -> State LiftState Decl
liftDecl env declaration =
  case declaration of
    DeclVal value -> do
      -- The existing declaration already shares its root expression.
      body <- walkChildren env (valName value) (valBody value)
      pure (DeclVal value {valBody = body})
    _ -> pure declaration

liftExpr :: TypeEnv -> Name -> Expr -> State LiftState Expr
liftExpr env owner expression =
  case constantType env expression of
    Just ty -> do
      state <- get
      modify' (\current -> current {liftSites = liftSites current + 1})
      case Map.lookup expression (liftKnown state) of
        Just name -> pure (ExVar name)
        Nothing -> do
          name <- freshConstant owner
          let declaration = DeclVal (ValDecl Private name ty expression (InlineNever NeverActive))
          modify' (\current -> current {liftKnown = Map.insert expression name (liftKnown current), liftDecls = declaration : liftDecls current})
          pure (ExVar name)
    Nothing -> walkChildren env owner expression

walkChildren :: TypeEnv -> Name -> Expr -> State LiftState Expr
walkChildren env owner expression =
  case expression of
    ExVar {} -> pure expression
    ExLit {} -> pure expression
    ExCoercion {} -> pure expression
    ExApp function argument -> ExApp <$> walk function <*> walk argument
    ExTyApp function ty -> (`ExTyApp` ty) <$> walk function
    ExLam binder body -> ExLam binder <$> walk body
    ExTyLam binder body -> ExTyLam binder <$> walk body
    ExLet binding body -> ExLet <$> walkBind binding <*> walk body
    ExRec bindings body -> ExRec <$> mapM walkBind bindings <*> walk body
    ExCase scrutinee binder ty alternatives -> ExCase <$> walk scrutinee <*> pure binder <*> pure ty <*> mapM walkAlt alternatives
    ExCast body coercion -> (`ExCast` coercion) <$> walk body
    ExForeignCall call types arguments -> ExForeignCall call types <$> mapM walk arguments
  where
    walk = liftExpr env owner
    walkBind binding = (\rhs -> binding {bindRhs = rhs}) <$> walk (bindRhs binding)
    walkAlt alternative = (\rhs -> alternative {altRhs = rhs}) <$> walk (altRhs alternative)

-- | Accept saturated, lifted constructors with no local references.
-- Nullary constructors already have static objects. Partial constructors,
-- unboxed results, and arbitrary calls are not candidates.
constantType :: TypeEnv -> Expr -> Maybe Type
constantType env expression = do
  let (headExpr, arguments) = collectSpine expression
  ExVar constructor <- pure headExpr
  guard (isConstructorName constructor && any isValueArgument arguments)
  guard (all isTop (exprReferences expression))
  header <- lookupHeaderType env constructor
  ty <- foldM applyArgument header arguments
  guard (isNothing (viewForAll env ty) && isNothing (viewFun env ty))
  guard (isLiftedType env ty && all isTop (typeReferences ty))
  pure ty
  where
    isValueArgument (Right _) = True
    isValueArgument _ = False
    applyArgument ty argument =
      case argument of
        Left actual -> do
          (binder, body) <- viewForAll env ty
          pure (substType (binderName binder) actual body)
        Right _ -> do
          (_, _, _, result) <- viewFun env ty
          pure result
    isTop name = case nameOrigin name of
      OriginTop {} -> True
      OriginLocal {} -> False

freshConstant :: Name -> State LiftState Name
freshConstant owner = do
  state <- get
  let index = liftNext state
      name = owner {nameText = nameText owner <> "$constant$" <> T.pack (show index), nameSort = SortValue}
  put state {liftNext = index + 1}
  if Set.member name (liftNames state)
    then freshConstant owner
    else do
      modify' (\current -> current {liftNames = Set.insert name (liftNames current)})
      pure name
