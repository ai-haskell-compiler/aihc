{-# LANGUAGE OverloadedStrings #-}

-- | Constraint generation for declarations.
--
-- Processes top-level data declarations and value bindings from a module.
module Aihc.Tc.Generate.Decl
  ( tcModule,
    TcBindingResult (..),
  )
where

import Aihc.Parser.Syntax
  ( DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Match (..),
    Module (..),
    Rhs (..),
    SourceSpan,
    UnqualifiedName (..),
    ValueDecl (..),
  )
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Text (Text)

-- | Result of type-checking a single binding.
data TcBindingResult = TcBindingResult
  { tbName :: !Text,
    tbType :: !TcType
  }
  deriving (Show)

-- | Type-check a module, returning the inferred types for each
-- top-level binding.
tcModule :: Module -> TcM [TcBindingResult]
tcModule m = do
  -- Phase 1: collect data declarations and register constructors.
  mapM_ registerDecl (moduleDecls m)
  -- Phase 2: type-check value bindings.
  results <- mapM tcDecl (moduleDecls m)
  pure (concat results)

-- | Register a declaration in the environment (data types, etc.).
-- Only data declarations are processed; other decls are skipped.
registerDecl :: Decl -> TcM ()
registerDecl (DeclData _sp dd) = registerDataDecl dd
registerDecl (DeclAnn _ inner) = registerDecl inner
registerDecl _ = pure ()

-- | Register a data declaration's constructors in the environment.
--
-- For @data Bool = True | False@, this registers:
--   - @True :: Bool@
--   - @False :: Bool@
registerDataDecl :: DataDecl -> TcM ()
registerDataDecl dd = do
  let tyName = unqualifiedNameText (dataDeclName dd)
      resTy = TcTyCon (TyCon tyName 0) []
  mapM_ (registerDataCon resTy) (dataDeclConstructors dd)

-- | Register a single data constructor as a polymorphic binding.
registerDataCon :: TcType -> DataConDecl -> TcM ()
registerDataCon resTy con = case con of
  PrefixCon _sp _docs _ctx conName args ->
    let name = unqualifiedNameText conName
        -- For each argument type, create a function type.
        -- For MVP, we ignore the actual types and treat nullary
        -- constructors as just returning the result type.
        -- Constructors with args get a function type with fresh args.
        scheme
          | null args = ForAll [] [] resTy
          | otherwise = ForAll [] [] resTy -- TODO: parse arg types
     in extendTermEnvPermanent name (TcIdBinder name scheme)
  InfixCon _sp _docs _ctx _lhs conName _rhs ->
    let name = unqualifiedNameText conName
     in extendTermEnvPermanent name (TcIdBinder name (ForAll [] [] resTy))
  RecordCon _sp _docs _ctx conName _fields ->
    let name = unqualifiedNameText conName
     in extendTermEnvPermanent name (TcIdBinder name (ForAll [] [] resTy))
  GadtCon {} -> pure () -- GADTs not handled in MVP

-- | Type-check a declaration, returning binding results for value bindings.
tcDecl :: Decl -> TcM [TcBindingResult]
tcDecl (DeclValue sp vd) = tcValueDecl sp vd
tcDecl (DeclAnn _ inner) = tcDecl inner
tcDecl _ = pure []

-- | Type-check a value declaration.
tcValueDecl :: SourceSpan -> ValueDecl -> TcM [TcBindingResult]
tcValueDecl _sp (FunctionBind _fsp binder matches) = do
  let name = unqualifiedNameText binder
  ty <- tcMatches matches
  zonkedTy <- zonkType ty
  -- Register the binding so later bindings can reference it.
  extendTermEnvPermanent name (TcIdBinder name (ForAll [] [] zonkedTy))
  pure [TcBindingResult name zonkedTy]
tcValueDecl _sp (PatternBind _psp _pat rhs) = do
  ty <- tcRhs rhs
  zonkedTy <- zonkType ty
  pure [TcBindingResult "<pattern>" zonkedTy]

-- | Type-check a list of matches (equations for a function binding).
-- For MVP, just check the first match.
tcMatches :: [Match] -> TcM TcType
tcMatches [] = freshMetaTv
tcMatches (m : _) = tcRhs (matchRhs m)

-- | Type-check a right-hand side.
tcRhs :: Rhs -> TcM TcType
tcRhs (UnguardedRhs _sp expr) = do
  (ty, cts) <- inferExpr expr
  _ <- solveConstraints cts
  pure ty
tcRhs (GuardedRhss _sp _guards) =
  -- Guarded RHS not handled in MVP.
  freshMetaTv
