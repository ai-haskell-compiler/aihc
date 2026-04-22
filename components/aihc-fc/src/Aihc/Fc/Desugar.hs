{-# LANGUAGE OverloadedStrings #-}

-- | Desugaring from type-checked surface AST to System FC Core.
--
-- The entry point 'desugarModule' takes a parsed module, runs the type
-- checker, and produces an 'FcProgram'.
module Aihc.Fc.Desugar
  ( -- * Entry point
    desugarModule,
    DesugarResult (..),
  )
where

import Aihc.Fc.Desugar.Expr (DsM, DsState (..), dsMatches, freshUnique, lookupType)
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( DataDecl (..),
    Decl (..),
    Match (..),
    Module (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    peelDeclAnn,
  )
import Aihc.Tc (TcBindingResult (..), TcModuleResult (..), renderTcType, typecheckModule)
import Aihc.Tc.Types (TcType (..), TyCon (..))
import Control.Monad.Trans.State.Strict (evalState)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | Result of desugaring.
data DesugarResult = DesugarResult
  { dsProgram :: !FcProgram,
    dsSuccess :: !Bool,
    dsErrors :: ![String]
  }
  deriving (Show)

-- | Desugar a module: parse, typecheck, then translate to Core.
desugarModule :: Module -> DesugarResult
desugarModule m =
  let tcResult = typecheckModule m
   in if not (tcmSuccess tcResult)
        then
          DesugarResult
            { dsProgram = FcProgram [],
              dsSuccess = False,
              dsErrors = map showBinding (tcmBindings tcResult)
            }
        else
          let typeEnv = Map.fromList [(tbName b, tbType b) | b <- tcmBindings tcResult]
              binds = evalState (dsModule m) (DsState 1000 typeEnv Map.empty)
           in DesugarResult
                { dsProgram = FcProgram binds,
                  dsSuccess = True,
                  dsErrors = []
                }

-- | Format a binding result for error messages.
showBinding :: TcBindingResult -> String
showBinding b = T.unpack (tbName b) ++ " :: " ++ renderTcType (tbType b)

-- | Desugar a module's declarations.
dsModule :: Module -> DsM [FcTopBind]
dsModule m = do
  let decls = moduleDecls m
  -- Phase 1: data declarations.
  dataTops <- concat <$> mapM dsDecl decls
  -- Phase 2: group and desugar value bindings.
  let grouped = groupFunctionBinds decls
  valueTops <- mapM dsGroup grouped
  pure (dataTops ++ valueTops)

-- | Desugar a single declaration (data types only; values handled by groups).
dsDecl :: Decl -> DsM [FcTopBind]
dsDecl (DeclData dd) = (: []) <$> dsDataDeclM dd
dsDecl (DeclAnn _ inner) = dsDecl inner
dsDecl _ = pure []

-- | Desugar a data declaration.
dsDataDeclM :: DataDecl -> DsM FcTopBind
dsDataDeclM dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
      cons = map (\c -> let (n, arity) = dsDataConPure c in (n, replicate arity (TcTyCon (TyCon "?" 0) []))) (dataDeclConstructors dd)
  pure (FcData tyName [] cons)

-- | A group of function bind declarations (possibly multi-equation).
data DeclGroup = DeclGroup
  { dgName :: !Text,
    dgMatches :: ![Match]
  }

-- | Group consecutive FunctionBind declarations with the same name.
groupFunctionBinds :: [Decl] -> [DeclGroup]
groupFunctionBinds [] = []
groupFunctionBinds (d : ds) = case extractFunBind d of
  Just (name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        allMatches = matches ++ concatMap (maybe [] snd . extractFunBind) sameNameDecls
     in DeclGroup name allMatches : groupFunctionBinds rest
  Nothing -> groupFunctionBinds ds

-- | Extract function bind info from a declaration.
extractFunBind :: Decl -> Maybe (Text, [Match])
extractFunBind decl = case peelDeclAnn decl of
  DeclValue (FunctionBind name matches) ->
    Just (unqualifiedNameText name, matches)
  _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: Text -> Decl -> Bool
hasSameName name d = case extractFunBind d of
  Just (n, _) -> n == name
  Nothing -> False

-- | Desugar a function binding group.
dsGroup :: DeclGroup -> DsM FcTopBind
dsGroup grp = do
  ty <- lookupType (dgName grp)
  u <- freshUnique
  let var = Var (dgName grp) u ty
  body <- dsMatches ty (dgMatches grp)
  pure (FcTopBind (FcNonRec var body))
