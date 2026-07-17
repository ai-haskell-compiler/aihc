{-# LANGUAGE OverloadedStrings #-}

-- | Representation-correct lowering of @newtype@ constructors and patterns.
--
-- Newtypes remain nominally distinct in FC types, with 'FcNewtypeDecl'
-- providing their representational equality axiom. Their term constructors
-- and patterns are casts and lazy bindings; they never denote heap nodes.
module Aihc.Fc.Newtype
  ( NewtypeInterface,
    extractNewtypeInterface,
    lowerNewtypes,
    lowerNewtypesWithInterface,
  )
where

import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (TcType (..), TyCon (..), Unique (..))
import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

newtype NewtypeInterface = NewtypeInterface
  { newtypesByConstructor :: Map Text FcNewtypeDecl
  }
  deriving (Eq, Show, Read)

instance Semigroup NewtypeInterface where
  NewtypeInterface left <> NewtypeInterface right = NewtypeInterface (right <> left)

instance Monoid NewtypeInterface where
  mempty = NewtypeInterface Map.empty

type NewtypeEnv = NewtypeInterface

type LowerM = State Int

-- | Replace every known newtype construction and match with a coercion cast.
-- Running this more than once is harmless, which lets callers normalize both
-- individual modules and a later combined cross-module program.
lowerNewtypes :: FcProgram -> FcProgram
lowerNewtypes = lowerNewtypesWithInterface mempty

-- | The representation information exported by one independently compiled
-- unit. It contains declarations only; no term implementation crosses the
-- incremental boundary.
extractNewtypeInterface :: FcProgram -> NewtypeInterface
extractNewtypeInterface (FcProgram topBinds) =
  NewtypeInterface
    ( Map.fromList
        [ (fcNewtypeConstructor declaration, declaration)
        | FcNewtype declaration <- topBinds
        ]
    )

-- | Lower one compilation unit using declaration interfaces imported from
-- already compiled units. Local declarations take precedence.
lowerNewtypesWithInterface :: NewtypeInterface -> FcProgram -> FcProgram
lowerNewtypesWithInterface imported program@(FcProgram topBinds) =
  FcProgram (evalState (mapM (lowerTopBind env) topBinds) (nextUnique program))
  where
    env = imported <> extractNewtypeInterface program

lowerTopBind :: NewtypeEnv -> FcTopBind -> LowerM FcTopBind
lowerTopBind env topBind =
  case topBind of
    FcTopBind bind -> FcTopBind <$> lowerBind env bind
    _ -> pure topBind

lowerBind :: NewtypeEnv -> FcBind -> LowerM FcBind
lowerBind env bind =
  case bind of
    FcNonRec var rhs -> FcNonRec var <$> lowerExpr env rhs
    FcRec bindings -> FcRec <$> mapM (traverse (lowerExpr env)) bindings

lowerExpr :: NewtypeEnv -> FcExpr -> LowerM FcExpr
lowerExpr env expr =
  case expr of
    FcVar var ->
      case Map.lookup (varName var) (newtypesByConstructor env) of
        Just declaration -> lowerConstructorValue declaration []
        Nothing -> pure expr
    FcLit {} -> pure expr
    FcApp function argument ->
      case newtypeConstructorSpine env function of
        Just (declaration, typeArgs) -> do
          argument' <- lowerExpr env argument
          pure (wrapNewtype declaration typeArgs argument')
        Nothing -> FcApp <$> lowerExpr env function <*> lowerExpr env argument
    FcDictApp function argument -> FcDictApp <$> lowerExpr env function <*> lowerExpr env argument
    FcTyApp {} ->
      case newtypeConstructorSpine env expr of
        Just (declaration, typeArgs) -> lowerConstructorValue declaration typeArgs
        Nothing -> lowerTypeApplication env expr
    FcLam var body -> FcLam var <$> lowerExpr env body
    FcTyLam tyVar body -> FcTyLam tyVar <$> lowerExpr env body
    FcDictLam var body -> FcDictLam var <$> lowerExpr env body
    FcDict fields -> FcDict <$> mapM (lowerExpr env) fields
    FcDictSelect dictionary index -> (`FcDictSelect` index) <$> lowerExpr env dictionary
    FcLet bind body -> FcLet <$> lowerBind env bind <*> lowerExpr env body
    FcCase scrutinee binder alternatives -> lowerCase env scrutinee binder alternatives
    FcCast inner coercion -> (`FcCast` coercion) <$> lowerExpr env inner
    FcCallForeign foreignCall arguments -> FcCallForeign foreignCall <$> mapM (lowerExpr env) arguments

lowerTypeApplication :: NewtypeEnv -> FcExpr -> LowerM FcExpr
lowerTypeApplication env expression =
  case expression of
    FcTyApp function ty -> (`FcTyApp` ty) <$> lowerExpr env function
    _ -> lowerExpr env expression

lowerConstructorValue :: FcNewtypeDecl -> [TcType] -> LowerM FcExpr
lowerConstructorValue declaration typeArgs = do
  binder <- freshVar "$newtype" (instantiateRepresentation declaration typeArgs)
  pure (FcLam binder (wrapNewtype declaration typeArgs (FcVar binder)))

lowerCase :: NewtypeEnv -> FcExpr -> Var -> [FcAlt] -> LowerM FcExpr
lowerCase env scrutinee binder alternatives =
  case firstNewtypeAlternative env alternatives of
    Just (declaration, fieldBinder, rhs) -> do
      scrutinee' <- lowerExpr env scrutinee
      rhs' <- lowerExpr env rhs
      let typeArgs = newtypeArguments declaration (varType binder)
          representation = unwrapNewtype declaration typeArgs (FcVar binder)
      pure
        ( FcLet
            (FcNonRec binder scrutinee')
            (FcLet (FcNonRec fieldBinder representation) rhs')
        )
    Nothing -> do
      scrutinee' <- lowerExpr env scrutinee
      alternatives' <- mapM (lowerAlt env) alternatives
      pure (FcCase scrutinee' binder alternatives')

lowerAlt :: NewtypeEnv -> FcAlt -> LowerM FcAlt
lowerAlt env alternative = do
  rhs <- lowerExpr env (altRhs alternative)
  pure alternative {altRhs = rhs}

firstNewtypeAlternative :: NewtypeEnv -> [FcAlt] -> Maybe (FcNewtypeDecl, Var, FcExpr)
firstNewtypeAlternative env alternatives =
  case [ (declaration, fieldBinder, altRhs alternative)
       | alternative <- alternatives,
         DataAlt constructor <- [altCon alternative],
         Just declaration <- [Map.lookup constructor (newtypesByConstructor env)],
         [fieldBinder] <- [altBinders alternative]
       ] of
    match : _ -> Just match
    [] -> Nothing

newtypeConstructorSpine :: NewtypeEnv -> FcExpr -> Maybe (FcNewtypeDecl, [TcType])
newtypeConstructorSpine env = go []
  where
    go typeArgs expression =
      case expression of
        FcTyApp inner ty -> go (ty : typeArgs) inner
        FcVar var -> (,typeArgs) <$> Map.lookup (varName var) (newtypesByConstructor env)
        _ -> Nothing

wrapNewtype :: FcNewtypeDecl -> [TcType] -> FcExpr -> FcExpr
wrapNewtype declaration typeArgs expression =
  FcCast expression (Sym (newtypeAxiom declaration typeArgs))

unwrapNewtype :: FcNewtypeDecl -> [TcType] -> FcExpr -> FcExpr
unwrapNewtype declaration typeArgs expression =
  FcCast expression (newtypeAxiom declaration typeArgs)

newtypeAxiom :: FcNewtypeDecl -> [TcType] -> Coercion
newtypeAxiom declaration typeArgs =
  AxiomInstCo (fcNewtypeName declaration) (completeTypeArgs declaration typeArgs)

instantiateRepresentation :: FcNewtypeDecl -> [TcType] -> TcType
instantiateRepresentation declaration typeArgs =
  substType
    (Map.fromList (zip (fcNewtypeTyVars declaration) (completeTypeArgs declaration typeArgs)))
    (fcNewtypeRepresentation declaration)

newtypeArguments :: FcNewtypeDecl -> TcType -> [TcType]
newtypeArguments declaration ty =
  case ty of
    TcTyCon (TyCon name _) arguments
      | name == fcNewtypeName declaration -> arguments
    _ -> []

completeTypeArgs :: FcNewtypeDecl -> [TcType] -> [TcType]
completeTypeArgs declaration typeArgs
  | length typeArgs <= length tyVars =
      typeArgs <> map TcTyVar (drop (length typeArgs) tyVars)
  | otherwise = typeArgs
  where
    tyVars = fcNewtypeTyVars declaration

freshVar :: Text -> TcType -> LowerM Var
freshVar name ty =
  state $ \unique -> (Var name (Unique unique) ty, unique + 1)

nextUnique :: FcProgram -> Int
nextUnique (FcProgram topBinds) = maximum (0 : concatMap topBindUniques topBinds) + 1

topBindUniques :: FcTopBind -> [Int]
topBindUniques topBind =
  case topBind of
    FcPrimitive var _ -> varUniques var
    FcForeignImport {} -> []
    FcTopBind bind -> bindUniques bind
    _ -> []

bindUniques :: FcBind -> [Int]
bindUniques bind =
  case bind of
    FcNonRec var rhs -> varUniques var <> exprUniques rhs
    FcRec bindings -> concatMap (\(var, rhs) -> varUniques var <> exprUniques rhs) bindings

exprUniques :: FcExpr -> [Int]
exprUniques expression =
  case expression of
    FcVar var -> varUniques var
    FcLit {} -> []
    FcApp function argument -> exprUniques function <> exprUniques argument
    FcDictApp function argument -> exprUniques function <> exprUniques argument
    FcTyApp inner _ -> exprUniques inner
    FcLam var body -> varUniques var <> exprUniques body
    FcTyLam _ body -> exprUniques body
    FcDictLam var body -> varUniques var <> exprUniques body
    FcDict fields -> concatMap exprUniques fields
    FcDictSelect dictionary _ -> exprUniques dictionary
    FcLet bind body -> bindUniques bind <> exprUniques body
    FcCase scrutinee binder alternatives ->
      exprUniques scrutinee <> varUniques binder <> concatMap altUniques alternatives
    FcCast inner _ -> exprUniques inner
    FcCallForeign _ arguments -> concatMap exprUniques arguments

altUniques :: FcAlt -> [Int]
altUniques alternative = concatMap varUniques (altBinders alternative) <> exprUniques (altRhs alternative)

varUniques :: Var -> [Int]
varUniques var =
  case varUnique var of
    Unique unique -> [unique]
