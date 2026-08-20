{-# LANGUAGE OverloadedStrings #-}

-- | Representation-correct lowering of @newtype@ constructors and patterns.
--
-- Newtypes remain nominally distinct in FC types, with 'CoreNewtypeDecl'
-- providing their representational equality axiom. Their term constructors
-- and patterns are casts and lazy bindings; they never denote heap nodes.
module Aihc.Fc2.Desugar.Core.Newtype
  ( NewtypeInterface,
    extractNewtypeInterface,
    lowerNewtypes,
    lowerNewtypesWithInterface,
  )
where

import Aihc.Fc2.Desugar.Core.Subst (substType)
import Aihc.Fc2.Desugar.Core.Syntax
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (TcType (..), TyCon (..), Unique (..))
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

newtype NewtypeInterface = NewtypeInterface
  { newtypesByConstructor :: Map CoreSymbolOrigin CoreNewtypeDecl
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
lowerNewtypes :: CoreProgram -> CoreProgram
lowerNewtypes = lowerNewtypesWithInterface mempty

-- | The representation information exported by one independently compiled
-- unit. It contains declarations only; no term implementation crosses the
-- incremental boundary.
extractNewtypeInterface :: CoreProgram -> NewtypeInterface
extractNewtypeInterface (CoreProgram _ topBinds) =
  NewtypeInterface
    ( Map.fromList
        [ (coreConstructorSymbolOrigin (coreNewtypeConstructorOrigin declaration), declaration)
        | CoreNewtype declaration <- topBinds
        ]
    )

-- | Lower one compilation unit using declaration interfaces imported from
-- already compiled units. Local declarations take precedence.
lowerNewtypesWithInterface :: NewtypeInterface -> CoreProgram -> CoreProgram
lowerNewtypesWithInterface imported program@(CoreProgram moduleId topBinds) =
  CoreProgram moduleId (evalState (mapM (lowerTopBind env) topBinds) (nextUnique program))
  where
    env = imported <> extractNewtypeInterface program

lowerTopBind :: NewtypeEnv -> CoreTopBind -> LowerM CoreTopBind
lowerTopBind env topBind =
  case topBind of
    CoreTopBind bind -> CoreTopBind <$> lowerBind env bind
    _ -> pure topBind

lowerBind :: NewtypeEnv -> CoreBind -> LowerM CoreBind
lowerBind env bind =
  case bind of
    CoreNonRec var rhs -> CoreNonRec var <$> lowerExpr env rhs
    CoreRec bindings -> CoreRec <$> mapM (traverse (lowerExpr env)) bindings

lowerExpr :: NewtypeEnv -> CoreExpr -> LowerM CoreExpr
lowerExpr env expr =
  case expr of
    CoreVar var ->
      case lookupNewtypeVar env var of
        Just declaration -> lowerConstructorValue declaration []
        Nothing -> pure expr
    CoreLit {} -> pure expr
    CoreApp function argument ->
      case newtypeConstructorSpine env function of
        Just (declaration, typeArgs) -> do
          argument' <- lowerExpr env argument
          pure (wrapNewtype declaration typeArgs argument')
        Nothing -> CoreApp <$> lowerExpr env function <*> lowerExpr env argument
    CoreTyApp {} ->
      case newtypeConstructorSpine env expr of
        Just (declaration, typeArgs) -> lowerConstructorValue declaration typeArgs
        Nothing -> lowerTypeApplication env expr
    CoreLam var body -> CoreLam var <$> lowerExpr env body
    CoreTyLam tyVar body -> CoreTyLam tyVar <$> lowerExpr env body
    CoreLet bind body -> CoreLet <$> lowerBind env bind <*> lowerExpr env body
    CoreCase scrutinee binder alternatives -> lowerCase env scrutinee binder alternatives
    CoreCast inner coercion -> (`CoreCast` coercion) <$> lowerExpr env inner
    CoreCallForeign foreignCall arguments -> CoreCallForeign foreignCall <$> mapM (lowerExpr env) arguments

lowerTypeApplication :: NewtypeEnv -> CoreExpr -> LowerM CoreExpr
lowerTypeApplication env expression =
  case expression of
    CoreTyApp function ty -> (`CoreTyApp` ty) <$> lowerExpr env function
    _ -> lowerExpr env expression

lowerConstructorValue :: CoreNewtypeDecl -> [TcType] -> LowerM CoreExpr
lowerConstructorValue declaration typeArgs = do
  binder <- freshVar "$newtype" (instantiateRepresentation declaration typeArgs)
  pure (CoreLam binder (wrapNewtype declaration typeArgs (CoreVar binder)))

lowerCase :: NewtypeEnv -> CoreExpr -> Var -> [CoreAlt] -> LowerM CoreExpr
lowerCase env scrutinee binder alternatives =
  case firstNewtypeAlternative env alternatives of
    Just (declaration, fieldBinder, rhs) -> do
      scrutinee' <- lowerExpr env scrutinee
      rhs' <- lowerExpr env rhs
      let typeArgs = newtypeArguments declaration (varType binder)
          representation = unwrapNewtype declaration typeArgs (CoreVar binder)
      pure
        ( CoreLet
            (CoreNonRec binder scrutinee')
            (CoreLet (CoreNonRec fieldBinder representation) rhs')
        )
    Nothing -> do
      scrutinee' <- lowerExpr env scrutinee
      alternatives' <- mapM (lowerAlt env) alternatives
      pure (CoreCase scrutinee' binder alternatives')

lowerAlt :: NewtypeEnv -> CoreAlt -> LowerM CoreAlt
lowerAlt env alternative = do
  rhs <- lowerExpr env (altRhs alternative)
  pure alternative {altRhs = rhs}

firstNewtypeAlternative :: NewtypeEnv -> [CoreAlt] -> Maybe (CoreNewtypeDecl, Var, CoreExpr)
firstNewtypeAlternative env alternatives =
  case [ (declaration, fieldBinder, altRhs alternative)
       | alternative <- alternatives,
         DataAlt constructor <- [altCon alternative],
         Just declaration <- [lookupNewtypeOrigin env (coreConstructorSymbolOrigin constructor)],
         [fieldBinder] <- [altBinders alternative]
       ] of
    match : _ -> Just match
    [] -> Nothing

newtypeConstructorSpine :: NewtypeEnv -> CoreExpr -> Maybe (CoreNewtypeDecl, [TcType])
newtypeConstructorSpine env = go []
  where
    go typeArgs expression =
      case expression of
        CoreTyApp inner ty -> go (ty : typeArgs) inner
        CoreVar var -> (,typeArgs) <$> lookupNewtypeVar env var
        _ -> Nothing

wrapNewtype :: CoreNewtypeDecl -> [TcType] -> CoreExpr -> CoreExpr
wrapNewtype declaration typeArgs expression =
  CoreCast expression (Sym (newtypeAxiom declaration typeArgs))

unwrapNewtype :: CoreNewtypeDecl -> [TcType] -> CoreExpr -> CoreExpr
unwrapNewtype declaration typeArgs expression =
  CoreCast expression (newtypeAxiom declaration typeArgs)

lookupNewtypeVar :: NewtypeEnv -> Var -> Maybe CoreNewtypeDecl
lookupNewtypeVar env var =
  (varResolvedName var >>= lookupNewtypeOrigin env)
    <|> uniqueSourceConstructor env (varName var)

lookupNewtypeOrigin :: NewtypeEnv -> CoreSymbolOrigin -> Maybe CoreNewtypeDecl
lookupNewtypeOrigin env origin =
  Map.lookup origin (newtypesByConstructor env)
    <|> uniqueSourceConstructor env (coreOriginName origin)

uniqueSourceConstructor :: NewtypeEnv -> Text -> Maybe CoreNewtypeDecl
uniqueSourceConstructor env name =
  case filter ((== name) . coreNewtypeConstructor) (Map.elems (newtypesByConstructor env)) of
    [declaration] -> Just declaration
    _ -> Nothing

newtypeAxiom :: CoreNewtypeDecl -> [TcType] -> Coercion
newtypeAxiom declaration typeArgs =
  AxiomInstCo (coreNewtypeName declaration) (completeTypeArgs declaration typeArgs)

instantiateRepresentation :: CoreNewtypeDecl -> [TcType] -> TcType
instantiateRepresentation declaration typeArgs =
  substType
    (Map.fromList (zip (coreNewtypeTyVars declaration) (completeTypeArgs declaration typeArgs)))
    (coreNewtypeRepresentation declaration)

newtypeArguments :: CoreNewtypeDecl -> TcType -> [TcType]
newtypeArguments declaration ty =
  case ty of
    TcTyCon (TyCon name _) arguments
      | name == coreNewtypeName declaration -> arguments
    _ -> []

completeTypeArgs :: CoreNewtypeDecl -> [TcType] -> [TcType]
completeTypeArgs declaration typeArgs
  | length typeArgs <= length tyVars =
      typeArgs <> map TcTyVar (drop (length typeArgs) tyVars)
  | otherwise = typeArgs
  where
    tyVars = coreNewtypeTyVars declaration

freshVar :: Text -> TcType -> LowerM Var
freshVar name ty =
  state $ \unique -> (Var name (Unique unique) ty, unique + 1)

nextUnique :: CoreProgram -> Int
nextUnique (CoreProgram _ topBinds) = maximum (0 : concatMap topBindUniques topBinds) + 1

topBindUniques :: CoreTopBind -> [Int]
topBindUniques topBind =
  case topBind of
    CorePrimitive var _ -> varUniques var
    CoreForeignImport {} -> []
    CoreTopBind bind -> bindUniques bind
    _ -> []

bindUniques :: CoreBind -> [Int]
bindUniques bind =
  case bind of
    CoreNonRec var rhs -> varUniques var <> exprUniques rhs
    CoreRec bindings -> concatMap (\(var, rhs) -> varUniques var <> exprUniques rhs) bindings

exprUniques :: CoreExpr -> [Int]
exprUniques expression =
  case expression of
    CoreVar var -> varUniques var
    CoreLit {} -> []
    CoreApp function argument -> exprUniques function <> exprUniques argument
    CoreTyApp inner _ -> exprUniques inner
    CoreLam var body -> varUniques var <> exprUniques body
    CoreTyLam _ body -> exprUniques body
    CoreLet bind body -> bindUniques bind <> exprUniques body
    CoreCase scrutinee binder alternatives ->
      exprUniques scrutinee <> varUniques binder <> concatMap altUniques alternatives
    CoreCast inner _ -> exprUniques inner
    CoreCallForeign _ arguments -> concatMap exprUniques arguments

altUniques :: CoreAlt -> [Int]
altUniques alternative = concatMap varUniques (altBinders alternative) <> exprUniques (altRhs alternative)

varUniques :: Var -> [Int]
varUniques var =
  case varUnique var of
    Unique unique -> [unique]
