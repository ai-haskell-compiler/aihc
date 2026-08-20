{-# LANGUAGE OverloadedStrings #-}

-- | Convert private desugarer terms into System FC 2 values.
module Aihc.Fc2.Desugar.ConvertCore
  ( convertValueDecls,
  )
where

import Aihc.Fc2.Convert
import Aihc.Fc2.Desugar.Core.Syntax qualified as Core
import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence qualified as Ev
import Aihc.Tc.Types (RuntimeRep (..), Unique (..))
import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

type TopVars = Map Unique Name

convertValueDecls :: ConvertEnv -> Core.CoreModuleId -> [Core.CoreTopBind] -> Either String [Decl]
convertValueDecls env moduleId topBinds =
  let tops = Map.fromList (concatMap (topBindVars moduleId) topBinds)
   in fmap concat (mapM (convertTopBind env moduleId tops) topBinds)

topBindVars :: Core.CoreModuleId -> Core.CoreTopBind -> [(Unique, Name)]
topBindVars moduleId topBind =
  case topBind of
    Core.CoreTopBind bind -> bindVars moduleId bind
    _ -> []

bindVars :: Core.CoreModuleId -> Core.CoreBind -> [(Unique, Name)]
bindVars moduleId bind =
  case bind of
    Core.CoreNonRec var _ -> [(Core.varUnique var, topVarName moduleId var)]
    Core.CoreRec bindings -> [(Core.varUnique var, topVarName moduleId var) | (var, _) <- bindings]

convertTopBind :: ConvertEnv -> Core.CoreModuleId -> TopVars -> Core.CoreTopBind -> Either String [Decl]
convertTopBind env moduleId tops topBind =
  case topBind of
    Core.CoreTopBind bind -> convertBindDecls env moduleId tops bind
    Core.CorePrimitive var _ -> (: []) . DeclPrim <$> convertPrim env moduleId var
    Core.CoreForeignImport {} -> Left "System FC 2 accepts only foreign import prim"
    _ -> Right []

convertBindDecls :: ConvertEnv -> Core.CoreModuleId -> TopVars -> Core.CoreBind -> Either String [Decl]
convertBindDecls env moduleId tops bind =
  case bind of
    Core.CoreNonRec var expr -> (: []) . DeclVal <$> convertVal env moduleId tops var expr
    Core.CoreRec bindings -> mapM (fmap DeclVal . uncurry (convertVal env moduleId tops)) bindings

convertVal :: ConvertEnv -> Core.CoreModuleId -> TopVars -> Core.Var -> Core.CoreExpr -> Either String ValDecl
convertVal env moduleId tops var expr = do
  ty <- convertType env (Core.varType var)
  body <- convertExpr env tops expr
  pure
    ValDecl
      { valVis = Pub,
        valName = topVarName moduleId var,
        valType = ty,
        valBody = body
      }

convertPrim :: ConvertEnv -> Core.CoreModuleId -> Core.Var -> Either String PrimDecl
convertPrim env moduleId var = do
  ty <- convertType env (Core.varType var)
  pure
    PrimDecl
      { primVis = Pub,
        primName = topVarName moduleId var,
        primType = ty
      }

convertExpr :: ConvertEnv -> TopVars -> Core.CoreExpr -> Either String Expr
convertExpr env tops expression =
  case expression of
    Core.CoreVar var -> Right (ExVar (varNameFc2 tops var))
    Core.CoreLit literal _ -> ExLit <$> convertLiteral env literal
    Core.CoreApp function argument -> ExApp <$> convertExpr env tops function <*> convertExpr env tops argument
    Core.CoreTyApp function ty -> ExTyApp <$> convertExpr env tops function <*> convertType env ty
    Core.CoreLam var body -> do
      binder <- convertTermBinder env tops var
      ExLam binder <$> convertExpr env tops body
    Core.CoreTyLam tyVar body -> do
      binder <- tyVarBinder env tyVar
      ExTyLam binder <$> convertExpr (withTyVar tyVar env) tops body
    Core.CoreLet bind body ->
      case bind of
        Core.CoreNonRec var expr -> do
          binder <- convertTermBinder env tops var
          rhs <- convertExpr env tops expr
          ExLet (Bind binder rhs) <$> convertExpr env tops body
        Core.CoreRec bindings -> do
          converted <- mapM (convertRecBind env tops) bindings
          ExRec converted <$> convertExpr env tops body
    Core.CoreCase scrutinee binder alternatives -> do
      scrutinee' <- convertExpr env tops scrutinee
      caseBinder <- convertTermBinder env tops binder
      alts <- mapM (convertAlt env tops) alternatives
      pure (ExCase scrutinee' caseBinder alts)
    Core.CoreCast inner coercion ->
      ExCast <$> convertExpr env tops inner <*> convertCoercion env coercion
    Core.CoreCallForeign {} ->
      Left "System FC 2 accepts only foreign import prim"

convertRecBind :: ConvertEnv -> TopVars -> (Core.Var, Core.CoreExpr) -> Either String Bind
convertRecBind env tops (var, expr) = do
  binder <- convertTermBinder env tops var
  Bind binder <$> convertExpr env tops expr

convertAlt :: ConvertEnv -> TopVars -> Core.CoreAlt -> Either String Alt
convertAlt env tops alternative = do
  con <- convertAltCon env (Core.altCon alternative)
  binders <- mapM (convertTermBinder env tops) (Core.altBinders alternative)
  rhs <- convertExpr env tops (Core.altRhs alternative)
  pure (Alt con binders rhs)

convertAltCon :: ConvertEnv -> Core.CoreAltCon -> Either String AltCon
convertAltCon env alternative =
  case alternative of
    Core.DataAlt constructor ->
      Right
        ( AltData
            ( Name
                (Core.coreConstructorName constructor)
                SortDataConstructor
                (OriginTop (Core.coreConstructorPackage constructor) (Core.coreConstructorModule constructor))
            )
        )
    Core.LitAlt literal _ -> AltLit <$> convertLiteral env literal
    Core.DefaultAlt -> Right AltDefault

convertLiteral :: ConvertEnv -> Core.Literal -> Either String Literal
convertLiteral env literal =
  case literal of
    Core.LitInt runtimeRep value -> LitInt <$> convertRep env runtimeRep <*> pure value
    Core.LitChar runtimeRep value -> LitChar <$> convertRep env runtimeRep <*> pure value
    Core.LitString value -> Right (LitString value)
    Core.LitAddr value -> LitAddr <$> convertRep env AddrRep <*> pure value

convertCoercion :: ConvertEnv -> Ev.Coercion -> Either String Coercion
convertCoercion env coercion =
  case coercion of
    Ev.CoVar (Ev.EvVar unique) ->
      Right (CoVar (Name "c" SortValue (OriginLocal unique)))
    Ev.Refl ty -> CoRefl <$> convertType env ty
    Ev.Sym inner -> CoSym <$> convertCoercion env inner
    Ev.Trans left right -> CoTrans <$> convertCoercion env left <*> convertCoercion env right
    Ev.TyConAppCo tyCon arguments ->
      CoTyConApp (tyConNameFc2 env tyCon) <$> mapM (convertCoercion env) arguments
    Ev.AxiomInstCo name arguments ->
      CoAxiom (lookupAxiomName env name) <$> mapM (convertType env) arguments

convertTermBinder :: ConvertEnv -> TopVars -> Core.Var -> Either String Binder
convertTermBinder env tops var = do
  ty <- convertType env (Core.varType var)
  pure (Binder (varNameFc2 tops var) ty)

topVarName :: Core.CoreModuleId -> Core.Var -> Name
topVarName moduleId var =
  case Core.varResolvedName var of
    Just (Core.CoreTopLevelOrigin package moduleName' name) ->
      Name name SortValue (OriginTop (originPackage package moduleId) moduleName')
    _ ->
      Name
        (displayName var)
        SortValue
        (OriginTop (Core.coreModulePackage moduleId) (Core.coreModuleName moduleId))

originPackage :: Text -> Core.CoreModuleId -> PackageId
originPackage package moduleId =
  if T.null package
    then Core.coreModulePackage moduleId
    else PackageId package

varNameFc2 :: TopVars -> Core.Var -> Name
varNameFc2 tops var =
  case Map.lookup (Core.varUnique var) tops of
    Just name -> name
    Nothing ->
      case Core.varResolvedName var of
        Just (Core.CoreTopLevelOrigin package moduleName' name) ->
          let sort =
                if startsWithConstructor name
                  then SortDataConstructor
                  else SortValue
           in Name name sort (OriginTop (PackageId package) moduleName')
        Just (Core.CoreBuiltinOrigin name) ->
          Name name SortValue (OriginLocal (Core.varUnique var))
        Nothing ->
          Name (displayName var) SortValue (OriginLocal (Core.varUnique var))

displayName :: Core.Var -> Text
displayName var = fromMaybe (Core.varName var) (stripUniqueSuffix (Core.varName var) (Core.varUnique var))

stripUniqueSuffix :: Text -> Unique -> Maybe Text
stripUniqueSuffix name (Unique unique) = do
  stripped <- T.stripSuffix (T.pack (show unique)) name
  if T.null stripped then Nothing else Just stripped

startsWithConstructor :: Text -> Bool
startsWithConstructor name =
  case T.uncons name of
    Just (first, _) -> first == ':' || first == '[' || isAsciiUpper first
    Nothing -> False
