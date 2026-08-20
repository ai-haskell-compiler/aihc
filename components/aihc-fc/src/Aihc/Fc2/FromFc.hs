{-# LANGUAGE OverloadedStrings #-}

-- | Convert System FC terms into System FC 2 values.
module Aihc.Fc2.FromFc
  ( convertValueDecls,
  )
where

import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax qualified as Fc
import Aihc.Fc2.Convert
import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence qualified as Ev
import Aihc.Tc.Types (RuntimeRep (..), TcType (..), Unique (..))
import Data.Char (isAsciiUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

type TopVars = Map Unique Name

convertValueDecls :: ConvertEnv -> Fc.FcModuleId -> [Fc.FcTopBind] -> Either String [Decl]
convertValueDecls env moduleId topBinds =
  let tops = Map.fromList (concatMap (topBindVars moduleId) topBinds)
   in fmap concat (mapM (convertTopBind env moduleId tops) topBinds)

topBindVars :: Fc.FcModuleId -> Fc.FcTopBind -> [(Unique, Name)]
topBindVars moduleId topBind =
  case topBind of
    Fc.FcTopBind bind -> bindVars moduleId bind
    _ -> []

bindVars :: Fc.FcModuleId -> Fc.FcBind -> [(Unique, Name)]
bindVars moduleId bind =
  case bind of
    Fc.FcNonRec var _ -> [(Fc.varUnique var, topVarName moduleId var)]
    Fc.FcRec bindings -> [(Fc.varUnique var, topVarName moduleId var) | (var, _) <- bindings]

convertTopBind :: ConvertEnv -> Fc.FcModuleId -> TopVars -> Fc.FcTopBind -> Either String [Decl]
convertTopBind env moduleId tops topBind =
  case topBind of
    Fc.FcTopBind bind -> convertBindDecls env moduleId tops bind
    Fc.FcPrimitive var _ -> (: []) . DeclPrim <$> convertPrim env moduleId var
    Fc.FcForeignImport {} -> Left "System FC 2 accepts only foreign import prim"
    _ -> Right []

convertBindDecls :: ConvertEnv -> Fc.FcModuleId -> TopVars -> Fc.FcBind -> Either String [Decl]
convertBindDecls env moduleId tops bind =
  case bind of
    Fc.FcNonRec var expr -> (: []) . DeclVal <$> convertVal env moduleId tops var expr
    Fc.FcRec bindings -> mapM (fmap DeclVal . uncurry (convertVal env moduleId tops)) bindings

convertVal :: ConvertEnv -> Fc.FcModuleId -> TopVars -> Fc.Var -> Fc.FcExpr -> Either String ValDecl
convertVal env moduleId tops var expr = do
  ty <- convertType env (Fc.varType var)
  body <- convertExpr env tops (Just (Fc.varType var)) expr
  pure
    ValDecl
      { valVis = Pub,
        valName = topVarName moduleId var,
        valType = ty,
        valBody = body
      }

convertPrim :: ConvertEnv -> Fc.FcModuleId -> Fc.Var -> Either String PrimDecl
convertPrim env moduleId var = do
  ty <- convertType env (Fc.varType var)
  pure
    PrimDecl
      { primVis = Pub,
        primName = topVarName moduleId var,
        primType = ty
      }

convertExpr :: ConvertEnv -> TopVars -> Maybe TcType -> Fc.FcExpr -> Either String Expr
convertExpr env tops expectedType expression =
  case expression of
    Fc.FcVar var -> Right (ExVar (varNameFc2 tops var))
    Fc.FcLit literal _ -> ExLit <$> convertLiteral env literal
    Fc.FcApp function argument -> do
      argumentType <- functionArgumentType =<< fcExprType function
      ExApp
        <$> convertExpr env tops Nothing function
        <*> convertExpr env tops (Just argumentType) argument
    Fc.FcTyApp function ty -> ExTyApp <$> convertExpr env tops Nothing function <*> convertType env ty
    Fc.FcLam var body -> do
      binder <- convertTermBinder env tops var
      bodyType <- case expectedType of
        Just ty -> functionResultType ty
        Nothing -> fcExprType body
      ExLam binder <$> convertExpr env tops (Just bodyType) body
    Fc.FcTyLam tyVar body -> do
      binder <- tyVarBinder env tyVar
      bodyType <- case expectedType of
        Just (TcForAllTy _ ty) -> Right ty
        _ -> fcExprType body
      ExTyLam binder <$> convertExpr (withTyVar tyVar env) tops (Just bodyType) body
    Fc.FcLet bind body ->
      case bind of
        Fc.FcNonRec var expr -> do
          binder <- convertTermBinder env tops var
          rhs <- convertExpr env tops (Just (Fc.varType var)) expr
          ExLet (Bind binder rhs) <$> convertExpr env tops expectedType body
        Fc.FcRec bindings -> do
          converted <- mapM (convertRecBind env tops) bindings
          ExRec converted <$> convertExpr env tops expectedType body
    Fc.FcCase scrutinee binder alternatives -> do
      resultType <- case expectedType of
        Just ty -> Right ty
        Nothing -> case alternatives of
          first : _ -> fcExprType (Fc.altRhs first)
          [] -> Left "Core-v2 case expression does not have an expected result type"
      scrutinee' <- convertExpr env tops (Just (Fc.varType binder)) scrutinee
      caseBinder <- convertTermBinder env tops binder
      resultType' <- convertType env resultType
      alts <- mapM (convertAlt env tops resultType) alternatives
      pure (ExCase scrutinee' caseBinder resultType' alts)
    Fc.FcCast inner coercion ->
      ExCast <$> convertExpr env tops Nothing inner <*> convertCoercion env coercion
    Fc.FcCallForeign {} ->
      Left "System FC 2 accepts only foreign import prim"

convertRecBind :: ConvertEnv -> TopVars -> (Fc.Var, Fc.FcExpr) -> Either String Bind
convertRecBind env tops (var, expr) = do
  binder <- convertTermBinder env tops var
  Bind binder <$> convertExpr env tops (Just (Fc.varType var)) expr

convertAlt :: ConvertEnv -> TopVars -> TcType -> Fc.FcAlt -> Either String Alt
convertAlt env tops resultType alternative = do
  con <- convertAltCon env (Fc.altCon alternative)
  binders <- mapM (convertTermBinder env tops) (Fc.altBinders alternative)
  rhs <- convertExpr env tops (Just resultType) (Fc.altRhs alternative)
  pure (Alt con binders rhs)

fcExprType :: Fc.FcExpr -> Either String TcType
fcExprType expression =
  case expression of
    Fc.FcVar var -> Right (Fc.varType var)
    Fc.FcLit _ ty -> Right ty
    Fc.FcApp function _ -> functionResultType =<< fcExprType function
    Fc.FcTyApp function ty -> do
      functionType <- fcExprType function
      case functionType of
        TcForAllTy tyVar body -> Right (substType (Map.singleton tyVar ty) body)
        _ -> Left "Core-v2 type application does not have a quantified function type"
    Fc.FcLam binder body -> TcFunTy (Fc.varType binder) <$> fcExprType body
    Fc.FcTyLam tyVar body -> TcForAllTy tyVar <$> fcExprType body
    Fc.FcLet _ body -> fcExprType body
    Fc.FcCase _ _ alternatives ->
      case alternatives of
        first : _ -> fcExprType (Fc.altRhs first)
        [] -> Left "Core-v2 cannot infer the result type of an empty FC1 case expression"
    Fc.FcCast inner _ -> fcExprType inner
    Fc.FcCallForeign foreignCall _ -> Right (Fc.fcForeignCallResultType (Fc.fcForeignCallSignature foreignCall))

functionArgumentType :: TcType -> Either String TcType
functionArgumentType ty =
  case ty of
    TcFunTy argument _ -> Right argument
    TcQualTy [] body -> functionArgumentType body
    TcQualTy (_ : predicates) body -> Right (TcQualTy predicates body)
    _ -> Left "Core-v2 application does not have a function type"

functionResultType :: TcType -> Either String TcType
functionResultType ty =
  case ty of
    TcFunTy _ result -> Right result
    TcQualTy [] body -> functionResultType body
    TcQualTy (_ : predicates) body -> Right (if null predicates then body else TcQualTy predicates body)
    _ -> Left "Core-v2 expression does not have a function result type"

convertAltCon :: ConvertEnv -> Fc.FcAltCon -> Either String AltCon
convertAltCon env alternative =
  case alternative of
    Fc.DataAlt constructor ->
      Right
        ( AltData
            ( Name
                (Fc.fcConstructorName constructor)
                SortDataConstructor
                (OriginTop (Fc.fcConstructorPackage constructor) (Fc.fcConstructorModule constructor))
            )
        )
    Fc.LitAlt literal _ -> AltLit <$> convertLiteral env literal
    Fc.DefaultAlt -> Right AltDefault

convertLiteral :: ConvertEnv -> Fc.Literal -> Either String Literal
convertLiteral env literal =
  case literal of
    Fc.LitInt runtimeRep value -> LitInt <$> convertRep env runtimeRep <*> pure value
    Fc.LitChar runtimeRep value -> LitChar <$> convertRep env runtimeRep <*> pure value
    Fc.LitString value -> Right (LitString value)
    Fc.LitAddr value -> LitAddr <$> convertRep env AddrRep <*> pure value

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

convertTermBinder :: ConvertEnv -> TopVars -> Fc.Var -> Either String Binder
convertTermBinder env tops var = do
  ty <- convertType env (Fc.varType var)
  pure (Binder (varNameFc2 tops var) ty)

topVarName :: Fc.FcModuleId -> Fc.Var -> Name
topVarName moduleId var =
  case Fc.varResolvedName var of
    Just (Fc.FcTopLevelOrigin package moduleName' name) ->
      Name name SortValue (OriginTop (originPackage package moduleId) moduleName')
    _ ->
      Name
        (displayName var)
        SortValue
        (OriginTop (Fc.fcModulePackage moduleId) (Fc.fcModuleName moduleId))

originPackage :: Text -> Fc.FcModuleId -> PackageId
originPackage package moduleId =
  if T.null package
    then Fc.fcModulePackage moduleId
    else PackageId package

varNameFc2 :: TopVars -> Fc.Var -> Name
varNameFc2 tops var =
  case Map.lookup (Fc.varUnique var) tops of
    Just name -> name
    Nothing ->
      case Fc.varResolvedName var of
        Just (Fc.FcTopLevelOrigin package moduleName' name) ->
          let sort =
                if startsWithConstructor name
                  then SortDataConstructor
                  else SortValue
           in Name name sort (OriginTop (PackageId package) moduleName')
        Just (Fc.FcBuiltinOrigin name) ->
          Name name SortValue (OriginLocal (Fc.varUnique var))
        Nothing ->
          Name (displayName var) SortValue (OriginLocal (Fc.varUnique var))

displayName :: Fc.Var -> Text
displayName var = fromMaybe (Fc.varName var) (stripUniqueSuffix (Fc.varName var) (Fc.varUnique var))

stripUniqueSuffix :: Text -> Unique -> Maybe Text
stripUniqueSuffix name (Unique unique) = do
  stripped <- T.stripSuffix (T.pack (show unique)) name
  if T.null stripped then Nothing else Just stripped

startsWithConstructor :: Text -> Bool
startsWithConstructor name =
  case T.uncons name of
    Just (first, _) -> first == ':' || first == '[' || isAsciiUpper first
    Nothing -> False
