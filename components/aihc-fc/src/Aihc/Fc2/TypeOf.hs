{-# LANGUAGE OverloadedStrings #-}

-- | typeOf and unfold tables for implicit FUN representations.
module Aihc.Fc2.TypeOf
  ( TypeEnv (..),
    emptyTypeEnv,
    typeEnvFromProgram,
    typeEnvFromPrograms,
    typeOf,
    unfoldType,
    unfoldRep,
    isLiftedRep,
    liftedRepType,
    repOf,
    headerType,
    applyType,
    lookupBinderType,
    lookupHeaderType,
    extendBinder,
    substType,
    reduceType,
    typesEqual,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.Wired
import Aihc.Resolve (PackageId)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)

-- | Local headers, synonym bodies, and binder types used by typeOf.
data TypeEnv = TypeEnv
  { tePrimPackage :: Maybe PackageId,
    teHeaders :: Map Name Type,
    teSynonyms :: Map Name Type,
    teBinders :: Map Name Type
  }
  deriving (Eq, Show)

emptyTypeEnv :: TypeEnv
emptyTypeEnv =
  TypeEnv
    { tePrimPackage = Nothing,
      teHeaders = Map.empty,
      teSynonyms = Map.empty,
      teBinders = Map.empty
    }

typeEnvFromProgram :: Program -> TypeEnv
typeEnvFromProgram program =
  typeEnvFromPrograms [program]

-- | Register every header from every program. Later programs replace equal names.
typeEnvFromPrograms :: [Program] -> TypeEnv
typeEnvFromPrograms programs =
  foldl' addProgram baseEnv programs
  where
    baseEnv =
      emptyTypeEnv
        { tePrimPackage = listToMaybe (mapMaybe (primPackageFromScopes . programScopes) programs)
        }
    addProgram env program = foldl' addDecl env (programDecls program)

addDecl :: TypeEnv -> Decl -> TypeEnv
addDecl env decl =
  case decl of
    DeclType declaration ->
      env {teHeaders = foldl' addConstructor (Map.insert (typeName declaration) (headerType (typeBinders declaration) (typeResult declaration)) (teHeaders env)) (typeCons declaration)}
      where
        addConstructor headers constructor = Map.insert (conName constructor) (conType constructor) headers
    DeclSynonym declaration ->
      env
        { teHeaders = Map.insert (synName declaration) (headerType (synBinders declaration) (synResult declaration)) (teHeaders env),
          teSynonyms = Map.insert (synName declaration) (foldr TyForAll (synBody declaration) (synBinders declaration)) (teSynonyms env)
        }
    DeclAxiom {} -> env
    DeclVal declaration ->
      env {teHeaders = Map.insert (valName declaration) (valType declaration) (teHeaders env)}
    DeclPrim declaration ->
      env {teHeaders = Map.insert (primName declaration) (primType declaration) (teHeaders env)}

headerType :: [Binder] -> Type -> Type
headerType binders result = foldr TyForAll result binders

lookupBinderType :: TypeEnv -> Name -> Maybe Type
lookupBinderType env name = Map.lookup name (teBinders env)

lookupHeaderType :: TypeEnv -> Name -> Maybe Type
lookupHeaderType env name =
  case Map.lookup name (teHeaders env) of
    Just header -> Just header
    Nothing -> wiredTypeOf env name

typeOf :: TypeEnv -> Type -> Maybe Type
typeOf env ty =
  case ty of
    TyVar name ->
      Map.lookup name (teBinders env)
    TyCon name ->
      case Map.lookup name (teHeaders env) of
        Just header -> Just header
        Nothing -> wiredTypeOf env name
    TyApp function argument ->
      do
        functionType <- typeOf env function
        applyType functionType argument
    TyFun {} ->
      typeSynonym <$> tePrimPackage env
    TyForAll binder body ->
      typeOf (extendBinder env binder) body
    TyEq {} ->
      TyCon . constraintName <$> tePrimPackage env

applyType :: Type -> Type -> Maybe Type
applyType function argument =
  case function of
    TyForAll binder body ->
      Just (substType (binderName binder) argument body)
    TyFun _ _ _ result ->
      Just result
    _ ->
      Nothing

unfoldType :: TypeEnv -> Type -> Type
unfoldType env ty =
  case ty of
    TyCon name
      | isWiredName env name "Type",
        Just representation <- liftedRepType env ->
          typeAppTYPE env representation
      | isWiredName env name "Constraint",
        Just representation <- liftedRepType env ->
          typeAppTYPE env representation
      | Just body <- Map.lookup name (teSynonyms env) ->
          unfoldType env (stripForAlls body)
      | isWiredName env name "TYPE" -> ty
      | isWiredName env name "RuntimeRep" -> ty
      | isWiredName env name "Levity" -> ty
      | otherwise -> ty
    TyApp (TyCon name) argument
      | isWiredName env name "TYPE" -> TyApp (TyCon name) argument
      | otherwise -> ty
    _ -> ty

unfoldRep :: TypeEnv -> Type -> Type
unfoldRep env ty =
  case ty of
    TyCon name
      | isWiredName env name "LiftedRep",
        Just package <- tePrimPackage env ->
          TyApp (TyCon (boxedRepName package)) (TyCon (liftedName package))
      | isWiredName env name "UnliftedRep",
        Just package <- tePrimPackage env ->
          TyApp (TyCon (boxedRepName package)) (TyCon (unliftedName package))
      | otherwise -> ty
    TyApp (TyCon name) argument
      | isWiredName env name "BoxedRep" -> TyApp (TyCon name) argument
      | otherwise -> ty
    _ -> ty

repOf :: TypeEnv -> Type -> Maybe Type
repOf env ty = do
  kind <- typeOf env ty
  case unfoldType env kind of
    TyApp (TyCon name) representation
      | isWiredName env name "TYPE" -> Just representation
    _ -> Nothing

-- | True when a stored FUN representation is lifted.
isLiftedRep :: TypeEnv -> Type -> Bool
isLiftedRep env ty =
  case unfoldRep env ty of
    TyApp (TyCon boxed) (TyCon levity)
      | isWiredName env boxed "BoxedRep",
        isWiredName env levity "Lifted" ->
          True
    TyCon name -> isWiredName env name "LiftedRep"
    _ -> False

extendBinder :: TypeEnv -> Binder -> TypeEnv
extendBinder env binder =
  env {teBinders = Map.insert (binderName binder) (binderType binder) (teBinders env)}

stripForAlls :: Type -> Type
stripForAlls ty =
  case ty of
    TyForAll _ body -> stripForAlls body
    _ -> ty

substType :: Name -> Type -> Type -> Type
substType target replacement = go
  where
    go ty =
      case ty of
        TyVar name
          | name == target -> replacement
          | otherwise -> ty
        TyCon {} -> ty
        TyApp function argument -> TyApp (go function) (go argument)
        TyFun r1 r2 argument result -> TyFun (go r1) (go r2) (go argument) (go result)
        TyForAll binder body
          | binderName binder == target -> TyForAll binder {binderType = go (binderType binder)} body
          | otherwise -> TyForAll binder {binderType = go (binderType binder)} (go body)
        TyEq left right -> TyEq (go left) (go right)

isWiredName :: TypeEnv -> Name -> Text -> Bool
isWiredName env name expected =
  case tePrimPackage env of
    Nothing -> False
    Just package ->
      isGhcTypesOrigin package name && nameText name == expected

wiredTypeOf :: TypeEnv -> Name -> Maybe Type
wiredTypeOf env name =
  case tePrimPackage env of
    Nothing -> Nothing
    Just package
      | not (isGhcTypesOrigin package name) -> Nothing
      | nameText name == "Type" -> Just (typeSynonym package)
      | nameText name == "Constraint" -> Just (typeSynonym package)
      | nameText name == "TYPE" ->
          do
            lifted <- liftedRepType env
            Just (TyFun lifted lifted (TyCon (runtimeRepConstructor package)) (typeSynonym package))
      | nameText name == "RuntimeRep" -> Just (typeSynonym package)
      | nameText name == "Levity" -> Just (typeSynonym package)
      | nameText name == "LiftedRep" -> Just (TyCon (runtimeRepConstructor package))
      | nameText name == "UnliftedRep" -> Just (TyCon (runtimeRepConstructor package))
      | nameText name == "BoxedRep" ->
          do
            lifted <- liftedRepType env
            Just (TyFun lifted lifted (TyCon (levityConstructor package)) (TyCon (runtimeRepConstructor package)))
      | nameText name == "Lifted" -> Just (TyCon (levityConstructor package))
      | nameText name == "Unlifted" -> Just (TyCon (levityConstructor package))
      | otherwise -> Nothing

typeAppTYPE :: TypeEnv -> Type -> Type
typeAppTYPE env representation =
  case tePrimPackage env of
    Nothing -> representation
    Just package -> TyApp (TyCon (typeConstructor package)) representation

liftedRepType :: TypeEnv -> Maybe Type
liftedRepType env =
  TyCon . liftedRepName <$> tePrimPackage env

-- | Unfold synonyms and wired Type, then compare structure.
reduceType :: TypeEnv -> Type -> Type
reduceType env ty =
  case ty of
    TyVar {} -> ty
    TyCon {} ->
      let unfolded = unfoldRep env (unfoldType env ty)
       in if unfolded == ty then ty else reduceType env unfolded
    TyApp function argument ->
      case reduceType env function of
        TyForAll binder body ->
          reduceType env (substType (binderName binder) argument body)
        function' -> TyApp function' (reduceType env argument)
    TyFun r1 r2 argument result ->
      TyFun (reduceType env r1) (reduceType env r2) (reduceType env argument) (reduceType env result)
    TyForAll binder body ->
      TyForAll binder {binderType = reduceType env (binderType binder)} (reduceType env body)
    TyEq left right ->
      TyEq (reduceType env left) (reduceType env right)

typesEqual :: TypeEnv -> Type -> Type -> Bool
typesEqual env left right =
  eq (reduceType env left) (reduceType env right)
  where
    eq (TyVar a) (TyVar b) = a == b
    eq (TyCon a) (TyCon b) = a == b
    eq (TyApp function1 argument1) (TyApp function2 argument2) =
      eq function1 function2 && eq argument1 argument2
    eq (TyFun r1a r2a a1 b1) (TyFun r1b r2b a2 b2) =
      eq r1a r1b && eq r2a r2b && eq a1 a2 && eq b1 b2
    eq (TyForAll binder1 body1) (TyForAll binder2 body2) =
      eq (binderType binder1) (binderType binder2)
        && typesEqual env body1 (substType (binderName binder2) (TyVar (binderName binder1)) body2)
    eq (TyEq a1 b1) (TyEq a2 b2) = eq a1 a2 && eq b1 b2
    eq _ _ = False
