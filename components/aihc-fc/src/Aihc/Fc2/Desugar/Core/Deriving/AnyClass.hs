{-# LANGUAGE OverloadedStrings #-}

-- | Lower finalized @DeriveAnyClass@ plans to System FC dictionaries.
module Aihc.Fc2.Desugar.Core.Deriving.AnyClass
  ( dsAnyClassDictionaryPlan,
  )
where

import Aihc.Fc2.Desugar.Core.Dictionary (checkedConstraintType, classMethodFieldType, defaultMethodName, predType)
import Aihc.Fc2.Desugar.Core.Expr (ClassDict (..), DsM, desugarBug, dsEvidence, freshUnique, freshVar, lookupType, withDicts)
import Aihc.Fc2.Desugar.Core.Syntax
import Aihc.Resolve (packageIdText)
import Aihc.Tc.Annotations (TcClassMethodAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDictBinderAnnotation (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), tyConModuleName, tyConPackageId)
import Control.Monad (when, zipWithM)
import Data.Text qualified as T

dsAnyClassDictionaryPlan :: TcDerivingPlan -> DsM (Var, CoreExpr)
dsAnyClassDictionaryPlan plan =
  case tcDerivingContext plan of
    TcDerivingExplicitContext context -> dsAnyClassDictionary plan context
    TcDerivingInferContext ->
      desugarBug ("unfinalized AnyClass deriving context for " <> T.unpack (tcDerivingDictName plan))

dsAnyClassDictionary :: TcDerivingPlan -> [Pred] -> DsM (Var, CoreExpr)
dsAnyClassDictionary plan context = do
  when
    (length (tcDerivingSuperClasses plan) /= length (tcDerivingClassSuperClasses plan))
    (desugarBug ("incomplete superclass evidence for " <> T.unpack (tcDerivingDictName plan)))
  contextDicts <- zipWithM mkPredicateDict [0 :: Int ..] context
  methodFieldTypes <-
    mapM
      (classMethodFieldType (tcDerivingClassName plan) (tcDerivingClassTyVars plan) . tcClassMethodType)
      (tcDerivingClassMethods plan)
  superClassFieldTypes <- mapM (checkedConstraintType "derived class superclass" . tcDictBinderType) (tcDerivingClassSuperClasses plan)
  let fieldTypes = superClassFieldTypes <> methodFieldTypes
      dictionaryType =
        foldr
          TcForAllTy
          (qualifyType context (TcTyCon (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan)))
          (tcDerivingTyVars plan)
      selfDictionary dictVar =
        foldl
          CoreApp
          (foldl CoreTyApp (CoreVar dictVar) (map TcTyVar (tcDerivingTyVars plan)))
          (map (CoreVar . classDictVar) contextDicts)
      usesDefaultMethod =
        any
          ((`elem` tcDerivingDefaultMethods plan) . tcClassMethodName)
          (tcDerivingClassMethods plan)
  dictVar <- freshVar (tcDerivingDictName plan) dictionaryType
  let maybeSelfDictionary
        | usesDefaultMethod = Just (selfDictionary dictVar)
        | otherwise = Nothing
  fields <-
    withDicts contextDicts $ do
      superClassFields <- mapM (dsEvidence . snd) (tcDerivingSuperClasses plan)
      methodFields <-
        zipWithM
          (dsAnyClassMethod plan maybeSelfDictionary)
          methodFieldTypes
          (tcDerivingClassMethods plan)
      pure (superClassFields <> methodFields)
  constructorUnique <- freshUnique
  let classTyVars = tcDerivingClassTyVars plan
      dictionaryConstructor = coreDictionaryConstructorName (tcDerivingClassName plan)
      genericDictionaryType = TcTyCon (tcDerivingClassTyCon plan) (map TcTyVar classTyVars)
      constructorType = foldr TcForAllTy (foldr TcFunTy genericDictionaryType fieldTypes) classTyVars
      constructorVar =
        (Var dictionaryConstructor constructorUnique constructorType)
          { varResolvedName =
              Just
                ( CoreTopLevelOrigin
                    (packageIdText (tyConPackageId (tcDerivingClassTyCon plan)))
                    (tyConModuleName (tcDerivingClassTyCon plan))
                    dictionaryConstructor
                )
          }
      constructor = foldl CoreTyApp (CoreVar constructorVar) (tcDerivingHeadTypes plan)
      dictionary = foldl CoreApp constructor fields
      body = foldr CoreTyLam (foldr (CoreLam . classDictVar) dictionary contextDicts) (tcDerivingTyVars plan)
  pure (dictVar, body)

dsAnyClassMethod :: TcDerivingPlan -> Maybe CoreExpr -> TcType -> TcClassMethodAnnotation -> DsM CoreExpr
dsAnyClassMethod plan maybeSelfDictionary fieldType method =
  if methodName `elem` tcDerivingDefaultMethods plan
    then do
      selfDictionary <-
        case maybeSelfDictionary of
          Just dictionary -> pure dictionary
          Nothing -> desugarBug ("default method " <> T.unpack methodName <> " requires a recursive derived dictionary")
      let workerName = defaultMethodName methodName
      workerType <- lookupType workerName
      worker <- freshVar workerName workerType
      let workerOrigin = fmap (\(packageName, moduleName) -> CoreTopLevelOrigin packageName moduleName workerName) (tcDerivingClassOrigin plan)
          resolvedWorker = worker {varResolvedName = workerOrigin}
      checkedEvidence <-
        case lookup methodName (tcDerivingDefaultMethodEvidence plan) of
          Just terms -> pure terms
          Nothing
            | methodName `elem` map fst (tcDerivingDefaultSignatures plan) ->
                desugarBug ("missing default-signature evidence for " <> T.unpack methodName)
            | otherwise -> pure []
      evidence <- mapM dsEvidence checkedEvidence
      pure
        ( foldl
            CoreApp
            (CoreApp (foldl CoreTyApp (CoreVar resolvedWorker) (tcDerivingHeadTypes plan)) selfDictionary)
            evidence
        )
    else do
      -- A method omitted by DeriveAnyClass denotes bottom. Keep that fact in
      -- FC without introducing a dependency on a particular exception runtime.
      missingMethod <- freshVar ("$missing$" <> methodName) fieldType
      pure (CoreLet (CoreRec [(missingMethod, CoreVar missingMethod)]) (CoreVar missingMethod))
  where
    methodName = tcClassMethodName method

mkPredicateDict :: Int -> Pred -> DsM ClassDict
mkPredicateDict index predicate = do
  dictVar <- freshVar ("$d" <> T.pack (show index)) (predType predicate)
  pure $
    case predicate of
      ClassPred className arguments -> ClassDict className arguments dictVar
      EqPred {} -> ClassDict (TyCon "<equality>" 0) [] dictVar

qualifyType :: [Pred] -> TcType -> TcType
qualifyType [] body = body
qualifyType predicates body = TcQualTy predicates body
