{-# LANGUAGE OverloadedStrings #-}

-- | Lower finalized @DeriveAnyClass@ plans to System FC dictionaries.
module Aihc.Fc.Desugar.Deriving.AnyClass
  ( dsAnyClassDictionaryPlan,
  )
where

import Aihc.Fc.Desugar.Dictionary (classMethodFieldType, defaultMethodName, predType)
import Aihc.Fc.Desugar.Expr (ClassDict (..), DsM, desugarBug, dsEvidence, freshUnique, freshVar, lookupType, withDicts)
import Aihc.Fc.Syntax
import Aihc.Tc.Annotations (TcClassMethodAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcDictBinderAnnotation (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..))
import Control.Monad (when, zipWithM)
import Data.Text qualified as T

dsAnyClassDictionaryPlan :: TcDerivingPlan -> DsM (Var, FcExpr)
dsAnyClassDictionaryPlan plan =
  case tcDerivingContext plan of
    TcDerivingExplicitContext context -> dsAnyClassDictionary plan context
    TcDerivingInferContext ->
      desugarBug ("unfinalized AnyClass deriving context for " <> T.unpack (tcDerivingDictName plan))

dsAnyClassDictionary :: TcDerivingPlan -> [Pred] -> DsM (Var, FcExpr)
dsAnyClassDictionary plan context = do
  when
    (length (tcDerivingSuperClasses plan) /= length (tcDerivingClassSuperClasses plan))
    (desugarBug ("incomplete superclass evidence for " <> T.unpack (tcDerivingDictName plan)))
  contextDicts <- zipWithM mkPredicateDict [0 :: Int ..] context
  methodFieldTypes <-
    mapM
      (classMethodFieldType (tcDerivingClassName plan) (tcDerivingClassTyVars plan) . tcClassMethodType)
      (tcDerivingClassMethods plan)
  let superClassFieldTypes = map tcDictBinderType (tcDerivingClassSuperClasses plan)
      fieldTypes = superClassFieldTypes <> methodFieldTypes
      dictionaryType =
        foldr
          TcForAllTy
          (qualifyType context (TcTyCon (TyCon (tcDerivingClassName plan) (length (tcDerivingClassTyVars plan))) (tcDerivingHeadTypes plan)))
          (tcDerivingTyVars plan)
      selfDictionary dictVar =
        foldl
          FcApp
          (foldl FcTyApp (FcVar dictVar) (map TcTyVar (tcDerivingTyVars plan)))
          (map (FcVar . classDictVar) contextDicts)
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
      dictionaryConstructor = fcDictionaryConstructorName (tcDerivingClassName plan)
      genericDictionaryType = TcTyCon (TyCon (tcDerivingClassName plan) (length classTyVars)) (map TcTyVar classTyVars)
      constructorType = foldr TcForAllTy (foldr TcFunTy genericDictionaryType fieldTypes) classTyVars
      constructorVar = Var dictionaryConstructor constructorUnique constructorType
      constructor = foldl FcTyApp (FcVar constructorVar) (tcDerivingHeadTypes plan)
      dictionary = foldl FcApp constructor fields
      body = foldr FcTyLam (foldr (FcLam . classDictVar) dictionary contextDicts) (tcDerivingTyVars plan)
  pure (dictVar, body)

dsAnyClassMethod :: TcDerivingPlan -> Maybe FcExpr -> TcType -> TcClassMethodAnnotation -> DsM FcExpr
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
      let workerOrigin = fmap (\(packageName, moduleName) -> FcTopLevelOrigin packageName moduleName workerName) (tcDerivingClassOrigin plan)
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
            FcApp
            (FcApp (foldl FcTyApp (FcVar resolvedWorker) (tcDerivingHeadTypes plan)) selfDictionary)
            evidence
        )
    else do
      -- A method omitted by DeriveAnyClass denotes bottom. Keep that fact in
      -- FC without introducing a dependency on a particular exception runtime.
      missingMethod <- freshVar ("$missing$" <> methodName) fieldType
      pure (FcLet (FcRec [(missingMethod, FcVar missingMethod)]) (FcVar missingMethod))
  where
    methodName = tcClassMethodName method

mkPredicateDict :: Int -> Pred -> DsM ClassDict
mkPredicateDict index predicate = do
  dictVar <- freshVar ("$d" <> T.pack (show index)) (predType predicate)
  pure $
    case predicate of
      ClassPred className arguments -> ClassDict className arguments dictVar
      EqPred {} -> ClassDict "<equality>" [] dictVar

qualifyType :: [Pred] -> TcType -> TcType
qualifyType [] body = body
qualifyType predicates body = TcQualTy predicates body
