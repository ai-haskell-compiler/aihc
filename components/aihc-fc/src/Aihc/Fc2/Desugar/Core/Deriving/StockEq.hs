{-# LANGUAGE OverloadedStrings #-}

-- | Lower finalized stock @Eq@ plans to System FC dictionaries.
module Aihc.Fc2.Desugar.Core.Deriving.StockEq
  ( dsStockEqDictionaryPlan,
  )
where

import Aihc.Fc2.Desugar.Core.Dictionary (classMethodFieldType, predType)
import Aihc.Fc2.Desugar.Core.Expr (ClassDict (..), DsM, desugarBug, dsEvidence, freshVar, withDicts)
import Aihc.Fc2.Desugar.Core.Subst (substType)
import Aihc.Fc2.Desugar.Core.Syntax
import Aihc.Resolve (packageIdText)
import Aihc.Tc.Annotations (TcClassMethodAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcStockDerivingPlan (..))
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), tyConModuleName, tyConPackageId)
import Control.Monad (zipWithM)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

dsStockEqDictionaryPlan :: TcDerivingPlan -> DsM (Var, CoreExpr)
dsStockEqDictionaryPlan plan =
  case (tcDerivingContext plan, tcDerivingDataType plan, tcDerivingStockPlan plan) of
    (TcDerivingExplicitContext context, Just dataType, Just (TcStockEqPlan fieldEvidence)) ->
      dsStockEqDictionary plan context dataType fieldEvidence
    (TcDerivingInferContext, _, _) ->
      desugarBug ("unfinalized stock Eq deriving context for " <> T.unpack (tcDerivingDictName plan))
    (_, Nothing, _) ->
      desugarBug ("missing datatype metadata for stock Eq deriving " <> T.unpack (tcDerivingDictName plan))
    (_, _, Nothing) ->
      desugarBug ("missing checked field evidence for stock Eq deriving " <> T.unpack (tcDerivingDictName plan))

dsStockEqDictionary :: TcDerivingPlan -> [Pred] -> DataTypeInfo -> [[EvTerm]] -> DsM (Var, CoreExpr)
dsStockEqDictionary plan context dataType fieldEvidence = do
  eqTyCon <- stockEqTyCon plan
  contextDicts <- zipWithM mkPredicateDict [0 :: Int ..] context
  genericMethodTypes <-
    mapM
      (classMethodFieldType "Eq" (tcDerivingClassTyVars plan) . tcClassMethodType)
      (tcDerivingClassMethods plan)
  targetType <-
    case reverse (tcDerivingHeadTypes plan) of
      target : _ -> pure target
      [] -> desugarBug "stock Eq deriving plan has an empty instance head"
  constructors <-
    zipExact
      "constructor evidence"
      (dtiConstructors dataType)
      (map (map (rewriteSelfEvidence plan targetType)) fieldEvidence)
  let dictionaryType =
        foldr
          TcForAllTy
          (qualifyType context (TcTyCon eqTyCon [targetType]))
          (tcDerivingTyVars plan)
  dictVar <- freshVar (tcDerivingDictName plan) dictionaryType
  let selfDictionaryType = TcTyCon eqTyCon [targetType]
      selfDictionaryExpression =
        foldl
          CoreApp
          (foldl CoreTyApp (CoreVar dictVar) (map TcTyVar (tcDerivingTyVars plan)))
          (map (CoreVar . classDictVar) contextDicts)
  selfDictionaryVar <- freshVar "$stock_eq_self" selfDictionaryType
  let selfDictionary = ClassDict (tcDerivingClassTyCon plan) [targetType] selfDictionaryVar
  methodFields <-
    withDicts (selfDictionary : contextDicts) $
      mapM
        (dsStockEqMethod eqTyCon dataType constructors targetType)
        (tcDerivingClassMethods plan)
  let classTyVars = tcDerivingClassTyVars plan
      dictionaryConstructor = coreDictionaryConstructorName "Eq"
      genericDictionaryType = TcTyCon eqTyCon (map TcTyVar classTyVars)
      constructorType = foldr TcForAllTy (foldr TcFunTy genericDictionaryType genericMethodTypes) classTyVars
  constructorVar <- freshVar dictionaryConstructor constructorType
  let constructor = foldl CoreTyApp (CoreVar constructorVar) (tcDerivingHeadTypes plan)
      dictionary =
        CoreLet
          (CoreNonRec selfDictionaryVar selfDictionaryExpression)
          (foldl CoreApp constructor methodFields)
      body = foldr CoreTyLam (foldr (CoreLam . classDictVar) dictionary contextDicts) (tcDerivingTyVars plan)
  pure (dictVar, body)

dsStockEqMethod :: TyCon -> DataTypeInfo -> [(DataConInfo, [EvTerm])] -> TcType -> TcClassMethodAnnotation -> DsM CoreExpr
dsStockEqMethod eqTyCon dataType constructors targetType method =
  let boolTy = resultType (tcClassMethodType method)
   in case tcClassMethodName method of
        "==" -> dsEqualityMethod eqTyCon dataType constructors targetType boolTy
        "/=" -> do
          left <- freshVar "$stock_eq_left" targetType
          right <- freshVar "$stock_eq_right" targetType
          equality <- equalityBody eqTyCon dataType constructors targetType boolTy left right
          negated <- negateBoolean boolTy equality
          pure (CoreLam left (CoreLam right negated))
        name -> desugarBug ("unsupported method in stock Eq dictionary: " <> T.unpack name)

dsEqualityMethod :: TyCon -> DataTypeInfo -> [(DataConInfo, [EvTerm])] -> TcType -> TcType -> DsM CoreExpr
dsEqualityMethod eqTyCon dataType constructors targetType boolTy = do
  left <- freshVar "$stock_eq_left" targetType
  right <- freshVar "$stock_eq_right" targetType
  body <- equalityBody eqTyCon dataType constructors targetType boolTy left right
  pure (CoreLam left (CoreLam right body))

equalityBody :: TyCon -> DataTypeInfo -> [(DataConInfo, [EvTerm])] -> TcType -> TcType -> Var -> Var -> DsM CoreExpr
equalityBody eqTyCon dataType constructors targetType boolTy left right = do
  outerBinder <- freshVar "$stock_eq_outer" targetType
  alternatives <- mapM (constructorEqualityAlternative eqTyCon dataType right targetType boolTy) constructors
  pure (CoreCase (CoreVar left) outerBinder alternatives)

constructorEqualityAlternative :: TyCon -> DataTypeInfo -> Var -> TcType -> TcType -> (DataConInfo, [EvTerm]) -> DsM CoreAlt
constructorEqualityAlternative eqTyCon dataType right targetType boolTy (constructor, evidence) = do
  fields <- instantiatedFields dataType targetType constructor
  fieldEvidence <- zipExact ("field evidence for " <> T.unpack (dciName constructor)) fields evidence
  leftFields <- zipWithM (fieldVar "$stock_eq_left_field") [0 :: Int ..] fields
  rightFields <- zipWithM (fieldVar "$stock_eq_right_field") [0 :: Int ..] fields
  comparisons <-
    zipWithM
      (fieldEquality eqTyCon boolTy leftFields rightFields)
      [0 :: Int ..]
      fieldEvidence
  equalFields <- andComparisons boolTy comparisons
  mismatch <- boolConstructor boolTy "False"
  innerBinder <- freshVar "$stock_eq_inner" targetType
  let constructorIdentity = dataConIdentity constructor
      matching = CoreAlt (DataAlt constructorIdentity) rightFields equalFields
      different = CoreAlt DefaultAlt [] mismatch
      compareRight = CoreCase (CoreVar right) innerBinder [matching, different]
  pure (CoreAlt (DataAlt constructorIdentity) leftFields compareRight)

instantiatedFields :: DataTypeInfo -> TcType -> DataConInfo -> DsM [DataConFieldInfo]
instantiatedFields dataType targetType constructor =
  case targetType of
    TcTyCon targetTyCon arguments
      | tyConName targetTyCon == dtiName dataType,
        length arguments == length (dtiTyVars dataType) ->
          let substitution =
                Map.fromList
                  [ (tyVar, argument)
                  | (tyVar, argument) <- zip (dtiTyVars dataType) arguments
                  ]
           in pure [field {dcfiType = substType substitution (dcfiType field)} | field <- dciFields constructor]
    _ -> desugarBug "stock Eq target does not match its datatype metadata"

fieldVar :: Text -> Int -> DataConFieldInfo -> DsM Var
fieldVar prefix index field = freshVar (prefix <> T.pack (show index)) (dcfiType field)

fieldEquality :: TyCon -> TcType -> [Var] -> [Var] -> Int -> (DataConFieldInfo, EvTerm) -> DsM CoreExpr
fieldEquality eqTyCon boolTy leftFields rightFields index (field, evidence) = do
  left <- indexVar "left" index leftFields
  right <- indexVar "right" index rightFields
  dictionary <- dsEvidence evidence
  let fieldType = dcfiType field
      methodType = TcFunTy fieldType (TcFunTy fieldType boolTy)
      dictionaryType = TcTyCon eqTyCon [fieldType]
  dictionaryBinder <- freshVar "$stock_eq_dictionary" dictionaryType
  equalityMethod <- freshVar "$stock_eq_method" methodType
  inequalityMethod <- freshVar "$stock_neq_method" methodType
  pure
    ( CoreCase
        dictionary
        dictionaryBinder
        [ CoreAlt
            (DataAlt (coreConstructorIdFromSymbol (tyConConstructorIdentity eqTyCon (coreDictionaryConstructorName "Eq"))))
            [equalityMethod, inequalityMethod]
            (CoreApp (CoreApp (CoreVar equalityMethod) (CoreVar left)) (CoreVar right))
        ]
    )

indexVar :: String -> Int -> [Var] -> DsM Var
indexVar side index variables =
  case drop index variables of
    variable : _ -> pure variable
    [] -> desugarBug ("missing " <> side <> " stock Eq field binder")

andComparisons :: TcType -> [CoreExpr] -> DsM CoreExpr
andComparisons boolTy comparisons =
  case comparisons of
    [] -> boolConstructor boolTy "True"
    comparison : rest -> do
      binder <- freshVar "$stock_eq_bool" boolTy
      false <- boolConstructor boolTy "False"
      trueBranch <- andComparisons boolTy rest
      let falseConstructor = typeConstructorIdentity boolTy "False"
          trueConstructor = typeConstructorIdentity boolTy "True"
      pure
        ( CoreCase
            comparison
            binder
            [ CoreAlt (DataAlt (coreConstructorIdFromSymbol falseConstructor)) [] false,
              CoreAlt (DataAlt (coreConstructorIdFromSymbol trueConstructor)) [] trueBranch
            ]
        )

negateBoolean :: TcType -> CoreExpr -> DsM CoreExpr
negateBoolean boolTy expression = do
  binder <- freshVar "$stock_eq_not" boolTy
  true <- boolConstructor boolTy "True"
  false <- boolConstructor boolTy "False"
  let falseConstructor = typeConstructorIdentity boolTy "False"
      trueConstructor = typeConstructorIdentity boolTy "True"
  pure
    ( CoreCase
        expression
        binder
        [ CoreAlt (DataAlt (coreConstructorIdFromSymbol falseConstructor)) [] true,
          CoreAlt (DataAlt (coreConstructorIdFromSymbol trueConstructor)) [] false
        ]
    )

boolConstructor :: TcType -> Text -> DsM CoreExpr
boolConstructor boolTy name = do
  constructor <- freshVar name boolTy
  let origin = typeConstructorIdentity boolTy name
  pure (CoreVar constructor {varResolvedName = Just origin})

dataConIdentity :: DataConInfo -> CoreConstructorId
dataConIdentity constructor =
  let (packageId, moduleName) = dciOrigin constructor
   in CoreConstructorId packageId moduleName (dciName constructor)

tyConConstructorIdentity :: TyCon -> Text -> CoreSymbolOrigin
tyConConstructorIdentity tyCon =
  CoreTopLevelOrigin
    (packageIdText (tyConPackageId tyCon))
    (tyConModuleName tyCon)

typeConstructorIdentity :: TcType -> Text -> CoreSymbolOrigin
typeConstructorIdentity ty constructorName =
  case resultType ty of
    TcTyCon tyCon _ -> tyConConstructorIdentity tyCon constructorName
    _ -> CoreBuiltinOrigin constructorName

resultType :: TcType -> TcType
resultType ty =
  case ty of
    TcForAllTy _ body -> resultType body
    TcQualTy _ body -> resultType body
    TcFunTy _ result -> resultType result
    result -> result

stockEqTyCon :: TcDerivingPlan -> DsM TyCon
stockEqTyCon plan =
  case tcDerivingClassMethods plan of
    method : _ ->
      case tcClassMethodDictType method of
        TcTyCon tyCon [_]
          | tyCon == tcDerivingClassTyCon plan -> pure tyCon
        ty -> desugarBug ("invalid stock Eq dictionary type: " <> show ty)
    [] -> desugarBug "stock Eq has no class methods"

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

rewriteSelfEvidence :: TcDerivingPlan -> TcType -> EvTerm -> EvTerm
rewriteSelfEvidence plan targetType evidence =
  case evidence of
    EvDict _ dictionaryName _ _
      | dictionaryName == tcDerivingDictName plan ->
          EvGiven (ClassPred (tcDerivingClassTyCon plan) [targetType])
    EvDict origin dictionaryName typeArguments contextEvidence ->
      EvDict origin dictionaryName typeArguments (map recurse contextEvidence)
    EvSuperClass source sourceOrigin sourcePredicate fieldTypes fieldIndex ->
      EvSuperClass (recurse source) sourceOrigin sourcePredicate fieldTypes fieldIndex
    EvCast source coercion -> EvCast (recurse source) coercion
    EvTypeable origin ty arguments -> EvTypeable origin ty (map recurse arguments)
    _ -> evidence
  where
    recurse = rewriteSelfEvidence plan targetType

zipExact :: String -> [left] -> [right] -> DsM [(left, right)]
zipExact context left right
  | length left == length right = pure (zip left right)
  | otherwise = desugarBug (context <> " arity mismatch")
