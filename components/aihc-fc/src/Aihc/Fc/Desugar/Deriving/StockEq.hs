{-# LANGUAGE OverloadedStrings #-}

-- | Lower finalized stock @Eq@ plans to System FC dictionaries.
module Aihc.Fc.Desugar.Deriving.StockEq
  ( dsStockEqDictionaryPlan,
  )
where

import Aihc.Fc.Desugar.Dictionary (classMethodFieldType, predType)
import Aihc.Fc.Desugar.Expr (ClassDict (..), DsM, desugarBug, dsEvidence, freshVar, lookupType, withDicts)
import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Annotations (TcClassMethodAnnotation (..), TcDerivingContext (..), TcDerivingPlan (..), TcStockDerivingPlan (..))
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..))
import Control.Monad (zipWithM)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

dsStockEqDictionaryPlan :: TcDerivingPlan -> DsM (Var, FcExpr)
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

dsStockEqDictionary :: TcDerivingPlan -> [Pred] -> DataTypeInfo -> [[EvTerm]] -> DsM (Var, FcExpr)
dsStockEqDictionary plan context dataType fieldEvidence = do
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
          (qualifyType context (TcTyCon (TyCon "Eq" 1) [targetType]))
          (tcDerivingTyVars plan)
  dictVar <- freshVar (tcDerivingDictName plan) dictionaryType
  let selfDictionaryType = TcTyCon (TyCon "Eq" 1) [targetType]
      selfDictionaryExpression =
        foldl
          FcApp
          (foldl FcTyApp (FcVar dictVar) (map TcTyVar (tcDerivingTyVars plan)))
          (map (FcVar . classDictVar) contextDicts)
  selfDictionaryVar <- freshVar "$stock_eq_self" selfDictionaryType
  let selfDictionary = ClassDict "Eq" [targetType] selfDictionaryVar
  methodFields <-
    withDicts (selfDictionary : contextDicts) $
      mapM
        (dsStockEqMethod dataType constructors targetType)
        (tcDerivingClassMethods plan)
  let classTyVars = tcDerivingClassTyVars plan
      dictionaryConstructor = fcDictionaryConstructorName "Eq"
      genericDictionaryType = TcTyCon (TyCon "Eq" 1) (map TcTyVar classTyVars)
      constructorType = foldr TcForAllTy (foldr TcFunTy genericDictionaryType genericMethodTypes) classTyVars
  constructorVar <- freshVar dictionaryConstructor constructorType
  let constructor = foldl FcTyApp (FcVar constructorVar) (tcDerivingHeadTypes plan)
      dictionary =
        FcLet
          (FcNonRec selfDictionaryVar selfDictionaryExpression)
          (foldl FcApp constructor methodFields)
      body = foldr FcTyLam (foldr (FcLam . classDictVar) dictionary contextDicts) (tcDerivingTyVars plan)
  pure (dictVar, body)

dsStockEqMethod :: DataTypeInfo -> [(DataConInfo, [EvTerm])] -> TcType -> TcClassMethodAnnotation -> DsM FcExpr
dsStockEqMethod dataType constructors targetType method =
  case tcClassMethodName method of
    "==" -> dsEqualityMethod dataType constructors targetType
    "/=" -> do
      left <- freshVar "$stock_eq_left" targetType
      right <- freshVar "$stock_eq_right" targetType
      equality <- equalityBody dataType constructors targetType left right
      negated <- negateBoolean equality
      pure (FcLam left (FcLam right negated))
    name -> desugarBug ("unsupported method in stock Eq dictionary: " <> T.unpack name)

dsEqualityMethod :: DataTypeInfo -> [(DataConInfo, [EvTerm])] -> TcType -> DsM FcExpr
dsEqualityMethod dataType constructors targetType = do
  left <- freshVar "$stock_eq_left" targetType
  right <- freshVar "$stock_eq_right" targetType
  body <- equalityBody dataType constructors targetType left right
  pure (FcLam left (FcLam right body))

equalityBody :: DataTypeInfo -> [(DataConInfo, [EvTerm])] -> TcType -> Var -> Var -> DsM FcExpr
equalityBody dataType constructors targetType left right = do
  outerBinder <- freshVar "$stock_eq_outer" targetType
  alternatives <- mapM (constructorEqualityAlternative dataType right targetType) constructors
  pure (FcCase (FcVar left) outerBinder alternatives)

constructorEqualityAlternative :: DataTypeInfo -> Var -> TcType -> (DataConInfo, [EvTerm]) -> DsM FcAlt
constructorEqualityAlternative dataType right targetType (constructor, evidence) = do
  fields <- instantiatedFields dataType targetType constructor
  fieldEvidence <- zipExact ("field evidence for " <> T.unpack (dciName constructor)) fields evidence
  leftFields <- zipWithM (fieldVar "$stock_eq_left_field") [0 :: Int ..] fields
  rightFields <- zipWithM (fieldVar "$stock_eq_right_field") [0 :: Int ..] fields
  comparisons <-
    zipWithM
      (fieldEquality leftFields rightFields)
      [0 :: Int ..]
      fieldEvidence
  equalFields <- andComparisons comparisons
  mismatch <- boolConstructor "False"
  innerBinder <- freshVar "$stock_eq_inner" targetType
  let matching = FcAlt (DataAlt (dciName constructor)) rightFields equalFields
      different = FcAlt DefaultAlt [] mismatch
      compareRight = FcCase (FcVar right) innerBinder [matching, different]
  pure (FcAlt (DataAlt (dciName constructor)) leftFields compareRight)

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

fieldEquality :: [Var] -> [Var] -> Int -> (DataConFieldInfo, EvTerm) -> DsM FcExpr
fieldEquality leftFields rightFields index (field, evidence) = do
  left <- indexVar "left" index leftFields
  right <- indexVar "right" index rightFields
  dictionary <- dsEvidence evidence
  let fieldType = dcfiType field
      methodType = TcFunTy fieldType (TcFunTy fieldType boolType)
      dictionaryType = TcTyCon (TyCon "Eq" 1) [fieldType]
  dictionaryBinder <- freshVar "$stock_eq_dictionary" dictionaryType
  equalityMethod <- freshVar "$stock_eq_method" methodType
  inequalityMethod <- freshVar "$stock_neq_method" methodType
  pure
    ( FcCase
        dictionary
        dictionaryBinder
        [ FcAlt
            (DataAlt (fcDictionaryConstructorName "Eq"))
            [equalityMethod, inequalityMethod]
            (FcApp (FcApp (FcVar equalityMethod) (FcVar left)) (FcVar right))
        ]
    )

indexVar :: String -> Int -> [Var] -> DsM Var
indexVar side index variables =
  case drop index variables of
    variable : _ -> pure variable
    [] -> desugarBug ("missing " <> side <> " stock Eq field binder")

andComparisons :: [FcExpr] -> DsM FcExpr
andComparisons comparisons =
  case comparisons of
    [] -> boolConstructor "True"
    comparison : rest -> do
      binder <- freshVar "$stock_eq_bool" boolType
      false <- boolConstructor "False"
      trueBranch <- andComparisons rest
      pure
        ( FcCase
            comparison
            binder
            [ FcAlt (DataAlt "False") [] false,
              FcAlt (DataAlt "True") [] trueBranch
            ]
        )

negateBoolean :: FcExpr -> DsM FcExpr
negateBoolean expression = do
  binder <- freshVar "$stock_eq_not" boolType
  true <- boolConstructor "True"
  false <- boolConstructor "False"
  pure
    ( FcCase
        expression
        binder
        [ FcAlt (DataAlt "False") [] true,
          FcAlt (DataAlt "True") [] false
        ]
    )

boolConstructor :: Text -> DsM FcExpr
boolConstructor name = do
  constructorType <- lookupType name
  constructor <- freshVar name constructorType
  pure (FcVar constructor)

boolType :: TcType
boolType = TcTyCon (TyCon "Bool" 0) []

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

rewriteSelfEvidence :: TcDerivingPlan -> TcType -> EvTerm -> EvTerm
rewriteSelfEvidence plan targetType evidence =
  case evidence of
    EvDict _ dictionaryName _ _
      | dictionaryName == tcDerivingDictName plan ->
          EvGiven (ClassPred "Eq" [targetType])
    EvDict origin dictionaryName typeArguments contextEvidence ->
      EvDict origin dictionaryName typeArguments (map recurse contextEvidence)
    EvSuperClass source sourcePredicate fieldTypes fieldIndex ->
      EvSuperClass (recurse source) sourcePredicate fieldTypes fieldIndex
    EvCast source coercion -> EvCast (recurse source) coercion
    EvTypeable ty arguments -> EvTypeable ty (map recurse arguments)
    _ -> evidence
  where
    recurse = rewriteSelfEvidence plan targetType

zipExact :: String -> [left] -> [right] -> DsM [(left, right)]
zipExact context left right
  | length left == length right = pure (zip left right)
  | otherwise = desugarBug (context <> " arity mismatch")
