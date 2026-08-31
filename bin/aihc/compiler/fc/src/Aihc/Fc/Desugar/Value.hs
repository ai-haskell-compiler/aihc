{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Direct value desugaring from checked source syntax to System FC.
module Aihc.Fc.Desugar.Value
  ( desugarValues,
    mergePreparedValueInterfaces,
    prepareValueInterface,
    PreparedValueInterface,
  )
where

import Aihc.Fc.Convert
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax qualified as Syn
import Aihc.Resolve
  ( Identifier (..),
    PackageId (..),
    ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolvedName (..),
    displayIdentifier,
    packageIdText,
  )
import Aihc.Tc
  ( DataConFieldInfo (..),
    DataConInfo (..),
    DataTypeInfo (..),
    TcBindingResult (..),
    TcInterface (..),
    TyConFlavor (..),
  )
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcForeignAbiType (..),
    TcForeignEffect (..),
    TcForeignImportAnnotation (..),
    TcForeignMarshal (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
  )
import Aihc.Tc.Evidence qualified as Ev
import Aihc.Tc.Solve.Dict (constraintTypeToPred, matchTypes)
import Aihc.Tc.Types
  ( Pred (..),
    TcType (..),
    TyCon,
    TyVarId,
    Unique (..),
    applySubst,
    applySubstPred,
    runtimeRepOfTypeInEnv,
    tvUnique,
    tyConModuleName,
    tyConName,
    tyConPackageId,
    typeKindType,
    pattern AddrRep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern Int8Rep,
    pattern IntRep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern Word8Rep,
    pattern WordRep,
  )
import Control.Applicative ((<|>))
import Control.Monad (unless, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data ValueState = ValueState
  { vsNextUnique :: !Int,
    vsModuleOrigin :: !(PackageId, Text),
    vsConvertEnv :: !ConvertEnv,
    vsTypes :: !(Map Text TcType),
    vsLocals :: !(Map Text (Binder, TcType)),
    vsBinderTypes :: !(Map Name TcType),
    vsDictionaries :: !(Map Text Binder),
    vsConstructors :: !(Map Text [Name]),
    vsConstructorInfos :: !(Map Text [DataConInfo]),
    vsNewtypeConstructors :: !(Map (PackageId, Text, Text) DataTypeInfo)
  }

data PreparedValueInterface = PreparedValueInterface
  { preparedTypes :: !(Map Text TcType),
    preparedConstructors :: !(Map Text [Name]),
    preparedConstructorInfos :: !(Map Text [DataConInfo]),
    preparedNewtypeConstructors :: !(Map (PackageId, Text, Text) DataTypeInfo)
  }

type ValueM = StateT ValueState (Either String)

data ValueGroup
  = FunctionGroup !Text ![Syn.Match] !TcType
  | PatternGroup !Text !Syn.Pattern !(Syn.Rhs Syn.Expr) !TcType

data TopValue = TopValue
  { topCoreName :: !Name,
    topType :: !TcType,
    topGroup :: !ValueGroup
  }

data Dictionary = Dictionary
  { dictionaryPredicate :: !Pred,
    dictionaryBinder :: !Binder
  }

prepareValueInterface :: [TcBindingResult] -> TcInterface -> PreparedValueInterface
prepareValueInterface bindings interface =
  PreparedValueInterface
    { preparedTypes = Map.fromList [(tbName binding, tbType binding) | binding <- bindings],
      preparedConstructors = constructors,
      preparedConstructorInfos = constructorInfos,
      preparedNewtypeConstructors = newtypes
    }
  where
    constructors =
      Map.fromListWith
        (<>)
        [ (dciName constructor, [Name (dciName constructor) SortDataConstructor (OriginTop package moduleName')])
        | dataType <- tcInterfaceDataTypes interface,
          constructor <- dtiConstructors dataType,
          let (package, moduleName') = dciOrigin constructor
        ]

    constructorInfos =
      Map.fromListWith
        (<>)
        [ (dciName constructor, [constructor])
        | dataType <- tcInterfaceDataTypes interface,
          constructor <- dtiConstructors dataType
        ]
    newtypes =
      Map.fromList
        [ ((package, moduleName', dciName constructor), dataType)
        | dataType <- tcInterfaceDataTypes interface,
          dtiFlavor dataType == NewtypeTyCon,
          constructor <- dtiConstructors dataType,
          let (package, moduleName') = dciOrigin constructor
        ]

mergePreparedValueInterfaces :: [PreparedValueInterface] -> PreparedValueInterface
mergePreparedValueInterfaces interfaces =
  PreparedValueInterface
    { preparedTypes = Map.unions (map preparedTypes interfaces),
      preparedConstructors = Map.unionsWith mergeCandidates (map preparedConstructors interfaces),
      preparedConstructorInfos = Map.unionsWith mergeCandidates (map preparedConstructorInfos interfaces),
      preparedNewtypeConstructors = Map.unions (map preparedNewtypeConstructors interfaces)
    }
  where
    mergeCandidates left right = List.nub (left <> right)

desugarValues :: ConvertEnv -> [TcBindingResult] -> PreparedValueInterface -> (PackageId, Text) -> Syn.Module -> Either String [Decl]
desugarValues convertEnv bindings interface moduleOrigin checked = do
  let typeEntries = Map.fromList [(tbName binding, tbType binding) | binding <- bindings] `Map.union` preparedTypes interface
      initialState =
        ValueState
          { vsNextUnique = 1000,
            vsModuleOrigin = moduleOrigin,
            vsConvertEnv = convertEnv,
            vsTypes = typeEntries,
            vsLocals = Map.empty,
            vsBinderTypes = Map.empty,
            vsDictionaries = Map.empty,
            vsConstructors = preparedConstructors interface,
            vsConstructorInfos = preparedConstructorInfos interface,
            vsNewtypeConstructors = preparedNewtypeConstructors interface
          }
  fst <$> runStateT (desugarModuleValues checked) initialState

desugarModuleValues :: Syn.Module -> ValueM [Decl]
desugarModuleValues checked = do
  phaseOne <- concat <$> mapM desugarEarlyDecl (Syn.moduleDecls checked)
  instances <- concat <$> mapM desugarInstanceDecl (Syn.moduleDecls checked)
  localGroups <- groupValues (Syn.moduleDecls checked)
  groups <- mapM useTopLevelType localGroups
  tops <- mapM allocateTopValue groups
  values <- mapM desugarTopValue tops
  pure (phaseOne <> instances <> map DeclVal values)

useTopLevelType :: ValueGroup -> ValueM ValueGroup
useTopLevelType group = do
  ty <- lookupCheckedType (groupName group)
  pure $ case group of
    FunctionGroup name matches _ -> FunctionGroup name matches ty
    PatternGroup name pattern' rhs _ -> PatternGroup name pattern' rhs ty

desugarEarlyDecl :: Syn.Decl -> ValueM [Decl]
desugarEarlyDecl declaration =
  case annotatedForeignDecl declaration of
    Just (annotation, foreignPlan, foreignDecl) -> desugarForeign annotation foreignPlan foreignDecl
    Nothing ->
      case declaration of
        Syn.DeclAnn annotation inner
          | Just classAnnotation <- Syn.fromAnnotation annotation,
            Syn.DeclClass classDecl <- Syn.peelDeclAnn inner ->
              (<>)
                <$> desugarClassSelectors classDecl classAnnotation
                <*> desugarClassDefaults classDecl
          | otherwise -> desugarEarlyDecl inner
        _ -> pure []

annotatedForeignDecl :: Syn.Decl -> Maybe (TcAnnotation, Maybe TcForeignImportAnnotation, Syn.ForeignDecl)
annotatedForeignDecl = go Nothing Nothing
  where
    go maybeType maybePlan declaration =
      case declaration of
        Syn.DeclAnn annotation inner ->
          go
            ((Syn.fromAnnotation annotation :: Maybe TcAnnotation) <|> maybeType)
            ((Syn.fromAnnotation annotation :: Maybe TcForeignImportAnnotation) <|> maybePlan)
            inner
        Syn.DeclForeign foreignDecl -> (,,foreignDecl) <$> maybeType <*> pure maybePlan
        _ -> Nothing

desugarForeign :: TcAnnotation -> Maybe TcForeignImportAnnotation -> Syn.ForeignDecl -> ValueM [Decl]
desugarForeign annotation foreignPlan foreignDecl =
  case Syn.foreignCallConv foreignDecl of
    Syn.CPrim -> (: []) <$> makeForeignImport Prim []
    Syn.CCall -> do
      unless (Syn.foreignDirection foreignDecl == Syn.ForeignImport) (failValue "System FC does not accept foreign exports")
      unless (Syn.foreignSafety foreignDecl == Just Syn.Unsafe) (failValue "System FC accepts only unsafe foreign imports")
      plan <- maybe (failValue "missing checked foreign import plan") pure foreignPlan
      symbol <- foreignSymbol foreignDecl
      dependencies <- foreignImportPlanDependencies annotation plan
      let convention =
            CCall
              CCallSpec
                { ccallSymbol = symbol,
                  ccallArgumentTypes = map (convertCAbiType . tcForeignAbiType) (tcForeignArguments plan),
                  ccallResultType = convertCAbiType (tcForeignAbiType (tcForeignResult plan)),
                  ccallEffect = convertForeignEffect (tcForeignEffect plan)
                }
      (: []) <$> makeForeignImport convention dependencies
    callConv -> failValue ("unsupported System FC foreign calling convention: " <> show callConv)
  where
    makeForeignImport convention dependencies = do
      _ <- freshUnique
      moduleOrigin <- gets vsModuleOrigin
      ty <- convertCheckedType (tcAnnType annotation)
      let valueName = Syn.unqualifiedNameText (Syn.foreignName foreignDecl)
      pure
        ( DeclForeignImport
            ForeignImportDecl
              { foreignImportVis = Pub,
                foreignImportName = topName moduleOrigin valueName,
                foreignImportCallingConvention = convention,
                foreignImportDependencies = dependencies,
                foreignImportType = ty
              }
        )

foreignImportPlanDependencies :: TcAnnotation -> TcForeignImportAnnotation -> ValueM [ForeignImportDependency]
foreignImportPlanDependencies annotation plan = do
  typeDependencies <- foreignTypeNewtypeDependencies (tcAnnType annotation)
  marshalDependencies <- concat <$> mapM foreignMarshalDependencies (tcForeignArguments plan <> [tcForeignResult plan])
  pure (List.nub (typeDependencies <> marshalDependencies))

foreignTypeNewtypeDependencies :: TcType -> ValueM [ForeignImportDependency]
foreignTypeNewtypeDependencies ty = do
  newtypes <- List.nub . Map.elems <$> gets vsNewtypeConstructors
  pure (go newtypes ty)
  where
    go newtypes current =
      case current of
        TcTyVar {} -> []
        TcMetaTv {} -> []
        TcTyCon tyCon arguments ->
          [foreignNewtypeDependency dataType | dataType <- newtypes, dtiTyCon dataType == tyCon]
            <> concatMap (go newtypes) arguments
        TcFunTy argument result -> go newtypes argument <> go newtypes result
        TcForAllTy _ body -> go newtypes body
        TcQualTy _ body -> go newtypes body
        TcAppTy function argument -> go newtypes function <> go newtypes argument

foreignMarshalDependencies :: TcForeignMarshal -> ValueM [ForeignImportDependency]
foreignMarshalDependencies marshal = go (tcForeignSourceType marshal) (tcForeignConstructors marshal)
  where
    go _ [] = pure []
    go sourceType (constructorName : rest) = do
      newtypes <- List.nub . Map.elems <$> gets vsNewtypeConstructors
      constructors <- Map.findWithDefault [] constructorName <$> gets vsConstructorInfos
      case [(dataType, constructor, fieldType) | dataType <- newtypes, constructor <- dtiConstructors dataType, dciName constructor == constructorName, Just fieldType <- [foreignConstructorField sourceType constructor]] of
        [(dataType, _, fieldType)] ->
          (foreignNewtypeDependency dataType :) <$> go fieldType rest
        [] ->
          case [(constructor, fieldType) | constructor <- constructors, Just fieldType <- [foreignConstructorField sourceType constructor]] of
            [(constructor, fieldType)] ->
              let (package, moduleName) = dciOrigin constructor
                  dependency = ForeignConstructor (Name constructorName SortDataConstructor (OriginTop package moduleName))
               in (dependency :) <$> go fieldType rest
            [] -> failValue ("missing checked foreign constructor " <> T.unpack constructorName)
            _ -> failValue ("ambiguous checked foreign constructor " <> T.unpack constructorName)
        _ -> failValue ("ambiguous checked foreign newtype constructor " <> T.unpack constructorName)

foreignConstructorField :: TcType -> DataConInfo -> Maybe TcType
foreignConstructorField sourceType constructor = do
  substitution <- matchTypes [dciResTy constructor] [sourceType]
  case dciFields constructor of
    [field] -> pure (applySubst substitution (dcfiType field))
    _ -> Nothing

foreignNewtypeDependency :: DataTypeInfo -> ForeignImportDependency
foreignNewtypeDependency dataType =
  let tyCon = dtiTyCon dataType
   in ForeignAxiom (Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)))

foreignSymbol :: Syn.ForeignDecl -> ValueM Text
foreignSymbol foreignDecl =
  case Syn.foreignEntity foreignDecl of
    Syn.ForeignEntityNamed name -> pure name
    Syn.ForeignEntityStatic (Just name) -> pure name
    Syn.ForeignEntityOmitted -> pure (Syn.unqualifiedNameText (Syn.foreignName foreignDecl))
    _ -> failValue "System FC accepts only statically named foreign imports"

convertCAbiType :: TcForeignAbiType -> CAbiType
convertCAbiType abiType =
  case abiType of
    TcForeignInt -> CAbiInt
    TcForeignInt32 -> CAbiInt32
    TcForeignWord64 -> CAbiWord64
    TcForeignAddr -> CAbiAddr

convertForeignEffect :: TcForeignEffect -> ForeignEffect
convertForeignEffect effect =
  case effect of
    TcForeignPure -> ForeignPure
    TcForeignRealWorld -> ForeignRealWorld

desugarClassSelectors :: Syn.ClassDecl -> TcClassAnnotation -> ValueM [Decl]
desugarClassSelectors classDecl classAnnotation = do
  let classTyVars = tcClassKindTyVars classAnnotation <> tcClassTyVars classAnnotation
      className = Syn.unqualifiedNameText (Syn.binderHeadName (Syn.classDeclHead classDecl))
  methodTypes <- mapM (methodFieldType className classTyVars) (tcClassMethods classAnnotation)
  let fieldTypes = map tcDictBinderType (tcClassSuperClasses classAnnotation) <> methodTypes
      superClassCount = length (tcClassSuperClasses classAnnotation)
  mapM (desugarSelector (tcClassTyCon classAnnotation) classTyVars fieldTypes superClassCount) (tcClassMethods classAnnotation)

desugarClassDefaults :: Syn.ClassDecl -> ValueM [Decl]
desugarClassDefaults classDecl =
  concat <$> mapM (defaultItem Nothing) (Syn.classDeclItems classDecl)
  where
    defaultItem maybeAnnotation item =
      case item of
        Syn.ClassItemAnn annotation inner ->
          defaultItem
            ((Syn.fromAnnotation annotation :: Maybe TcInstanceMethodAnnotation) <|> maybeAnnotation)
            inner
        Syn.ClassItemDefault valueDecl ->
          case maybeAnnotation of
            Just annotation -> (: []) <$> desugarDefaultWorker annotation valueDecl
            Nothing -> failValue "class default method does not have a checked annotation"
        _ -> pure []

desugarDefaultWorker :: TcInstanceMethodAnnotation -> Syn.ValueDecl -> ValueM Decl
desugarDefaultWorker annotation valueDecl = do
  let workerType = tcInstanceMethodType annotation
      methodName = tcInstanceMethodName annotation
      matches =
        case valueDecl of
          Syn.FunctionBind _ sourceMatches -> sourceMatches
          Syn.PatternBind _ _ rhs -> [emptyMatch rhs]
  body <- desugarMatches workerType matches
  convertedType <- convertCheckedType workerType
  moduleOrigin <- gets vsModuleOrigin
  pure
    ( DeclVal
        ValDecl
          { valVis = Pub,
            valName = topName moduleOrigin ("$dm" <> methodName),
            valType = convertedType,
            valBody = body
          }
    )

desugarSelector :: TyCon -> [TyVarId] -> [TcType] -> Int -> TcClassMethodAnnotation -> ValueM Decl
desugarSelector classTyCon classTyVars fieldTypes superClassCount method = do
  _ <- freshUnique
  let (typeVariables, afterForAlls) = peelForAlls (tcClassMethodType method)
      (predicates, resultType) = peelConstraints afterForAlls
  withTypeVariables typeVariables $ do
    dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
    classDictionary <-
      case dictionaries of
        dictionary : _ -> pure dictionary
        [] -> freshBinder "$d" (tcClassMethodDictType method)
    caseBinder <- freshBinder "$dict" (tcClassMethodDictType method)
    fields <- zipWithM (freshIndexedBinder "$method") [0 :: Int ..] fieldTypes
    selected <-
      case drop (superClassCount + tcClassMethodIndex method) fields of
        field : _ -> pure field
        [] -> failValue ("invalid class method index for " <> T.unpack (tcClassMethodName method))
    extraTypes <- mapM (convertCheckedType . TcTyVar) (filter (`notElem` classTyVars) (tcClassMethodTyVars method))
    resultType' <- convertCheckedType resultType
    let extraDictionaries = drop 1 dictionaries
        selectedExpr =
          foldl
            ExApp
            (foldl ExTyApp (ExVar (binderName selected)) extraTypes)
            (map (ExVar . binderName) extraDictionaries)
        selection =
          ExCase
            (ExVar (binderName classDictionary))
            caseBinder
            resultType'
            [Alt (AltData (classDictConName classTyCon)) [] fields selectedExpr]
    typeBinders <- convertTypeBinders typeVariables
    methodType' <- convertCheckedType (tcClassMethodType method)
    moduleOrigin <- gets vsModuleOrigin
    pure
      ( DeclVal
          ValDecl
            { valVis = Pub,
              valName = topName moduleOrigin (tcClassMethodName method),
              valType = methodType',
              valBody = foldr ExTyLam (foldr ExLam selection dictionaries) typeBinders
            }
      )

methodFieldType :: Text -> [TyVarId] -> TcClassMethodAnnotation -> ValueM TcType
methodFieldType className classTyVars method = do
  remaining <-
    case removeClassPredicate predicates of
      Just result -> pure result
      Nothing -> failValue ("class method lacks its class predicate for " <> T.unpack className)
  let extraVariables = filter (`notElem` classTyVars) methodVariables
      qualifiedBody = if null remaining then body else TcQualTy remaining body
  pure (foldr TcForAllTy qualifiedBody extraVariables)
  where
    (methodVariables, afterForAlls) = peelForAlls (tcClassMethodType method)
    (predicates, body) = peelConstraints afterForAlls
    removeClassPredicate [] = Nothing
    removeClassPredicate (predicate : rest) =
      case predicate of
        ClassPred predicateClass _
          | tyConName predicateClass == className -> Just rest
        _ -> (predicate :) <$> removeClassPredicate rest

desugarInstanceDecl :: Syn.Decl -> ValueM [Decl]
desugarInstanceDecl declaration =
  case declaration of
    Syn.DeclAnn annotation inner
      | Just instanceAnnotation <- Syn.fromAnnotation annotation,
        Syn.DeclInstance instanceDecl <- Syn.peelDeclAnn inner ->
          (: []) <$> desugarInstance instanceAnnotation instanceDecl
      | otherwise -> desugarInstanceDecl inner
    Syn.DeclInstance {} -> failValue "missing type-checker annotation for instance declaration"
    _ -> pure []

desugarInstance :: TcInstanceAnnotation -> Syn.InstanceDecl -> ValueM Decl
desugarInstance annotation instanceDecl = withTypeVariables (tcInstanceTyVars annotation) $ do
  let methods = Map.fromListWith appendMatches (instanceMethods instanceDecl)
  contextDictionaries <- zipWithM makeContextDictionary [0 :: Int ..] (tcInstanceContextDicts annotation)
  fields <- withDictionaries contextDictionaries $ do
    superClasses <- mapM (desugarEvidence . snd) (tcInstanceSuperClasses annotation)
    methodFields <- mapM (desugarInstanceMethod annotation contextDictionaries methods) (tcInstanceMethodOrder annotation)
    pure (superClasses <> methodFields)
  _ <- freshUnique
  _ <- freshUnique
  typeBinders <- convertTypeBinders (tcInstanceTyVars annotation)
  headTypes <- convertTyConApplicationArguments (tcInstanceClassTyCon annotation) (tcInstanceHeadTypes annotation)
  dictionaryType <- convertCheckedType (tcInstanceDictType annotation)
  let dictionaryBinders = map dictionaryBinder contextDictionaries
      constructor = foldl ExTyApp (ExVar (classDictConName (tcInstanceClassTyCon annotation))) headTypes
      body = foldr ExTyLam (foldr ExLam (foldl ExApp constructor fields) dictionaryBinders) typeBinders
  moduleOrigin <- gets vsModuleOrigin
  pure
    ( DeclVal
        ValDecl
          { valVis = Pub,
            valName = topName moduleOrigin (tcInstanceDictName annotation),
            valType = dictionaryType,
            valBody = body
          }
    )
  where
    appendMatches (newType, newMatches) (_, oldMatches) = (newType, oldMatches <> newMatches)

desugarInstanceMethod :: TcInstanceAnnotation -> [Dictionary] -> Map Text (TcType, [Syn.Match]) -> Text -> ValueM Expr
desugarInstanceMethod annotation dictionaries methods methodName =
  case Map.lookup methodName methods of
    Just (methodType, matches) -> withDictionaries dictionaries (desugarMatches methodType matches)
    Nothing
      | methodName `elem` tcInstanceDefaultMethods annotation -> desugarDefaultMethod annotation dictionaries methodName
      | otherwise ->
          failValue
            ( "missing method "
                <> T.unpack methodName
                <> " in instance dictionary for "
                <> T.unpack (tyConName (tcInstanceClassTyCon annotation))
                <> " "
                <> show (tcInstanceHeadTypes annotation)
            )

desugarDefaultMethod :: TcInstanceAnnotation -> [Dictionary] -> Text -> ValueM Expr
desugarDefaultMethod annotation dictionaries methodName = do
  method <-
    case [candidate | candidate <- tcInstanceClassMethods annotation, tcClassMethodName candidate == methodName] of
      candidate : _ -> pure candidate
      [] -> failValue ("missing checked class method layout for " <> T.unpack methodName)
  convertedHeadTypes <- convertTyConApplicationArguments (tcInstanceClassTyCon annotation) (tcInstanceHeadTypes annotation)
  convertedInstanceTypes <- mapM (convertCheckedType . TcTyVar) (tcInstanceTyVars annotation)
  let classTyVars = tcInstanceClassTyVars annotation
      extraTyVars = filter (`notElem` classTyVars) (tcClassMethodTyVars method)
      substitution = Map.fromList [(tvUnique tyVar, ty) | (tyVar, ty) <- zip classTyVars (tcInstanceHeadTypes annotation)]
      (_, methodAfterForAlls) = peelForAlls (tcClassMethodType method)
      (methodPredicates, _) = peelConstraints methodAfterForAlls
      extraPredicates = map (applySubstPred substitution) (dropClassPredicate (tcInstanceClassTyCon annotation) methodPredicates)
  extraTypeBinders <- convertTypeBinders extraTyVars
  convertedExtraTypes <- mapM (convertCheckedType . TcTyVar) extraTyVars
  extraDictionaries <- zipWithM (freshDictionaryBinder "$method_d") [0 :: Int ..] extraPredicates
  let workerOrigin =
        case tcInstanceClassOrigin annotation of
          Just (packageName, moduleName') -> OriginTop (PackageId packageName) moduleName'
          Nothing -> OriginLocal (Unique 0)
      worker = foldl ExTyApp (ExVar (Name ("$dm" <> methodName) SortValue workerOrigin)) (convertedHeadTypes <> convertedExtraTypes)
      dictionaryArguments = map (ExVar . binderName . dictionaryBinder) dictionaries
  moduleOrigin <- gets vsModuleOrigin
  let selfName = topName moduleOrigin (tcInstanceDictName annotation)
      self = foldl ExApp (foldl ExTyApp (ExVar selfName) convertedInstanceTypes) dictionaryArguments
      body = foldl ExApp (ExApp worker self) (map (ExVar . binderName) extraDictionaries)
  pure (foldr ExTyLam (foldr ExLam body extraDictionaries) extraTypeBinders)

dropClassPredicate :: TyCon -> [Pred] -> [Pred]
dropClassPredicate classTyCon predicates =
  case predicates of
    [] -> []
    ClassPred predicateClass _ : rest
      | predicateClass == classTyCon -> rest
    predicate : rest -> predicate : dropClassPredicate classTyCon rest

makeContextDictionary :: Int -> TcDictBinderAnnotation -> ValueM Dictionary
makeContextDictionary index annotation = do
  binder <- freshBinder ("$d" <> T.pack (show index)) (tcDictBinderType annotation)
  case constraintTypeToPred (tcDictBinderType annotation) of
    Just predicate -> pure (Dictionary predicate binder)
    Nothing -> failValue ("invalid checked class dictionary type: " <> show (tcDictBinderType annotation))

instanceMethods :: Syn.InstanceDecl -> [(Text, (TcType, [Syn.Match]))]
instanceMethods instanceDecl = concatMap itemMethods (Syn.instanceDeclItems instanceDecl)
  where
    itemMethods item =
      case item of
        Syn.InstanceItemAnn annotation inner
          | Just methodAnnotation <- Syn.fromAnnotation annotation -> methodItem methodAnnotation inner
          | otherwise -> itemMethods inner
        _ -> []
    methodItem methodAnnotation item =
      case item of
        Syn.InstanceItemAnn annotation inner ->
          methodItem
            (fromMaybe methodAnnotation (Syn.fromAnnotation annotation))
            inner
        Syn.InstanceItemBind (Syn.FunctionBind _ matches) ->
          [(tcInstanceMethodName methodAnnotation, (tcInstanceMethodType methodAnnotation, matches))]
        Syn.InstanceItemBind (Syn.PatternBind _ _ rhs) ->
          [(tcInstanceMethodName methodAnnotation, (tcInstanceMethodType methodAnnotation, [emptyMatch rhs]))]
        _ -> []

groupValues :: [Syn.Decl] -> ValueM [ValueGroup]
groupValues [] = pure []
groupValues (declaration : rest) =
  case functionBinding declaration of
    Just (name, matches, Just checkedType) ->
      let (same, remaining) = span (sameFunction name) rest
          moreMatches = concatMap (maybe [] middle . functionBinding) same
       in (FunctionGroup name (matches <> moreMatches) checkedType :) <$> groupValues remaining
    Just (name, _, Nothing) -> failValue ("function " <> T.unpack name <> " does not have a checked type annotation")
    Nothing ->
      case patternBinding declaration of
        Just (name, pattern', rhs, Just checkedType) -> (PatternGroup name pattern' rhs checkedType :) <$> groupValues rest
        Just (name, _, _, Nothing) -> failValue ("pattern binding " <> T.unpack name <> " does not have a checked type annotation")
        Nothing -> groupValues rest

functionBinding :: Syn.Decl -> Maybe (Text, [Syn.Match], Maybe TcType)
functionBinding declaration =
  case Syn.peelDeclAnn declaration of
    Syn.DeclValue (Syn.FunctionBind name matches) -> Just (Syn.unqualifiedNameText name, matches, declarationType declaration)
    _ -> Nothing

sameFunction :: Text -> Syn.Decl -> Bool
sameFunction name declaration = maybe False (\(value, _, _) -> value == name) (functionBinding declaration)

patternBinding :: Syn.Decl -> Maybe (Text, Syn.Pattern, Syn.Rhs Syn.Expr, Maybe TcType)
patternBinding declaration =
  case Syn.peelDeclAnn declaration of
    Syn.DeclValue (Syn.PatternBind _ pattern' rhs) -> (,,,declarationType declaration) <$> singlePatternName pattern' <*> pure pattern' <*> pure rhs
    _ -> Nothing

singlePatternName :: Syn.Pattern -> Maybe Text
singlePatternName pattern' =
  case patternNames pattern' of
    [name] -> Just name
    _ -> Nothing

patternNames :: Syn.Pattern -> [Text]
patternNames pattern' =
  case pattern' of
    Syn.PAnn _ inner -> patternNames inner
    Syn.PParen inner -> patternNames inner
    Syn.PStrict inner -> patternNames inner
    Syn.PIrrefutable inner -> patternNames inner
    Syn.PTypeSig inner _ -> patternNames inner
    Syn.PVar name -> [Syn.unqualifiedNameText name]
    Syn.PAs name inner -> Syn.unqualifiedNameText name : patternNames inner
    Syn.PCon _ _ children -> concatMap patternNames children
    Syn.PInfix left _ right -> patternNames left <> patternNames right
    Syn.PList children -> concatMap patternNames children
    Syn.PTuple _ children -> concatMap patternNames children
    _ -> []

declarationType :: Syn.Decl -> Maybe TcType
declarationType declaration =
  case declaration of
    Syn.DeclAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> declarationType inner
    _ -> Nothing

middle :: (a, b, c) -> b
middle (_, value, _) = value

barePatternName :: Syn.Pattern -> Maybe Text
barePatternName pattern' =
  case pattern' of
    Syn.PVar name -> Just (Syn.unqualifiedNameText name)
    Syn.PAnn _ inner -> barePatternName inner
    Syn.PParen inner -> barePatternName inner
    _ -> Nothing

allocateTopValue :: ValueGroup -> ValueM TopValue
allocateTopValue group = do
  _ <- freshUnique
  let name = groupName group
      ty = groupType group
  moduleOrigin <- gets vsModuleOrigin
  pure (TopValue (topName moduleOrigin name) ty group)

groupName :: ValueGroup -> Text
groupName group =
  case group of
    FunctionGroup name _ _ -> name
    PatternGroup name _ _ _ -> name

groupType :: ValueGroup -> TcType
groupType group =
  case group of
    FunctionGroup _ _ ty -> ty
    PatternGroup _ _ _ ty -> ty

desugarTopValue :: TopValue -> ValueM ValDecl
desugarTopValue top = do
  body <- case topGroup top of
    FunctionGroup _ matches _ -> desugarMatches (topType top) matches
    PatternGroup name pattern' rhs _
      | isJust (barePatternName pattern') -> desugarMatches (topType top) [emptyMatch rhs]
      | otherwise -> desugarPatternBindingScheme name pattern' rhs (topType top)
  ty <- convertCheckedType (topType top)
  pure
    ValDecl
      { valVis = Pub,
        valName = topCoreName top,
        valType = ty,
        valBody = body
      }

desugarPatternBinding :: Text -> Syn.Pattern -> Syn.Rhs Syn.Expr -> TcType -> ValueM Expr
desugarPatternBinding name pattern' rhs resultType = do
  constructorType <- patternConstructorResultType pattern'
  let sourceType = fromMaybe resultType (patternType pattern' <|> constructorType)
  source <- desugarRhs rhs
  sourceBinder <- freshPatternBinder pattern' sourceType
  selected <-
    desugarDoPattern resultType sourceBinder sourceType pattern' $ do
      locals <- gets vsLocals
      case Map.lookup name locals of
        Just (binder, _) -> pure (ExVar (binderName binder))
        Nothing -> failValue ("pattern binding does not bind " <> T.unpack name)
  pure (ExLet (Bind sourceBinder source) selected)

patternConstructorResultType :: Syn.Pattern -> ValueM (Maybe TcType)
patternConstructorResultType pattern' =
  case patternConstructorSourceName pattern' of
    Nothing -> pure Nothing
    Just constructorName -> do
      constructors <- Map.findWithDefault [] (Syn.nameText constructorName) <$> gets vsConstructorInfos
      pure $
        case resolvedTermKey constructorName of
          Just (package, moduleName', _) ->
            dciResTy
              <$> listToMaybe
                [ candidate
                | candidate <- constructors,
                  dciOrigin candidate == (package, moduleName')
                ]
          Nothing -> dciResTy <$> listToMaybe constructors

desugarPatternBindingScheme :: Text -> Syn.Pattern -> Syn.Rhs Syn.Expr -> TcType -> ValueM Expr
desugarPatternBindingScheme name pattern' rhs ty = do
  let (typeVariables, afterForAlls) = peelForAlls ty
      (predicates, resultType) = peelConstraints afterForAlls
  typeBinders <- convertTypeBinders typeVariables
  withTypeVariables typeVariables $ do
    dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
    body <-
      withDictionaries
        (zipWith Dictionary predicates dictionaries)
        (desugarPatternBinding name pattern' rhs resultType)
    pure (foldr ExTyLam (foldr ExLam body dictionaries) typeBinders)

emptyMatch :: Syn.Rhs Syn.Expr -> Syn.Match
emptyMatch rhs =
  Syn.Match
    { Syn.matchAnns = [],
      Syn.matchHeadForm = Syn.MatchHeadPrefix,
      Syn.matchPats = [],
      Syn.matchRhs = rhs
    }

desugarMatches :: TcType -> [Syn.Match] -> ValueM Expr
desugarMatches ty matches =
  case matches of
    [] -> failValue "value binding has no match"
    first : _ -> do
      let (typeVariables, afterForAlls) = peelForAlls ty
          (predicates, bodyType) = peelConstraints afterForAlls
          argumentCount = length (Syn.matchPats first)
          (argumentTypes, resultType) = peelFunctions argumentCount bodyType
      typeBinders <- convertTypeBinders typeVariables
      (dictionaries, arguments, body) <-
        withTypeVariables typeVariables $ do
          dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
          arguments <- zipWithM freshArgument [0 :: Int ..] argumentTypes
          body <-
            withBinderTypes (zip (map binderName arguments) argumentTypes) $
              withDictionaries (zipWith Dictionary predicates dictionaries) (desugarMatchArguments resultType arguments matches)
          pure (dictionaries, arguments, body)
      pure (foldr ExTyLam (foldr ExLam (foldr ExLam body arguments) dictionaries) typeBinders)

withTypeVariables :: [TyVarId] -> ValueM a -> ValueM a
withTypeVariables variables action = do
  previous <- gets vsConvertEnv
  modify' $ \state -> state {vsConvertEnv = withTyVars variables previous}
  result <- action
  modify' $ \state -> state {vsConvertEnv = previous}
  pure result

desugarMatchArguments :: TcType -> [Binder] -> [Syn.Match] -> ValueM Expr
desugarMatchArguments _ [] (match : _) = desugarRhs (Syn.matchRhs match)
desugarMatchArguments _ [] [] = failValue "pattern match has no result"
desugarMatchArguments resultType (argument : arguments) matches
  | any firstPatternIsOverloadedInteger matches =
      desugarOverloadedIntegerMatches resultType (argument : arguments) matches
  | first : _ <- matches,
    let firstPatterns = Syn.matchPats first,
    length firstPatterns == length (argument : arguments),
    all patternIsIrrefutable firstPatterns =
      withLocals (matchArgumentBindings (argument : arguments) first) (desugarRhs (Syn.matchRhs first))
  | all (maybe False patternIsIrrefutable . listToMaybe . Syn.matchPats) matches = do
      let locals = concatMap (firstPatternBindings argument) matches
      withLocals locals (desugarMatchArguments resultType arguments (map dropFirstPattern matches))
  | otherwise = do
      maybeNewtype <- firstNewtypePattern matches
      case maybeNewtype of
        Just (pattern', dataType) -> desugarNewtypePatterns resultType argument arguments matches pattern' dataType
        Nothing -> desugarDataPatterns resultType argument arguments matches

desugarOverloadedIntegerMatches :: TcType -> [Binder] -> [Syn.Match] -> ValueM Expr
desugarOverloadedIntegerMatches resultType arguments matches =
  case matches of
    [] -> overloadedPatternFailure resultType arguments
    match : rest -> do
      failure <- desugarOverloadedIntegerMatches resultType arguments rest
      desugarOverloadedIntegerMatch resultType arguments match failure

desugarOverloadedIntegerMatch :: TcType -> [Binder] -> Syn.Match -> Expr -> ValueM Expr
desugarOverloadedIntegerMatch resultType arguments match failure =
  compile (zip arguments (Syn.matchPats match))
  where
    compile [] = desugarRhs (Syn.matchRhs match)
    compile ((argument, pattern') : rest)
      | patternIsIrrefutable pattern' =
          withLocals (patternMatchBindings pattern' argument (fromBinderType pattern')) (compile rest)
      | isOverloadedIntegerPattern pattern' = do
          test <- desugarOverloadedIntegerPatternTest (ExVar (binderName argument)) pattern'
          testType <- requiredPatternMethodResultType "==" pattern'
          testBinder <- freshBinder "_case_guard" testType
          resultType' <- convertCheckedType resultType
          trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
          falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
          success <- compile rest
          pure
            ( ExCase
                test
                testBinder
                resultType'
                [ Alt (AltData trueName) [] [] success,
                  Alt (AltData falseName) [] [] failure
                ]
            )
      | otherwise =
          let remainingMatch = match {Syn.matchPats = pattern' : map snd rest}
           in desugarMatchArguments resultType (argument : map fst rest) [remainingMatch]

firstPatternIsOverloadedInteger :: Syn.Match -> Bool
firstPatternIsOverloadedInteger match =
  case Syn.matchPats match of
    pattern' : _ -> isOverloadedIntegerPattern pattern'
    [] -> False

overloadedPatternFailure :: TcType -> [Binder] -> ValueM Expr
overloadedPatternFailure resultType arguments = do
  resultType' <- convertCheckedType resultType
  case arguments of
    argument : _ -> do
      failureBinder <- freshBinderFromType "_case_nomatch" (binderType argument)
      pure (ExCase (ExVar (binderName argument)) failureBinder resultType' [])
    [] -> failValue "overloaded integer match has no argument"

desugarOverloadedIntegerPatternTest :: Expr -> Syn.Pattern -> ValueM Expr
desugarOverloadedIntegerPatternTest scrutinee pattern' = do
  (value, negative) <-
    maybe
      (failValue ("invalid overloaded integer pattern: " <> take 80 (show pattern')))
      pure
      (integerPatternValue pattern')
  fromIntegerMethod <- desugarPatternMethod "fromInteger" pattern'
  integer <- desugarIntegerLiteral value
  let positive = ExApp fromIntegerMethod integer
  patternValue <-
    if negative
      then (`ExApp` positive) <$> desugarPatternMethod "negate" pattern'
      else pure positive
  equality <- desugarPatternMethod "==" pattern'
  pure (ExApp (ExApp equality scrutinee) patternValue)

desugarPatternMethod :: Text -> Syn.Pattern -> ValueM Expr
desugarPatternMethod name pattern' = do
  (annotation, resolution) <- requiredPatternOccurrence name pattern'
  desugarResolvedOccurrence annotation resolution

requiredPatternMethodResultType :: Text -> Syn.Pattern -> ValueM TcType
requiredPatternMethodResultType name pattern' = do
  (annotation, _) <- requiredPatternOccurrence name pattern'
  case applicationResultType (tcAnnType annotation) >>= applicationResultType of
    Just result -> pure result
    Nothing -> failValue ("invalid checked pattern method type for " <> T.unpack name)

requiredPatternOccurrence :: Text -> Syn.Pattern -> ValueM (TcAnnotation, ResolutionAnnotation)
requiredPatternOccurrence name pattern' =
  maybe
    (failValue ("missing checked " <> T.unpack name <> " occurrence for overloaded integer pattern"))
    pure
    (patternOccurrence name pattern')

patternOccurrence :: Text -> Syn.Pattern -> Maybe (TcAnnotation, ResolutionAnnotation)
patternOccurrence target = go Nothing
  where
    go currentType pattern' =
      case pattern' of
        Syn.PAnn annotation inner ->
          case (Syn.fromAnnotation annotation :: Maybe TcAnnotation, Syn.fromAnnotation annotation :: Maybe ResolutionAnnotation) of
            (Just checked, _) -> go (Just checked) inner
            (_, Just resolution)
              | resolutionIdentifier resolution == IdentifierNamed target,
                resolutionNamespace resolution == ResolutionNamespaceTerm ->
                  (,resolution) <$> currentType
            _ -> go currentType inner
        Syn.PParen inner -> go currentType inner
        Syn.PStrict inner -> go currentType inner
        Syn.PIrrefutable inner -> go currentType inner
        Syn.PAs _ inner -> go currentType inner
        Syn.PTypeSig inner _ -> go currentType inner
        _ -> Nothing

isOverloadedIntegerPattern :: Syn.Pattern -> Bool
isOverloadedIntegerPattern = isJust . integerPatternValue

integerPatternValue :: Syn.Pattern -> Maybe (Integer, Bool)
integerPatternValue pattern' =
  case pattern' of
    Syn.PAnn _ inner -> integerPatternValue inner
    Syn.PParen inner -> integerPatternValue inner
    Syn.PStrict inner -> integerPatternValue inner
    Syn.PIrrefutable inner -> integerPatternValue inner
    Syn.PAs _ inner -> integerPatternValue inner
    Syn.PTypeSig inner _ -> integerPatternValue inner
    Syn.PLit literal -> (,False) <$> overloadedIntegerValue literal
    Syn.PNegLit literal -> (,True) <$> overloadedIntegerValue literal
    _ -> Nothing

overloadedIntegerValue :: Syn.Literal -> Maybe Integer
overloadedIntegerValue literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitInt value Syn.TInteger _ -> Just value
    _ -> Nothing

desugarDataPatterns :: TcType -> Binder -> [Binder] -> [Syn.Match] -> ValueM Expr
desugarDataPatterns resultType argument arguments matches = do
  argumentType <-
    gets vsBinderTypes
      >>= maybe (failValue "missing checked case argument type") pure
        . Map.lookup (binderName argument)
  caseBinder <- freshBinderFromType "_scrut" (binderType argument)
  resultType' <- convertCheckedType resultType
  let keys = patternKeys matches
      defaultMatches = filter firstPatternIsDefault matches
      rootBindings = concatMap (firstPatternBindings caseBinder) matches
  withLocals rootBindings $ do
    constructorAlternatives <- mapM (desugarPatternGroup resultType argumentType arguments matches) keys
    defaultAlternatives <-
      case defaultMatches of
        [] -> pure []
        _ -> do
          body <- desugarMatchArguments resultType arguments (map dropFirstPattern defaultMatches)
          pure [Alt AltDefault [] [] body]
    pure (ExCase (ExVar (binderName argument)) caseBinder resultType' (constructorAlternatives <> defaultAlternatives))

firstNewtypePattern :: [Syn.Match] -> ValueM (Maybe (Syn.Pattern, DataTypeInfo))
firstNewtypePattern matches = do
  newtypes <- gets vsNewtypeConstructors
  pure $ do
    pattern' <-
      listToMaybe
        [ candidate
        | match <- matches,
          candidate : _ <- [Syn.matchPats match],
          not (patternIsDefault candidate)
        ]
    name <- patternConstructorSourceName pattern'
    key <- resolvedTermKey name
    dataType <- Map.lookup key newtypes
    pure (pattern', dataType)

patternConstructorSourceName :: Syn.Pattern -> Maybe Syn.Name
patternConstructorSourceName pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> Just name
    Syn.PInfix _ name _ -> Just name
    _ -> Nothing

desugarNewtypePatterns :: TcType -> Binder -> [Binder] -> [Syn.Match] -> Syn.Pattern -> DataTypeInfo -> ValueM Expr
desugarNewtypePatterns resultType argument remaining matches representative dataType = do
  child <-
    case patternChildren representative of
      [pattern'] -> pure pattern'
      _ -> failValue ("newtype pattern does not have one field: " <> T.unpack (dtiName dataType))
  childType <- requiredPatternTypeFor "newtype match field" child
  field <- freshPatternBinder child childType
  typeArguments <- newtypePatternArguments representative
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  let key = patternKey representative
      expanded = mapMaybe (specializeMatch key 1) matches
      rootBindings = concatMap (firstPatternBindings argument) matches
      childBindings =
        concat
          [ patternMatchBindings pattern' field childType
          | match <- matches,
            candidate : _ <- [Syn.matchPats match],
            not (patternIsDefault candidate),
            patternKey candidate == key,
            pattern' <- patternChildren candidate
          ]
      tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      unwrapped = ExCast (ExVar (binderName argument)) (CoAxiom axiom convertedArguments)
  body <-
    withBinderTypes [(binderName field, childType)] $
      withLocals (rootBindings <> childBindings) (desugarMatchArguments resultType (field : remaining) expanded)
  pure (ExLet (Bind field unwrapped) body)

newtypePatternArguments :: Syn.Pattern -> ValueM [TcType]
newtypePatternArguments pattern' =
  case constructorResultType (length (patternChildren pattern')) (fromBinderType pattern') of
    TcTyCon _ arguments -> pure arguments
    ty -> failValue ("newtype pattern has an invalid checked type: " <> show ty)

constructorResultType :: Int -> TcType -> TcType
constructorResultType arity ty =
  case ty of
    TcForAllTy _ body -> constructorResultType arity body
    TcQualTy _ body -> constructorResultType arity body
    TcFunTy _ result
      | arity > 0 -> constructorResultType (arity - 1) result
    _ -> ty

desugarPatternGroup :: TcType -> TcType -> [Binder] -> [Syn.Match] -> Text -> ValueM Alt
desugarPatternGroup resultType argumentType remaining matches key = do
  pattern' <-
    case [candidate | match <- matches, candidate : _ <- [Syn.matchPats match], not (patternIsDefault candidate), patternKey candidate == key] of
      candidate : _ -> pure candidate
      [] -> failValue ("missing representative pattern for " <> T.unpack key)
  constructor <- patternConstructor pattern'
  let subpatterns = patternChildren pattern'
      predicates = patternGivenPredicates pattern'
      typeVariables = patternTypeVariables pattern'
  typeBinders <- convertTypeBinders typeVariables
  fieldTypes <-
    case (peelPattern pattern', argumentType) of
      (Syn.PTuple _ fields, TcTyCon _ types)
        | length fields == length types -> pure types
      _ -> patternFieldTypes pattern' subpatterns
  fields <- zipWithM freshPatternBinder subpatterns fieldTypes
  dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] predicates
  let arity = length fields
      expanded = mapMaybe (specializeMatch key arity) matches
      localBindings =
        concat
          [ concat (zipWith3 patternMatchBindings children fields fieldTypes)
          | match <- matches,
            candidate : _ <- [Syn.matchPats match],
            not (patternIsDefault candidate),
            patternKey candidate == key,
            let children = patternChildren candidate
          ]
  body <-
    withBinderTypes (zip (map binderName fields) fieldTypes) $
      withDictionaries
        (zipWith Dictionary predicates dictionaries)
        (withLocals localBindings (desugarMatchArguments resultType (fields <> remaining) expanded))
  pure (Alt constructor typeBinders (dictionaries <> fields) body)

patternGivenPredicates :: Syn.Pattern -> [Pred]
patternGivenPredicates = go
  where
    go pattern' =
      case pattern' of
        Syn.PAnn annotation inner -> annotationPredicates annotation <> go inner
        Syn.PParen inner -> go inner
        Syn.PStrict inner -> go inner
        Syn.PIrrefutable inner -> go inner
        Syn.PAs _ inner -> go inner
        Syn.PTypeSig inner _ -> go inner
        Syn.PCon name _ _ -> annotationsPredicates (Syn.nameAnns name)
        Syn.PInfix _ name _ -> annotationsPredicates (Syn.nameAnns name)
        _ -> []
    annotationPredicates annotation =
      maybe [] evidencePredicates (Syn.fromAnnotation annotation :: Maybe TcAnnotation)
    annotationsPredicates annotations =
      concat
        [ evidencePredicates checked
        | annotation <- annotations,
          Just checked <- [Syn.fromAnnotation annotation :: Maybe TcAnnotation]
        ]
    evidencePredicates checked = [predicate | Ev.EvGiven predicate <- tcAnnEvidenceTerms checked]

patternTypeVariables :: Syn.Pattern -> [TyVarId]
patternTypeVariables = go
  where
    go pattern' =
      case pattern' of
        Syn.PAnn annotation inner -> annotationTypeVariables annotation <> go inner
        Syn.PParen inner -> go inner
        Syn.PStrict inner -> go inner
        Syn.PIrrefutable inner -> go inner
        Syn.PAs _ inner -> go inner
        Syn.PTypeSig inner _ -> go inner
        _ -> []
    annotationTypeVariables annotation =
      maybe [] tcAnnTypeBinders (Syn.fromAnnotation annotation :: Maybe TcAnnotation)

patternKeys :: [Syn.Match] -> [Text]
patternKeys matches =
  List.nub
    [ patternKey pattern'
    | match <- matches,
      pattern' : _ <- [Syn.matchPats match],
      not (patternIsDefault pattern')
    ]

patternKey :: Syn.Pattern -> Text
patternKey pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> Syn.nameText name
    Syn.PInfix _ name _ -> Syn.nameText name
    Syn.PList [] -> "[]"
    Syn.PList (_ : _) -> ":"
    Syn.PTuple _ fields -> "(" <> T.replicate (max 0 (length fields - 1)) "," <> ")"
    Syn.PLit literal
      | isBoxedCharacterLiteral literal -> "C#"
      | otherwise -> T.pack (show (Syn.peelLiteralAnn literal))
    _ -> "_"

firstPatternIsDefault :: Syn.Match -> Bool
firstPatternIsDefault match =
  case Syn.matchPats match of
    pattern' : _ -> patternIsDefault pattern'
    [] -> False

firstPatternBindings :: Binder -> Syn.Match -> [(Text, (Binder, TcType))]
firstPatternBindings binder match =
  case Syn.matchPats match of
    pattern' : _ -> patternMatchBindings pattern' binder (fromBinderType pattern')
    [] -> []

matchArgumentBindings :: [Binder] -> Syn.Match -> [(Text, (Binder, TcType))]
matchArgumentBindings binders match =
  concat
    ( zipWith
        (\pattern' binder -> patternMatchBindings pattern' binder (fromBinderType pattern'))
        (Syn.matchPats match)
        binders
    )

dropFirstPattern :: Syn.Match -> Syn.Match
dropFirstPattern match = match {Syn.matchPats = drop 1 (Syn.matchPats match)}

specializeMatch :: Text -> Int -> Syn.Match -> Maybe Syn.Match
specializeMatch key arity match =
  case Syn.matchPats match of
    pattern' : rest
      | patternIsDefault pattern' -> Just match {Syn.matchPats = replicate arity Syn.PWildcard <> rest}
      | patternKey pattern' == key -> Just match {Syn.matchPats = patternChildren pattern' <> rest}
      | otherwise -> Nothing
    [] -> Nothing

patternIsIrrefutable :: Syn.Pattern -> Bool
patternIsIrrefutable pattern' =
  case pattern' of
    Syn.PAnn _ inner -> patternIsIrrefutable inner
    Syn.PParen inner -> patternIsIrrefutable inner
    Syn.PAs _ inner -> patternIsIrrefutable inner
    Syn.PIrrefutable {} -> True
    Syn.PTypeSig inner _ -> patternIsIrrefutable inner
    Syn.PVar {} -> True
    Syn.PWildcard -> True
    _ -> False

patternIsDefault :: Syn.Pattern -> Bool
patternIsDefault pattern' =
  case pattern' of
    Syn.PAnn _ inner -> patternIsDefault inner
    Syn.PParen inner -> patternIsDefault inner
    Syn.PAs _ inner -> patternIsDefault inner
    Syn.PStrict inner -> patternIsDefault inner
    Syn.PIrrefutable {} -> True
    Syn.PTypeSig inner _ -> patternIsDefault inner
    Syn.PVar {} -> True
    Syn.PWildcard -> True
    _ -> False

patternChildren :: Syn.Pattern -> [Syn.Pattern]
patternChildren pattern' =
  case peelPattern pattern' of
    Syn.PCon _ _ children -> children
    Syn.PInfix left _ right -> [left, right]
    Syn.PList [] -> []
    Syn.PList (item : items) ->
      let tailPattern = Syn.PList items
          checkedTail =
            case patternType pattern' of
              Just ty -> Syn.PAnn (Syn.mkAnnotation (TcAnnotation ty [] [] [] [] [])) tailPattern
              Nothing -> tailPattern
       in [item, checkedTail]
    Syn.PTuple _ children -> children
    Syn.PLit literal
      | Syn.LitChar value source <- Syn.peelLiteralAnn literal -> [Syn.PLit (Syn.LitCharHash value source)]
    _ -> []

patternConstructor :: Syn.Pattern -> ValueM AltCon
patternConstructor pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> AltData <$> resolvedTermName name
    Syn.PInfix _ name _ -> AltData <$> resolvedTermName name
    Syn.PList [] -> AltData <$> primitiveName "GHC.Types" "[]" SortDataConstructor
    Syn.PList (_ : _) -> AltData <$> primitiveName "GHC.Types" ":" SortDataConstructor
    Syn.PTuple flavor fields ->
      let arity = length fields
          constructor =
            case flavor of
              Syn.Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
              Syn.Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"
          moduleName' =
            case flavor of
              Syn.Boxed -> "GHC.Tuple"
              Syn.Unboxed -> "GHC.Types"
       in AltData <$> primitiveName moduleName' constructor SortDataConstructor
    Syn.PLit literal
      | isBoxedCharacterLiteral literal -> AltData <$> uniqueConstructorName "C#"
      | otherwise -> AltLit <$> patternLiteral literal
    Syn.PWildcard -> pure AltDefault
    Syn.PVar {} -> pure AltDefault
    unsupported -> failValue ("unsupported System FC pattern: " <> take 80 (show unsupported))

patternLiteral :: Syn.Literal -> ValueM Literal
patternLiteral literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitInt value numericType _ -> LitInt <$> convertRuntimeRep (numericRepresentation numericType) <*> pure value
    Syn.LitChar value _ -> LitChar <$> convertRuntimeRep WordRep <*> pure value
    Syn.LitCharHash value _ -> LitChar <$> convertRuntimeRep WordRep <*> pure value
    unsupported -> failValue ("unsupported System FC pattern literal: " <> show unsupported)

isBoxedCharacterLiteral :: Syn.Literal -> Bool
isBoxedCharacterLiteral literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitChar {} -> True
    _ -> False

patternFieldTypes :: Syn.Pattern -> [Syn.Pattern] -> ValueM [TcType]
patternFieldTypes parent children
  | Syn.PLit literal <- peelPattern parent,
    isBoxedCharacterLiteral literal = do
      constructorType <- lookupCheckedType "C#"
      case firstFunctionArgument constructorType of
        Just fieldType -> pure [fieldType]
        Nothing -> failValue "boxed character constructor does not have one field"
  | Syn.PTuple _ fields <- peelPattern parent,
    length fields == length children,
    Just (TcTyCon _ fieldTypes) <- patternType parent,
    length fieldTypes == length children =
      pure fieldTypes
  | otherwise = mapM (requiredPatternTypeFor "constructor field") children

firstFunctionArgument :: TcType -> Maybe TcType
firstFunctionArgument ty =
  case ty of
    TcForAllTy _ body -> firstFunctionArgument body
    TcQualTy _ body -> firstFunctionArgument body
    TcFunTy argument _ -> Just argument
    _ -> Nothing

freshPatternBinder :: Syn.Pattern -> TcType -> ValueM Binder
freshPatternBinder pattern' = freshBinder (fromMaybe "_pat" (barePatternName pattern'))

patternLocalBindings :: Syn.Pattern -> Binder -> [(Text, (Binder, TcType))]
patternLocalBindings pattern' binder = patternMatchBindings pattern' binder (fromBinderType pattern')

patternMatchBindings :: Syn.Pattern -> Binder -> TcType -> [(Text, (Binder, TcType))]
patternMatchBindings pattern' binder ty =
  case pattern' of
    Syn.PAnn _ inner -> patternMatchBindings inner binder ty
    Syn.PParen inner -> patternMatchBindings inner binder ty
    Syn.PStrict inner -> patternMatchBindings inner binder ty
    Syn.PIrrefutable inner -> patternMatchBindings inner binder ty
    Syn.PTypeSig inner _ -> patternMatchBindings inner binder ty
    Syn.PVar name -> [(Syn.unqualifiedNameText name, (binder, ty))]
    Syn.PAs name inner -> (Syn.unqualifiedNameText name, (binder, ty)) : patternMatchBindings inner binder ty
    _ -> []

peelPattern :: Syn.Pattern -> Syn.Pattern
peelPattern pattern' =
  case pattern' of
    Syn.PAnn _ inner -> peelPattern inner
    Syn.PParen inner -> peelPattern inner
    Syn.PStrict inner -> peelPattern inner
    Syn.PIrrefutable inner -> peelPattern inner
    Syn.PAs _ inner -> peelPattern inner
    Syn.PTypeSig inner _ -> peelPattern inner
    _ -> pattern'

requiredPatternTypeFor :: String -> Syn.Pattern -> ValueM TcType
requiredPatternTypeFor context pattern' =
  case patternType pattern' of
    Just ty -> pure ty
    Nothing -> failValue ("missing checked " <> context <> " type: " <> take 240 (show pattern'))

patternType :: Syn.Pattern -> Maybe TcType
patternType pattern' =
  case pattern' of
    Syn.PVar name -> nameTcType name
    Syn.PAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> patternType inner
    Syn.PLit literal -> literalType literal
    Syn.PNegLit literal -> literalType literal
    Syn.PParen inner -> patternType inner
    Syn.PStrict inner -> patternType inner
    Syn.PIrrefutable inner -> patternType inner
    Syn.PAs name inner -> nameTcType name <|> patternType inner
    Syn.PTypeSig inner _ -> patternType inner
    _ -> Nothing

literalType :: Syn.Literal -> Maybe TcType
literalType literal =
  case literal of
    Syn.LitAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> literalType inner
    _ -> Nothing

fromBinderType :: Syn.Pattern -> TcType
fromBinderType pattern' = fromMaybe typeKindType (patternType pattern')

nameTcType :: Syn.UnqualifiedName -> Maybe TcType
nameTcType name =
  tcAnnType <$> listToMaybe (mapMaybe Syn.fromAnnotation (Syn.unqualifiedNameAnns name))

desugarRhs :: Syn.Rhs Syn.Expr -> ValueM Expr
desugarRhs rhs =
  case rhs of
    Syn.UnguardedRhs _ expression Nothing -> desugarExpr expression
    Syn.UnguardedRhs _ expression (Just declarations) -> desugarLocalDecls declarations (desugarExpr expression)
    Syn.GuardedRhss {} -> failValue ("guarded right-hand side remains after type checking: " <> take 160 (show rhs))

desugarExpr :: Syn.Expr -> ValueM Expr
desugarExpr expression =
  case annotatedListComp expression of
    Just (annotation, resolutionAnnotation, body, statements) ->
      desugarAnnotatedExpr annotation (Syn.EAnn resolutionAnnotation (Syn.EListComp body statements))
    Nothing -> desugarOrdinaryExpr expression

desugarOrdinaryExpr :: Syn.Expr -> ValueM Expr
desugarOrdinaryExpr expression =
  case expression of
    Syn.EAnn annotation inner
      | Just tcAnnotation <- Syn.fromAnnotation annotation -> desugarAnnotatedExpr tcAnnotation inner
      | otherwise -> desugarExpr inner
    Syn.EVar name -> desugarVariable Nothing name
    Syn.EApp function argument -> desugarApplication function argument
    Syn.EInfix left operator right
      | Syn.nameText operator == "$" -> ExApp <$> desugarExpr left <*> desugarExpr right
    Syn.EInfix left operator right -> do
      operator' <- desugarInfixOperator operator
      (ExApp . ExApp operator' <$> desugarExpr left) <*> desugarExpr right
    Syn.EParen inner -> desugarExpr inner
    Syn.ETypeSig inner _ -> desugarExpr inner
    Syn.ETypeApp function _ -> desugarExpr function
    Syn.ELambdaPats patterns body -> desugarLambda patterns body
    Syn.EIf condition thenExpression elseExpression -> do
      resultType <- requiredExprType thenExpression
      desugarIf resultType condition thenExpression elseExpression
    Syn.ECase {} -> failValue "case expression does not have a checked result type"
    Syn.ELetDecls declarations body -> desugarLocalDecls declarations (desugarExpr body)
    unsupported -> failValue ("unsupported System FC expression: " <> take 80 (show unsupported))

annotatedListComp :: Syn.Expr -> Maybe (TcAnnotation, Syn.Annotation, Syn.Expr, [Syn.CompStmt])
annotatedListComp = go Nothing Nothing
  where
    go checked resolution expression =
      case expression of
        Syn.EAnn annotation inner ->
          go
            ((Syn.fromAnnotation annotation :: Maybe TcAnnotation) <|> checked)
            ( if isJust (Syn.fromAnnotation annotation :: Maybe ResolutionAnnotation)
                then Just annotation
                else resolution
            )
            inner
        Syn.EListComp body statements -> (,,,statements) <$> checked <*> resolution <*> pure body
        _ -> Nothing

desugarAnnotatedExpr :: TcAnnotation -> Syn.Expr -> ValueM Expr
desugarAnnotatedExpr annotation inner = do
  let evidencePredicates = [predicate | Ev.EvGiven predicate <- tcAnnEvidenceBinders annotation]
  evidenceBinders <- zipWithM (freshDictionaryBinder "$higher_rank_d") [0 :: Int ..] evidencePredicates
  body <-
    withDictionaries (zipWith Dictionary evidencePredicates evidenceBinders) $
      case inner of
        _
          | not (null (tcAnnTypeBinders annotation)) || not (null evidenceBinders) -> desugarExpr inner
        expression
          | Just name <- annotatedVariable expression -> do
              desugarVariable (Just annotation) name
        Syn.EAnn resolutionAnnotation (Syn.EInt value Syn.TInteger _)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation,
            resolutionNamespace resolution == ResolutionNamespaceTerm,
            resolutionIdentifier resolution == IdentifierNamed "fromInteger" ->
              desugarOverloadedInteger annotation resolution value
        Syn.EAnn resolutionAnnotation (Syn.EArithSeq arithSeq)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation ->
              desugarArithSeq annotation resolution arithSeq
        Syn.EInt value numericType _
          | numericType /= Syn.TInteger -> do
              representation <- convertRuntimeRep (numericRepresentation numericType)
              pure (ExLit (LitInt representation value))
        Syn.EChar value _ -> do
          constructor <- uniqueConstructorName "C#"
          representation <- convertRuntimeRep WordRep
          pure (ExApp (ExVar constructor) (ExLit (LitChar representation value)))
        Syn.ECharHash value _ -> do
          representation <- convertRuntimeRep WordRep
          pure (ExLit (LitChar representation value))
        Syn.EString value _ -> desugarString annotation value
        Syn.EStringHash value _ -> do
          representation <- convertRuntimeRep AddrRep
          pure (ExLit (LitAddr representation (BS.pack (map (fromIntegral . fromEnum) (T.unpack value)))))
        Syn.EList elements -> desugarList annotation elements
        Syn.EAnn resolutionAnnotation (Syn.EListComp body statements)
          | Just resolution <- Syn.fromAnnotation resolutionAnnotation -> desugarListComp annotation resolution body statements
        Syn.ETuple flavor elements -> desugarTuple annotation flavor elements
        Syn.ESectionL operand operator -> desugarSectionL annotation operand operator
        Syn.ESectionR operator operand -> desugarSectionR annotation operator operand
        Syn.EDo statements _ -> desugarDo statements
        Syn.EIf condition thenExpression elseExpression ->
          desugarIf (tcAnnType annotation) condition thenExpression elseExpression
        Syn.ECase scrutinee alternatives -> desugarCase (tcAnnType annotation) scrutinee alternatives
        _ -> desugarExpr inner
  typeBinders <- convertTypeBinders (tcAnnTypeBinders annotation)
  pure (foldr ExTyLam (foldr ExLam body evidenceBinders) typeBinders)

desugarIf :: TcType -> Syn.Expr -> Syn.Expr -> Syn.Expr -> ValueM Expr
desugarIf resultType condition thenExpression elseExpression = do
  condition' <- desugarExpr condition
  conditionType <- requiredExprType condition
  binder <- freshBinder "_if" conditionType
  resultType' <- convertCheckedType resultType
  trueName <- primitiveName "GHC.Types" "True" SortDataConstructor
  falseName <- primitiveName "GHC.Types" "False" SortDataConstructor
  thenExpression' <- desugarExpr thenExpression
  elseExpression' <- desugarExpr elseExpression
  pure
    ( ExCase
        condition'
        binder
        resultType'
        [ Alt (AltData trueName) [] [] thenExpression',
          Alt (AltData falseName) [] [] elseExpression'
        ]
    )

annotatedVariable :: Syn.Expr -> Maybe Syn.Name
annotatedVariable expression =
  case expression of
    Syn.EAnn _ inner -> annotatedVariable inner
    Syn.EParen inner -> annotatedVariable inner
    Syn.EVar name -> Just name
    _ -> Nothing

localOccurrenceTypeArguments :: Syn.Name -> TcAnnotation -> ValueM [TcType]
localOccurrenceTypeArguments name annotation
  | not (null (tcAnnTypeArgs annotation)) = pure (tcAnnTypeArgs annotation)
  | otherwise = do
      local <- Map.lookup (Syn.nameText name) <$> gets vsLocals
      pure
        ( fromMaybe [] $ do
            (_, declaredType) <- local
            let (typeVariables, bodyType) = peelForAlls declaredType
            substitution <- matchTypes [bodyType] [tcAnnType annotation]
            mapM (\typeVariable -> Map.lookup (tvUnique typeVariable) substitution) typeVariables
        )

desugarInfixOperator :: Syn.Name -> ValueM Expr
desugarInfixOperator operator =
  case listToMaybe (mapMaybe Syn.fromAnnotation (Syn.nameAnns operator)) of
    Just annotation -> do
      variable <- occurrenceName operator
      types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
      evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
      pure (foldl ExApp (foldl ExTyApp (ExVar variable) types) evidence)
    Nothing -> ExVar <$> occurrenceName operator

desugarSectionL :: TcAnnotation -> Syn.Expr -> Syn.Name -> ValueM Expr
desugarSectionL annotation operand operator = do
  binder <- freshBinder "_section" =<< sectionArgumentType annotation
  operator' <- desugarInfixOperator operator
  operand' <- desugarExpr operand
  pure (ExLam binder (ExApp (ExApp operator' operand') (ExVar (binderName binder))))

desugarSectionR :: TcAnnotation -> Syn.Name -> Syn.Expr -> ValueM Expr
desugarSectionR annotation operator operand = do
  binder <- freshBinder "_section" =<< sectionArgumentType annotation
  operator' <- desugarInfixOperator operator
  operand' <- desugarExpr operand
  pure (ExLam binder (ExApp (ExApp operator' (ExVar (binderName binder))) operand'))

sectionArgumentType :: TcAnnotation -> ValueM TcType
sectionArgumentType annotation =
  case tcAnnTermArgTypes annotation of
    [argumentType] -> pure argumentType
    argumentTypes -> failValue ("operator section has " <> show (length argumentTypes) <> " checked argument types")

desugarApplication :: Syn.Expr -> Syn.Expr -> ValueM Expr
desugarApplication function argument = do
  argument' <- desugarExpr argument
  ExApp <$> desugarExpr function <*> pure argument'

desugarVariable :: Maybe TcAnnotation -> Syn.Name -> ValueM Expr
desugarVariable maybeAnnotation name = do
  maybeNewtype <- newtypeConstructorData name
  case maybeNewtype of
    Just dataType -> do
      annotation <-
        case maybeAnnotation of
          Just value -> pure value
          Nothing -> do
            types <- gets vsTypes
            case Map.lookup (Syn.nameText name) types of
              Just constructorType -> pure (TcAnnotation constructorType [] [] [] [] [])
              Nothing -> failValue ("missing checked newtype constructor type " <> T.unpack (Syn.nameText name))
      desugarNewtypeConstructor annotation dataType
    Nothing -> do
      variable <- occurrenceName name
      case maybeAnnotation of
        Nothing -> pure (ExVar variable)
        Just annotation -> do
          inferredTypes <- localOccurrenceTypeArguments name annotation
          types <- mapM convertCheckedType inferredTypes
          evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
          pure (foldl ExApp (foldl ExTyApp (ExVar variable) types) evidence)

newtypeConstructorData :: Syn.Name -> ValueM (Maybe DataTypeInfo)
newtypeConstructorData name = do
  newtypes <- gets vsNewtypeConstructors
  pure (resolvedTermKey name >>= (`Map.lookup` newtypes))

desugarNewtypeConstructor :: TcAnnotation -> DataTypeInfo -> ValueM Expr
desugarNewtypeConstructor annotation dataType = do
  let (_, afterForAlls) = peelForAlls (tcAnnType annotation)
      (_, bodyType) = peelConstraints afterForAlls
  (argumentType, resultType) <-
    case bodyType of
      TcFunTy argument result -> pure (argument, result)
      _ -> failValue ("newtype constructor does not have a function type: " <> T.unpack (dtiName dataType))
  argument <- freshBinder "_newtype" argumentType
  let resultArguments =
        case resultType of
          TcTyCon _ arguments -> arguments
          _ -> []
      typeArguments =
        case tcAnnTypeArgs annotation of
          [] -> resultArguments
          arguments -> arguments
      tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  pure (ExLam argument (ExCast (ExVar (binderName argument)) (CoSym (CoAxiom axiom convertedArguments))))

convertNewtypeAxiomArguments :: DataTypeInfo -> [TcType] -> ValueM [Type]
convertNewtypeAxiomArguments dataType arguments =
  if length arguments > length (dtiTyVars dataType)
    then mapM convertCheckedType arguments
    else do
      env <- gets vsConvertEnv
      invisibleArguments <- liftEither (invisibleKindArgs env (dtiTyCon dataType) arguments Nothing)
      visibleArguments <- mapM convertCheckedType arguments
      pure (invisibleArguments <> visibleArguments)

convertTyConApplicationArguments :: TyCon -> [TcType] -> ValueM [Type]
convertTyConApplicationArguments tyCon arguments = do
  env <- gets vsConvertEnv
  invisibleArguments <- liftEither (invisibleKindArgs env tyCon arguments Nothing)
  visibleArguments <- mapM convertCheckedType arguments
  pure (invisibleArguments <> visibleArguments)

desugarLambda :: [Syn.Pattern] -> Syn.Expr -> ValueM Expr
desugarLambda patterns body = do
  types <- mapM (requiredPatternTypeFor "lambda argument") patterns
  binders <- zipWithM freshPatternBinder patterns types
  let locals = concat (zipWith patternLocalBindings patterns binders)
  body' <- withLocals locals (desugarExpr body)
  pure (foldr ExLam body' binders)

desugarList :: TcAnnotation -> [Syn.Expr] -> ValueM Expr
desugarList annotation elements = do
  elementType <-
    case tcAnnTypeArgs annotation of
      [ty] -> pure ty
      types -> failValue ("list annotation has " <> show (length types) <> " element types")
  convertedType <- convertCheckedType elementType
  elements' <- mapM desugarExpr elements
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) convertedType
      cons = ExTyApp (ExVar consName) convertedType
  pure (foldr (ExApp . ExApp cons) nil elements')

desugarListComp :: TcAnnotation -> ResolutionAnnotation -> Syn.Expr -> [Syn.CompStmt] -> ValueM Expr
desugarListComp annotation resolution body statements =
  case map peelCompStatement statements of
    [Syn.CompGen pattern' source] -> do
      resultType <-
        case tcAnnTypeArgs annotation of
          [ty] -> pure ty
          types -> failValue ("list comprehension has " <> show (length types) <> " element types")
      sourceListType <- requiredExprType source
      sourceType <-
        case sourceListType of
          TcTyCon tyCon [ty]
            | tyConName tyCon == "[]" -> pure ty
          ty -> failValue ("list comprehension source has non-list type " <> show ty)
      convertedSourceType <- convertCheckedType sourceType
      convertedResultType <- convertCheckedType resultType
      source' <- desugarExpr source
      binder <- freshPatternBinder pattern' sourceType
      mappedBody <- desugarDoPattern resultType binder sourceType pattern' (desugarExpr body)
      mapName <- resolutionValueName resolution
      let mapFunction = ExTyApp (ExTyApp (ExVar mapName) convertedSourceType) convertedResultType
      pure (ExApp (ExApp mapFunction (ExLam binder mappedBody)) source')
    unsupported -> failValue ("unsupported list comprehension statements: " <> take 80 (show unsupported))
  where
    peelCompStatement statement =
      case statement of
        Syn.CompAnn _ inner -> peelCompStatement inner
        _ -> statement

resolutionValueName :: ResolutionAnnotation -> ValueM Name
resolutionValueName resolution =
  case resolutionTarget resolution of
    ResolvedTopLevel package target ->
      pure (Name (Syn.nameText target) SortValue (OriginTop package (fromMaybe "" (Syn.nameQualifier target))))
    target -> failValue ("list comprehension map has invalid resolution: " <> show target)

desugarString :: TcAnnotation -> Text -> ValueM Expr
desugarString annotation value = do
  elementType <-
    case tcAnnType annotation of
      TcTyCon tyCon [ty]
        | tyConName tyCon == "[]" -> pure ty
      ty -> failValue ("string literal has non-list type " <> show ty)
  convertedType <- convertCheckedType elementType
  charConstructor <- uniqueConstructorName "C#"
  representation <- convertRuntimeRep WordRep
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) convertedType
      cons = ExTyApp (ExVar consName) convertedType
      boxedChar character = ExApp (ExVar charConstructor) (ExLit (LitChar representation character))
  pure (foldr (ExApp . ExApp cons . boxedChar) nil (T.unpack value))

desugarTuple :: TcAnnotation -> Syn.TupleFlavor -> [Maybe Syn.Expr] -> ValueM Expr
desugarTuple annotation flavor elements = do
  let elementTypes = tcAnnTypeArgs annotation
  unless (length elementTypes == length elements) $
    failValue ("tuple annotation has " <> show (length elementTypes) <> " element types for " <> show (length elements) <> " fields")
  convertedTypes <- mapM convertCheckedType elementTypes
  convertedElements <- zipWithM desugarTupleElement elementTypes elements
  representationTypes <-
    case flavor of
      Syn.Boxed -> pure []
      Syn.Unboxed -> mapM checkedRuntimeRep elementTypes
  let arity = length elements
  constructorName <- tupleConstructorName annotation flavor arity
  let constructor = ExVar constructorName
      applied = foldl ExApp (foldl ExTyApp constructor (representationTypes <> convertedTypes)) (map fst convertedElements)
  pure (foldr ExLam applied (concatMap snd convertedElements))

checkedRuntimeRep :: TcType -> ValueM Type
checkedRuntimeRep ty = do
  kindEnv <- gets (ceKindEnv . vsConvertEnv)
  liftEither (runtimeRepOfTypeInEnv kindEnv ty) >>= convertRuntimeRep

desugarTupleElement :: TcType -> Maybe Syn.Expr -> ValueM (Expr, [Binder])
desugarTupleElement _ (Just expression) = (,[]) <$> desugarExpr expression
desugarTupleElement ty Nothing = do
  binder <- freshBinder "_tuple_section" ty
  pure (ExVar (binderName binder), [binder])

tupleConstructorName :: TcAnnotation -> Syn.TupleFlavor -> Int -> ValueM Name
tupleConstructorName annotation flavor arity = do
  primPackage <- gets (cePrimPackage . vsConvertEnv)
  pure (Name constructorText SortDataConstructor (origin primPackage))
  where
    constructorText =
      case flavor of
        Syn.Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
        Syn.Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"
    origin primPackage =
      case tcAnnType annotation of
        TcTyCon tyCon _ -> OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)
        _ ->
          case flavor of
            Syn.Boxed -> OriginTop (PackageId "") "GHC.Tuple"
            Syn.Unboxed -> OriginTop primPackage "GHC.Types"

desugarDo :: [Syn.DoStmt Syn.Expr] -> ValueM Expr
desugarDo statements =
  case statements of
    [] -> failValue "do block has no statements"
    [statement] ->
      case peelDoStatement statement of
        Syn.DoExpr body -> desugarExpr body
        other -> failValue ("invalid final do statement: " <> take 80 (show other))
    statement : rest ->
      case peelDoStatement statement of
        Syn.DoLetDecls declarations -> desugarLocalDecls declarations (desugarDo rest)
        Syn.DoBind pattern' action -> do
          (annotation, resolution) <- requiredDoBindOccurrence statement
          bind <- desugarResolvedOccurrence annotation resolution
          action' <- desugarExpr action
          continuation <- desugarDoPatternContinuation annotation pattern' rest
          pure (ExApp (ExApp bind action') continuation)
        Syn.DoExpr action -> do
          (annotation, resolution) <- requiredDoBindOccurrence statement
          bind <- desugarResolvedOccurrence annotation resolution
          action' <- desugarExpr action
          argumentType <- doBindArgumentType annotation
          argument <- freshBinder "_do" argumentType
          continuation <- ExLam argument <$> desugarDo rest
          pure (ExApp (ExApp bind action') continuation)
        other -> failValue ("unsupported do statement: " <> take 80 (show other))

desugarDoPatternContinuation :: TcAnnotation -> Syn.Pattern -> [Syn.DoStmt Syn.Expr] -> ValueM Expr
desugarDoPatternContinuation annotation pattern' rest = do
  ty <- doBindArgumentType annotation
  binder <- freshPatternBinder pattern' ty
  let locals = directPatternBindings pattern' binder ty
  case locals of
    Just bindings -> ExLam binder <$> withLocals bindings (desugarDo rest)
    Nothing -> do
      resultType <- doBindResultType annotation
      body <- desugarDoPattern resultType binder ty pattern' (desugarDo rest)
      pure (ExLam binder body)

desugarDoPattern :: TcType -> Binder -> TcType -> Syn.Pattern -> ValueM Expr -> ValueM Expr
desugarDoPattern resultType binder ty pattern' success =
  case pattern' of
    Syn.PAnn _ inner -> desugarDoPattern resultType binder ty inner success
    Syn.PParen inner -> desugarDoPattern resultType binder ty inner success
    Syn.PStrict inner -> desugarDoPattern resultType binder ty inner success
    Syn.PIrrefutable inner -> desugarDoPattern resultType binder ty inner success
    Syn.PTypeSig inner _ -> desugarDoPattern resultType binder ty inner success
    Syn.PVar name -> withLocals [(Syn.unqualifiedNameText name, (binder, ty))] success
    Syn.PWildcard -> success
    Syn.PAs name inner ->
      withLocals
        [(Syn.unqualifiedNameText name, (binder, ty))]
        (desugarDoPattern resultType binder ty inner success)
    _ -> desugarDoConstructorPattern resultType binder ty pattern' success

desugarDoConstructorPattern :: TcType -> Binder -> TcType -> Syn.Pattern -> ValueM Expr -> ValueM Expr
desugarDoConstructorPattern resultType binder patternType' pattern' success = do
  maybeNewtype <- doPatternNewtype pattern'
  case maybeNewtype of
    Just dataType -> desugarDoNewtypePattern resultType binder pattern' dataType success
    Nothing -> do
      let children = patternChildren pattern'
          predicates = patternGivenPredicates pattern'
      let typeVariables = patternTypeVariables pattern'
      typeBinders <- convertTypeBinders typeVariables
      checkedFields <- checkedConstructorFieldTypes patternType' pattern'
      fieldTypes <- case checkedFields of
        Just types -> pure types
        Nothing ->
          case (peelPattern pattern', patternType') of
            (Syn.PTuple _ fields, TcTyCon _ types)
              | length fields == length types -> pure types
            _ -> patternFieldTypes pattern' children
      fields <- zipWithM freshPatternBinder children fieldTypes
      dictionaries <- zipWithM (freshDictionaryBinder "$pattern_d") [0 :: Int ..] predicates
      constructor <- patternConstructor pattern'
      resultType' <- convertCheckedType resultType
      caseBinder <- freshBinderFromType "_do_scrut" (binderType binder)
      body <-
        withDictionaries
          (zipWith Dictionary predicates dictionaries)
          (desugarDoChildPatterns resultType (zip3 fields fieldTypes children) success)
      pure (ExCase (ExVar (binderName binder)) caseBinder resultType' [Alt constructor typeBinders (dictionaries <> fields) body])

checkedConstructorFieldTypes :: TcType -> Syn.Pattern -> ValueM (Maybe [TcType])
checkedConstructorFieldTypes parentType pattern' =
  case patternConstructorSourceName pattern' of
    Nothing -> pure Nothing
    Just constructorName -> do
      constructors <- Map.findWithDefault [] (Syn.nameText constructorName) <$> gets vsConstructorInfos
      pure $
        listToMaybe
          [ map (applySubst substitution . dcfiType) (dciFields constructor)
          | constructor <- constructors,
            Just substitution <- [matchTypes [dciResTy constructor] [parentType]]
          ]

desugarDoChildPatterns :: TcType -> [(Binder, TcType, Syn.Pattern)] -> ValueM Expr -> ValueM Expr
desugarDoChildPatterns resultType children success =
  case children of
    [] -> success
    (binder, ty, pattern') : rest ->
      desugarDoPattern resultType binder ty pattern' (desugarDoChildPatterns resultType rest success)

doPatternNewtype :: Syn.Pattern -> ValueM (Maybe DataTypeInfo)
doPatternNewtype pattern' = do
  newtypes <- gets vsNewtypeConstructors
  pure $ do
    name <- patternConstructorSourceName pattern'
    key <- resolvedTermKey name
    Map.lookup key newtypes

desugarDoNewtypePattern :: TcType -> Binder -> Syn.Pattern -> DataTypeInfo -> ValueM Expr -> ValueM Expr
desugarDoNewtypePattern resultType binder pattern' dataType success = do
  child <-
    case patternChildren pattern' of
      [fieldPattern] -> pure fieldPattern
      _ -> failValue ("newtype do pattern does not have one field: " <> T.unpack (dtiName dataType))
  childType <- requiredPatternTypeFor "newtype do field" child
  field <- freshPatternBinder child childType
  typeArguments <- newtypePatternArguments pattern'
  convertedArguments <- convertNewtypeAxiomArguments dataType typeArguments
  let tyCon = dtiTyCon dataType
      axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      unwrapped = ExCast (ExVar (binderName binder)) (CoAxiom axiom convertedArguments)
  body <- desugarDoPattern resultType field childType child success
  pure (ExLet (Bind field unwrapped) body)

directPatternBindings :: Syn.Pattern -> Binder -> TcType -> Maybe [(Text, (Binder, TcType))]
directPatternBindings pattern' binder ty =
  case pattern' of
    Syn.PAnn _ inner -> directPatternBindings inner binder ty
    Syn.PParen inner -> directPatternBindings inner binder ty
    Syn.PStrict inner -> directPatternBindings inner binder ty
    Syn.PIrrefutable inner -> directPatternBindings inner binder ty
    Syn.PTypeSig inner _ -> directPatternBindings inner binder ty
    Syn.PVar name -> Just [(Syn.unqualifiedNameText name, (binder, ty))]
    Syn.PWildcard -> Just []
    Syn.PAs name inner -> ((Syn.unqualifiedNameText name, (binder, ty)) :) <$> directPatternBindings inner binder ty
    _ -> Nothing

peelDoStatement :: Syn.DoStmt body -> Syn.DoStmt body
peelDoStatement statement =
  case statement of
    Syn.DoAnn _ inner -> peelDoStatement inner
    _ -> statement

requiredDoBindOccurrence :: Syn.DoStmt Syn.Expr -> ValueM (TcAnnotation, ResolutionAnnotation)
requiredDoBindOccurrence statement =
  case doBindOccurrence statement of
    Just occurrence -> pure occurrence
    Nothing -> failValue ("missing checked >>= occurrence: " <> take 80 (show statement))

doBindOccurrence :: Syn.DoStmt Syn.Expr -> Maybe (TcAnnotation, ResolutionAnnotation)
doBindOccurrence = go Nothing Nothing
  where
    go maybeAnnotation maybeResolution statement =
      case statement of
        Syn.DoAnn annotation inner ->
          go
            ((Syn.fromAnnotation annotation :: Maybe TcAnnotation) <|> maybeAnnotation)
            ((Syn.fromAnnotation annotation :: Maybe ResolutionAnnotation) <|> maybeResolution)
            inner
        _ -> (,) <$> maybeAnnotation <*> maybeResolution

doBindArgumentType :: TcAnnotation -> ValueM TcType
doBindArgumentType annotation =
  case tcAnnType annotation of
    TcFunTy _ (TcFunTy (TcFunTy argumentType _) _) -> pure argumentType
    ty -> failValue ("invalid checked >>= type: " <> show ty)

doBindResultType :: TcAnnotation -> ValueM TcType
doBindResultType annotation =
  case tcAnnType annotation of
    TcFunTy _ (TcFunTy _ resultType) -> pure resultType
    ty -> failValue ("invalid checked >>= result type: " <> show ty)

desugarCase :: TcType -> Syn.Expr -> [Syn.CaseAlt Syn.Expr] -> ValueM Expr
desugarCase resultType scrutinee alternatives = do
  scrutinee' <- desugarExpr scrutinee
  scrutineeType <- requiredExprType scrutinee
  convertedType <- convertCheckedType scrutineeType
  case alternatives of
    [] -> do
      binder <- freshBinder "_case" scrutineeType
      resultType' <- convertCheckedType resultType
      pure (ExCase scrutinee' binder resultType' [])
    _ -> do
      let matches = map caseAlternativeMatch alternatives
      case scrutinee' of
        ExVar name ->
          withBinderTypes [(name, scrutineeType)] (desugarMatchArguments resultType [Binder name convertedType] matches)
        _ -> do
          binder <- freshBinder "_case" scrutineeType
          body <- withBinderTypes [(binderName binder, scrutineeType)] (desugarMatchArguments resultType [binder] matches)
          pure (ExLet (Bind binder scrutinee') body)

caseAlternativeMatch :: Syn.CaseAlt Syn.Expr -> Syn.Match
caseAlternativeMatch alternative =
  case alternative of
    Syn.CaseAlt annotations pattern' rhs ->
      (emptyMatch rhs)
        { Syn.matchAnns = annotations,
          Syn.matchPats = [pattern']
        }

desugarLocalDecls :: [Syn.Decl] -> ValueM Expr -> ValueM Expr
desugarLocalDecls declarations body = do
  groups <- groupValues declarations
  allocated <- mapM allocateLocal groups
  withLocals [(name, (binder, ty)) | (name, binder, ty, _) <- allocated] $ do
    binds <- mapM desugarLocal allocated
    body' <- body
    let recursiveBinds = [bind | (True, bind) <- binds]
        patternBinds = [bind | (False, bind) <- binds]
        recursiveBody = if null recursiveBinds then body' else ExRec recursiveBinds body'
    pure (foldr ExLet recursiveBody patternBinds)
  where
    allocateLocal group = do
      let name = groupName group
          ty = groupType group
      binder <- freshBinder name ty
      pure (name, binder, ty, group)
    desugarLocal (_, binder, ty, group) = do
      rhs <-
        case group of
          FunctionGroup _ matches _ -> desugarMatches ty matches
          PatternGroup name pattern' sourceRhs _
            | isJust (barePatternName pattern') -> desugarMatches ty [emptyMatch sourceRhs]
            | otherwise -> desugarPatternBindingScheme name pattern' sourceRhs ty
      recursive <-
        case group of
          FunctionGroup {} -> pure True
          PatternGroup _ pattern' _ _
            | isJust (barePatternName pattern') -> pure True
            | otherwise -> do
                representation <- checkedRuntimeRep ty
                lifted <- gets (liftedRepType . vsConvertEnv)
                pure (representation == lifted)
      pure (recursive, Bind binder rhs)

desugarEvidence :: Ev.EvTerm -> ValueM Expr
desugarEvidence evidence =
  case evidence of
    Ev.EvVarTerm variable -> failValue ("unresolved evidence variable: " <> show variable)
    Ev.EvGiven predicate -> do
      dictionaries <- gets vsDictionaries
      case Map.lookup (predicateKey predicate) dictionaries of
        Just binder -> pure (ExVar (binderName binder))
        Nothing -> failValue ("missing given dictionary for " <> show predicate)
    Ev.EvDict origin dictionaryName types subEvidence -> do
      convertedTypes <- mapM convertCheckedType types
      evidenceArguments <- mapM desugarEvidence subEvidence
      let (packageName, moduleName') = origin
          package = PackageId packageName
          name = Name dictionaryName SortValue (OriginTop package moduleName')
      pure (foldl ExApp (foldl ExTyApp (ExVar name) convertedTypes) evidenceArguments)
    Ev.EvCoercion coercion -> ExCast (ExVar (Name "coercion" SortValue (OriginLocal (Unique 0)))) <$> convertCoercion coercion
    Ev.EvSuperClass source _ sourcePredicate fieldTypes fieldIndex -> do
      sourceExpression <- desugarEvidence source
      (classTyCon, sourceType) <-
        case sourcePredicate of
          ClassPred classTyCon arguments -> pure (classTyCon, TcTyCon classTyCon arguments)
          EqPred {} -> failValue "cannot select a superclass from equality evidence"
          QuantifiedPred {} -> failValue "cannot select a superclass from quantified evidence before application"
      sourceBinder <- freshBinder "$super_source" sourceType
      fieldBinders <- zipWithM (freshIndexedBinder "$super_field") [0 :: Int ..] fieldTypes
      selected <-
        case drop fieldIndex fieldBinders of
          field : _ -> pure field
          [] -> failValue "superclass field index is outside the dictionary layout"
      resultType <-
        case drop fieldIndex fieldTypes of
          fieldType : _ -> convertCheckedType fieldType
          [] -> failValue "superclass field type index is outside the dictionary layout"
      pure
        ( ExCase
            sourceExpression
            sourceBinder
            resultType
            [Alt (AltData (classDictConName classTyCon)) [] fieldBinders (ExVar (binderName selected))]
        )
    Ev.EvCast inner coercion -> ExCast <$> desugarEvidence inner <*> convertCoercion coercion
    Ev.EvTypeable origin ty arguments -> desugarTypeableEvidence origin ty arguments
    Ev.EvTypeLam variable body ->
      ExTyLam <$> convertTypeBinder variable <*> desugarEvidence body
    Ev.EvDictLam predicate binderType body -> do
      binder <- freshBinder "$quantified_d" binderType
      body' <- withDictionaries [Dictionary predicate binder] (desugarEvidence body)
      pure (ExLam binder body')
    Ev.EvTypeApp function argument ->
      ExTyApp <$> desugarEvidence function <*> convertCheckedType argument
    Ev.EvDictApp function argument ->
      ExApp <$> desugarEvidence function <*> desugarEvidence argument

desugarTypeableEvidence :: Maybe (Text, Text) -> TcType -> [Ev.EvTerm] -> ValueM Expr
desugarTypeableEvidence origin ty argumentEvidence = do
  (_, argumentTypes) <- typeableTypeView ty
  unless (length argumentTypes == length argumentEvidence) (failValue "Typeable evidence argument count does not match its type")
  argumentRepresentations <- zipWithM (desugarTypeableArgument origin) argumentTypes argumentEvidence
  representation <- desugarTypeRepresentation origin ty argumentRepresentations
  convertedType <- convertCheckedType ty
  proxyName <- typeableName origin "Data.Proxy" "Proxy" SortTypeConstructor
  let proxyType = TyApp (TyCon proxyName) convertedType
  proxyBinder <- freshBinderFromType "$typeable_proxy" proxyType
  valueBinder <- freshBinderFromType "$typeable_value" convertedType
  dictionaryConstructor <- typeableName origin "Type.Reflection" "$Dict$Typeable" SortDataConstructor
  pure
    ( ExApp
        (ExApp (ExTyApp (ExVar dictionaryConstructor) convertedType) (ExLam proxyBinder representation))
        (ExLam valueBinder representation)
    )

desugarTypeableArgument :: Maybe (Text, Text) -> TcType -> Ev.EvTerm -> ValueM Expr
desugarTypeableArgument origin ty evidence = do
  dictionary <- desugarEvidence evidence
  convertedType <- convertCheckedType ty
  selector <- typeableName origin "Type.Reflection" "typeRep" SortValue
  someTypeRepConstructor <- typeableName origin "Type.Reflection" "SomeTypeRep" SortDataConstructor
  proxyConstructor <- typeableName origin "Data.Proxy" "Proxy" SortDataConstructor
  let proxy = ExTyApp (ExVar proxyConstructor) convertedType
      typeRepValue = ExApp (ExApp (ExTyApp (ExVar selector) convertedType) dictionary) proxy
  pure (ExApp (ExTyApp (ExVar someTypeRepConstructor) convertedType) typeRepValue)

desugarTypeRepresentation :: Maybe (Text, Text) -> TcType -> [Expr] -> ValueM Expr
desugarTypeRepresentation origin ty arguments = do
  (typeName, _) <- typeableTypeView ty
  convertedType <- convertCheckedType ty
  someTypeRepName <- typeableName origin "Type.Reflection" "SomeTypeRep" SortTypeConstructor
  typeRepConstructor <- typeableName origin "Type.Reflection" "TypeRep" SortDataConstructor
  tyConAxiom <- typeableName origin "Type.Reflection" "$ax$TyCon" SortAxiom
  charName <- typeableName origin "GHC.Internal.Char" "Char" SortTypeConstructor
  charConstructor <- typeableName origin "GHC.Internal.Char" "C#" SortDataConstructor
  wordRep <- convertRuntimeRep WordRep
  let someTypeRepType = TyCon someTypeRepName
      charType = TyCon charName
      typeNameChars =
        [ ExApp (ExVar charConstructor) (ExLit (LitChar wordRep character))
        | character <- T.unpack typeName
        ]
  nameList <- desugarFcList charType typeNameChars
  argumentList <- desugarFcList someTypeRepType arguments
  let tyCon = ExCast nameList (CoSym (CoAxiom tyConAxiom []))
  pure (ExApp (ExApp (ExTyApp (ExVar typeRepConstructor) convertedType) tyCon) argumentList)

desugarFcList :: Type -> [Expr] -> ValueM Expr
desugarFcList elementType elements = do
  nilName <- primitiveName "GHC.Types" "[]" SortDataConstructor
  consName <- primitiveName "GHC.Types" ":" SortDataConstructor
  let nil = ExTyApp (ExVar nilName) elementType
      cons item = ExApp (ExApp (ExTyApp (ExVar consName) elementType) item)
  pure (foldr cons nil elements)

typeableName :: Maybe (Text, Text) -> Text -> Text -> Sort -> ValueM Name
typeableName origin fallbackModule name sort =
  case origin of
    Just (packageName, moduleName') ->
      let selectedModule = if fallbackModule == "Type.Reflection" then moduleName' else fallbackModule
       in pure (Name name sort (OriginTop (PackageId packageName) selectedModule))
    Nothing -> failValue ("Typeable evidence has no origin for " <> T.unpack name)

typeableTypeView :: TcType -> ValueM (Text, [TcType])
typeableTypeView ty =
  case ty of
    TcTyCon tyCon arguments -> pure (tyConName tyCon, arguments)
    TcFunTy argument result -> pure ("(->)", [argument, result])
    _ -> failValue ("cannot construct Typeable evidence for " <> show ty)

desugarResolvedOccurrence :: TcAnnotation -> ResolutionAnnotation -> ValueM Expr
desugarResolvedOccurrence annotation resolution = do
  name <- resolvedAnnotationName resolution
  types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
  evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
  pure (foldl ExApp (foldl ExTyApp (ExVar name) types) evidence)

resolvedAnnotationName :: ResolutionAnnotation -> ValueM Name
resolvedAnnotationName resolution =
  case resolutionTarget resolution of
    ResolvedTopLevel package target ->
      pure
        ( Name
            (Syn.nameText target)
            (sourceNameSort target)
            (OriginTop package (fromMaybe "" (Syn.nameQualifier target)))
        )
    ResolvedLocal _ localName -> do
      local <- Map.lookup (Syn.unqualifiedNameText localName) <$> gets vsLocals
      case local of
        Just (binder, _) -> pure (binderName binder)
        Nothing -> failValue ("missing local occurrence " <> T.unpack (Syn.unqualifiedNameText localName))
    ResolvedSyntax -> failValue ("syntax identifier reached ordinary occurrence " <> T.unpack (displayIdentifier (resolutionIdentifier resolution)))
    ResolvedError message -> failValue message

desugarOverloadedInteger :: TcAnnotation -> ResolutionAnnotation -> Integer -> ValueM Expr
desugarOverloadedInteger annotation resolution value = do
  fromIntegerExpression <- desugarResolvedOccurrence annotation resolution
  integer <- desugarIntegerLiteral value
  pure (ExApp fromIntegerExpression integer)

desugarArithSeq :: TcAnnotation -> ResolutionAnnotation -> Syn.ArithSeq -> ValueM Expr
desugarArithSeq annotation resolution arithSeq = do
  method <- desugarResolvedOccurrence annotation resolution
  arguments <- mapM desugarExpr (arithSeqArguments arithSeq)
  pure (foldl ExApp method arguments)

arithSeqArguments :: Syn.ArithSeq -> [Syn.Expr]
arithSeqArguments arithSeq =
  case arithSeq of
    Syn.ArithSeqAnn _ inner -> arithSeqArguments inner
    Syn.ArithSeqFrom from -> [from]
    Syn.ArithSeqFromThen from then' -> [from, then']
    Syn.ArithSeqFromTo from to -> [from, to]
    Syn.ArithSeqFromThenTo from then' to -> [from, then', to]

desugarIntegerLiteral :: Integer -> ValueM Expr
desugarIntegerLiteral value = do
  constructor <- uniqueConstructorName "IS"
  intRepresentation <- convertRuntimeRep IntRep
  wordRepresentation <- convertRuntimeRep WordRep
  let small integer = ExApp (ExVar constructor) (ExLit (LitInt intRepresentation integer))
      coreName text = Name text SortValue (nameOrigin constructor)
      apply name = foldl ExApp (ExVar (coreName name))
      word integer = ExLit (LitInt wordRepresentation integer)
      positive integer
        | integer <= maxInt = small integer
        | integer <= maxWord =
            apply "integerFromTwoWords#" [ExLit (LitInt intRepresentation 1), word 0, word integer]
        | otherwise =
            let (high, low) = integer `quotRem` wordBase
                shifted = apply "integerShiftL#" [positive high, ExLit (LitInt intRepresentation 64)]
             in apply "integerAdd" [shifted, positive low]
      magnitude = positive (abs value)
  pure
    ( if value >= minInt && value <= maxInt
        then small value
        else if value < 0 then apply "integerNegate" [magnitude] else magnitude
    )
  where
    wordBase = 18446744073709551616
    maxWord = wordBase - 1
    maxInt = 9223372036854775807
    minInt = -9223372036854775808

uniqueConstructorName :: Text -> ValueM Name
uniqueConstructorName name = do
  constructors <- Map.findWithDefault [] name <$> gets vsConstructors
  case constructors of
    [constructor] -> pure constructor
    [] -> failValue ("missing constructor " <> T.unpack name)
    _ -> failValue ("ambiguous constructor " <> T.unpack name)

convertCoercion :: Ev.Coercion -> ValueM Coercion
convertCoercion coercion =
  case coercion of
    Ev.CoVar (Ev.EvVar unique) -> pure (CoVar (Name "c" SortValue (OriginLocal unique)))
    Ev.Refl ty -> CoRefl <$> convertCheckedType ty
    Ev.Sym inner -> CoSym <$> convertCoercion inner
    Ev.Trans left right -> CoTrans <$> convertCoercion left <*> convertCoercion right
    Ev.TyConAppCo tyCon arguments -> do
      env <- gets vsConvertEnv
      CoTyConApp (tyConNameFc env tyCon) <$> mapM convertCoercion arguments
    Ev.AxiomInstCo name arguments -> do
      env <- gets vsConvertEnv
      CoAxiom (lookupAxiomName env name) <$> mapM convertCheckedType arguments

occurrenceName :: Syn.Name -> ValueM Name
occurrenceName sourceName = do
  let localText = Syn.nameText sourceName
  local <- Map.lookup localText <$> gets vsLocals
  case local of
    Just (binder, _) -> pure (binderName binder)
    Nothing -> resolvedTermName sourceName

resolvedTermName :: Syn.Name -> ValueM Name
resolvedTermName sourceName =
  case termResolution sourceName of
    Just resolution ->
      case resolutionTarget resolution of
        ResolvedTopLevel package target ->
          pure
            ( Name
                (Syn.nameText target)
                (sourceNameSort target)
                (OriginTop package (fromMaybe "" (Syn.nameQualifier target)))
            )
        ResolvedSyntax -> failValue ("syntax identifier reached ordinary term " <> T.unpack (Syn.nameText sourceName))
        ResolvedLocal _ localName -> do
          local <- Map.lookup (Syn.unqualifiedNameText localName) <$> gets vsLocals
          case local of
            Just (binder, _) -> pure (binderName binder)
            Nothing ->
              failValue
                ( "missing local value "
                    <> T.unpack (Syn.unqualifiedNameText localName)
                    <> " at "
                    <> show (resolutionSpan resolution)
                )
        ResolvedError message -> failValue message
    Nothing -> failValue ("missing resolved value " <> T.unpack (Syn.nameText sourceName))

resolvedTermKey :: Syn.Name -> Maybe (PackageId, Text, Text)
resolvedTermKey sourceName = do
  resolution <- termResolution sourceName
  case resolutionTarget resolution of
    ResolvedTopLevel package target -> Just (package, fromMaybe "" (Syn.nameQualifier target), Syn.nameText target)
    _ -> Nothing

termResolution :: Syn.Name -> Maybe ResolutionAnnotation
termResolution sourceName =
  listToMaybe
    [ resolution
    | resolution <- mapMaybe Syn.fromAnnotation (Syn.nameAnns sourceName),
      resolutionNamespace resolution == ResolutionNamespaceTerm
    ]

sourceNameSort :: Syn.Name -> Sort
sourceNameSort sourceName =
  case T.uncons (Syn.nameText sourceName) of
    Just (first, _)
      | first == ':' || first == '[' || first == '(' || isAsciiUpper first -> SortDataConstructor
    _ -> SortValue

topName :: (PackageId, Text) -> Text -> Name
topName (package, moduleName') name = Name name SortValue (OriginTop package moduleName')

primitiveName :: Text -> Text -> Sort -> ValueM Name
primitiveName moduleName' name sort = do
  package <- gets (cePrimPackage . vsConvertEnv)
  pure (Name name sort (OriginTop package moduleName'))

freshArgument :: Int -> TcType -> ValueM Binder
freshArgument index = freshBinder (argumentName index)

argumentName :: Int -> Text
argumentName index
  | index < 3 = T.singleton (['x', 'y', 'z'] !! index)
  | otherwise = "x" <> T.pack (show (index - 2))

freshIndexedBinder :: Text -> Int -> TcType -> ValueM Binder
freshIndexedBinder prefix index = freshBinder (prefix <> T.pack (show index))

freshDictionaryBinder :: Text -> Int -> Pred -> ValueM Binder
freshDictionaryBinder prefix index predicate = do
  unique <- freshUnique
  env <- gets vsConvertEnv
  ty <- liftEither (convertPred env predicate)
  pure (Binder (Name (prefix <> T.pack (show index)) SortValue (OriginLocal unique)) ty)

freshBinder :: Text -> TcType -> ValueM Binder
freshBinder name ty = do
  unique <- freshUnique
  converted <- convertCheckedType ty
  pure (Binder (Name name SortValue (OriginLocal unique)) converted)

freshBinderFromType :: Text -> Type -> ValueM Binder
freshBinderFromType name ty = do
  unique <- freshUnique
  pure (Binder (Name name SortValue (OriginLocal unique)) ty)

freshUnique :: ValueM Unique
freshUnique = do
  next <- gets vsNextUnique
  modify' (\state -> state {vsNextUnique = next + 1})
  pure (Unique next)

lookupCheckedType :: Text -> ValueM TcType
lookupCheckedType name = do
  types <- gets vsTypes
  case Map.lookup name types of
    Just ty -> pure ty
    Nothing -> do
      moduleOrigin <- gets vsModuleOrigin
      locals <- gets vsLocals
      failValue
        ( "missing checked type for "
            <> T.unpack name
            <> " in "
            <> show moduleOrigin
            <> "; local values: "
            <> show (Map.keys locals)
        )

requiredExprType :: Syn.Expr -> ValueM TcType
requiredExprType expression =
  case exprType expression of
    Just ty -> pure ty
    Nothing -> inferExprType expression

inferExprType :: Syn.Expr -> ValueM TcType
inferExprType expression =
  case expression of
    Syn.EAnn _ inner -> inferExprType inner
    Syn.EVar name -> do
      local <- Map.lookup (Syn.nameText name) <$> gets vsLocals
      case local of
        Just (_, ty) -> pure ty
        Nothing -> lookupCheckedType (Syn.nameText name)
    Syn.EApp function _ -> do
      functionType <- inferExprType function
      case applicationResultType functionType of
        Just result -> pure result
        Nothing -> failValue ("application head is not a checked function: " <> show functionType)
    Syn.EInfix _ operator _ -> do
      operatorType <-
        maybe
          (lookupCheckedType (Syn.nameText operator))
          (pure . tcAnnType)
          (listToMaybe (mapMaybe Syn.fromAnnotation (Syn.nameAnns operator)))
      case applicationResultType operatorType >>= applicationResultType of
        Just result -> pure result
        Nothing -> failValue ("infix operator is not a checked binary function: " <> show operatorType)
    Syn.EParen inner -> inferExprType inner
    Syn.ETypeSig inner _ -> inferExprType inner
    Syn.ETypeApp inner _ -> inferExprType inner
    unsupported -> failValue ("missing checked expression type: " <> take 80 (show unsupported))

exprType :: Syn.Expr -> Maybe TcType
exprType expression =
  case expression of
    Syn.EAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> exprType inner
    Syn.EApp function _ -> exprType function >>= applicationResultType
    Syn.EParen inner -> exprType inner
    Syn.ETypeSig inner _ -> exprType inner
    Syn.ETypeApp inner _ -> exprType inner
    _ -> Nothing

applicationResultType :: TcType -> Maybe TcType
applicationResultType ty =
  case ty of
    TcForAllTy _ body -> applicationResultType body
    TcQualTy _ body -> applicationResultType body
    TcFunTy _ result -> Just result
    _ -> Nothing

convertCheckedType :: TcType -> ValueM Type
convertCheckedType ty = do
  env <- gets vsConvertEnv
  case convertType env ty of
    Left message -> failValue (message <> " while converting " <> show ty)
    Right converted -> pure converted

convertTypeBinder :: TyVarId -> ValueM Binder
convertTypeBinder tyVar = do
  env <- gets vsConvertEnv
  liftEither (tyVarBinder env tyVar)

convertTypeBinders :: [TyVarId] -> ValueM [Binder]
convertTypeBinders variables =
  withTypeVariables variables (mapM convertTypeBinder variables)

convertRuntimeRep :: TcType -> ValueM Type
convertRuntimeRep runtimeRep = do
  env <- gets vsConvertEnv
  liftEither (convertRep env runtimeRep)

numericRepresentation :: Syn.NumericType -> TcType
numericRepresentation numericType =
  case numericType of
    Syn.TInteger -> IntRep
    Syn.TIntHash -> IntRep
    Syn.TWordHash -> WordRep
    Syn.TInt8Hash -> Int8Rep
    Syn.TInt16Hash -> Int16Rep
    Syn.TInt32Hash -> Int32Rep
    Syn.TInt64Hash -> Int64Rep
    Syn.TWord8Hash -> Word8Rep
    Syn.TWord16Hash -> Word16Rep
    Syn.TWord32Hash -> Word32Rep
    Syn.TWord64Hash -> Word64Rep

withLocals :: [(Text, (Binder, TcType))] -> ValueM a -> ValueM a
withLocals additions action = do
  previous <- gets vsLocals
  modify' (\state -> state {vsLocals = foldr (uncurry Map.insert) previous additions})
  result <- action
  modify' (\state -> state {vsLocals = previous})
  pure result

withBinderTypes :: [(Name, TcType)] -> ValueM a -> ValueM a
withBinderTypes additions action = do
  previous <- gets vsBinderTypes
  modify' (\state -> state {vsBinderTypes = foldr (uncurry Map.insert) previous additions})
  result <- action
  modify' (\state -> state {vsBinderTypes = previous})
  pure result

withDictionaries :: [Dictionary] -> ValueM a -> ValueM a
withDictionaries additions action = do
  previous <- gets vsDictionaries
  let updated = foldr insertDictionary previous additions
  modify' (\state -> state {vsDictionaries = updated})
  result <- action
  modify' (\state -> state {vsDictionaries = previous})
  pure result
  where
    insertDictionary dictionary =
      Map.insert (predicateKey (dictionaryPredicate dictionary)) (dictionaryBinder dictionary)

predicateKey :: Pred -> Text
predicateKey predicate =
  case predicate of
    ClassPred classTyCon arguments -> dictionaryKey classTyCon arguments
    EqPred left right -> typeKey left <> "~" <> typeKey right
    QuantifiedPred {} -> "quantified:" <> T.pack (show predicate)

dictionaryKey :: TyCon -> [TcType] -> Text
dictionaryKey classTyCon arguments =
  packageIdText (tyConPackageId classTyCon)
    <> ":"
    <> tyConModuleName classTyCon
    <> ":"
    <> tyConName classTyCon
    <> T.concat (map ((":" <>) . typeKey) arguments)

typeKey :: TcType -> Text
typeKey ty = T.pack (show ty)

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls ty =
  case ty of
    TcForAllTy tyVar body ->
      let (tyVars, result) = peelForAlls body
       in (tyVar : tyVars, result)
    _ -> ([], ty)

peelConstraints :: TcType -> ([Pred], TcType)
peelConstraints ty =
  case ty of
    TcQualTy predicates body -> (predicates, body)
    _ -> ([], ty)

peelFunctions :: Int -> TcType -> ([TcType], TcType)
peelFunctions count ty
  | count <= 0 = ([], ty)
peelFunctions count (TcFunTy argument result) =
  let (arguments, finalResult) = peelFunctions (count - 1) result
   in (argument : arguments, finalResult)
peelFunctions _ ty = ([], ty)

liftEither :: Either String a -> ValueM a
liftEither = either failValue pure

failValue :: String -> ValueM a
failValue = lift . Left
