{-# LANGUAGE OverloadedStrings #-}

-- | Direct value desugaring from checked source syntax to System FC 2.
module Aihc.Fc2.Desugar.Value
  ( desugarValues,
  )
where

import Aihc.Fc2.Convert
import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Parser.Syntax qualified as Syn
import Aihc.Resolve
  ( PackageId (..),
    ResolutionAnnotation (..),
    ResolutionNamespace (..),
    ResolvedName (..),
    packageIdText,
  )
import Aihc.Tc
  ( DataConInfo (..),
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
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
  )
import Aihc.Tc.Evidence qualified as Ev
import Aihc.Tc.Types
  ( Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon,
    TyVarId,
    Unique (..),
    tyConModuleName,
    tyConName,
    tyConPackageId,
  )
import Control.Applicative ((<|>))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import Data.ByteString qualified as BS
import Data.Char (isAsciiUpper)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data ValueState = ValueState
  { vsNextUnique :: !Int,
    vsModuleOrigin :: !(PackageId, Text),
    vsConvertEnv :: !ConvertEnv,
    vsTypes :: !(Map Text TcType),
    vsLocals :: !(Map Text (Binder, TcType)),
    vsDictionaries :: !(Map Text Binder),
    vsNewtypeConstructors :: !(Map (PackageId, Text, Text) DataTypeInfo)
  }

type ValueM = StateT ValueState (Either String)

data ValueGroup
  = FunctionGroup !Text ![Syn.Match] !(Maybe TcType)
  | PatternGroup !Text !(Syn.Rhs Syn.Expr) !(Maybe TcType)

data TopValue = TopValue
  { topCoreName :: !Name,
    topType :: !TcType,
    topGroup :: !ValueGroup
  }

data Dictionary = Dictionary
  { dictionaryPredicate :: !Pred,
    dictionaryBinder :: !Binder
  }

desugarValues :: ConvertEnv -> [TcBindingResult] -> TcInterface -> (PackageId, Text) -> Syn.Module -> Either String [Decl]
desugarValues convertEnv bindings interface moduleOrigin checked = do
  let typeEntries = Map.fromList [(tbName binding, tbType binding) | binding <- bindings]
      newtypes =
        Map.fromList
          [ ((package, moduleName', dciName constructor), dataType)
          | dataType <- tcInterfaceDataTypes interface,
            dtiFlavor dataType == NewtypeTyCon,
            constructor <- dtiConstructors dataType,
            let (package, moduleName') = dciOrigin constructor
          ]
      initialState =
        ValueState
          { vsNextUnique = 1000,
            vsModuleOrigin = moduleOrigin,
            vsConvertEnv = convertEnv,
            vsTypes = typeEntries,
            vsLocals = Map.empty,
            vsDictionaries = Map.empty,
            vsNewtypeConstructors = newtypes
          }
  fst <$> runStateT (desugarModuleValues checked) initialState

desugarModuleValues :: Syn.Module -> ValueM [Decl]
desugarModuleValues checked = do
  phaseOne <- concat <$> mapM desugarEarlyDecl (Syn.moduleDecls checked)
  instances <- concat <$> mapM desugarInstanceDecl (Syn.moduleDecls checked)
  tops <- mapM allocateTopValue (groupValues (Syn.moduleDecls checked))
  values <- mapM desugarTopValue tops
  pure (phaseOne <> instances <> map DeclVal values)

desugarEarlyDecl :: Syn.Decl -> ValueM [Decl]
desugarEarlyDecl declaration =
  case declaration of
    Syn.DeclAnn annotation inner
      | Just classAnnotation <- Syn.fromAnnotation annotation,
        Syn.DeclClass classDecl <- Syn.peelDeclAnn inner ->
          desugarClassSelectors classDecl classAnnotation
      | Just tcAnnotation <- Syn.fromAnnotation annotation,
        Syn.DeclForeign foreignDecl <- Syn.peelDeclAnn inner ->
          desugarForeign tcAnnotation foreignDecl
      | otherwise -> desugarEarlyDecl inner
    _ -> pure []

desugarForeign :: TcAnnotation -> Syn.ForeignDecl -> ValueM [Decl]
desugarForeign annotation foreignDecl =
  case Syn.foreignCallConv foreignDecl of
    Syn.CPrim -> do
      _ <- freshUnique
      moduleOrigin <- gets vsModuleOrigin
      ty <- convertCheckedType (tcAnnType annotation)
      let valueName = Syn.unqualifiedNameText (Syn.foreignName foreignDecl)
      pure
        [ DeclPrim
            PrimDecl
              { primVis = Pub,
                primName = topName moduleOrigin valueName,
                primType = ty
              }
        ]
    _ -> failValue "System FC 2 accepts only foreign import prim"

desugarClassSelectors :: Syn.ClassDecl -> TcClassAnnotation -> ValueM [Decl]
desugarClassSelectors classDecl classAnnotation = do
  let classTyVars = tcClassTyVars classAnnotation
      className = Syn.unqualifiedNameText (Syn.binderHeadName (Syn.classDeclHead classDecl))
  methodTypes <- mapM (methodFieldType className classTyVars) (tcClassMethods classAnnotation)
  let fieldTypes = map tcDictBinderType (tcClassSuperClasses classAnnotation) <> methodTypes
      superClassCount = length (tcClassSuperClasses classAnnotation)
  mapM (desugarSelector (tcClassTyCon classAnnotation) classTyVars fieldTypes superClassCount) (tcClassMethods classAnnotation)

desugarSelector :: TyCon -> [TyVarId] -> [TcType] -> Int -> TcClassMethodAnnotation -> ValueM Decl
desugarSelector classTyCon classTyVars fieldTypes superClassCount method = do
  _ <- freshUnique
  let (typeVariables, afterForAlls) = peelForAlls (tcClassMethodType method)
      (predicates, _) = peelConstraints afterForAlls
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
          [Alt (AltData (classDictConName classTyCon)) fields selectedExpr]
  typeBinders <- mapM convertTypeBinder typeVariables
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
desugarInstance annotation instanceDecl = do
  let methods = Map.fromListWith appendMatches (instanceMethods instanceDecl)
  contextDictionaries <- zipWithM makeContextDictionary [0 :: Int ..] (tcInstanceContextDicts annotation)
  fields <- withDictionaries contextDictionaries $ mapM (desugarInstanceMethod contextDictionaries methods) (tcInstanceMethodOrder annotation)
  _ <- freshUnique
  _ <- freshUnique
  typeBinders <- mapM convertTypeBinder (tcInstanceTyVars annotation)
  headTypes <- mapM convertCheckedType (tcInstanceHeadTypes annotation)
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

desugarInstanceMethod :: [Dictionary] -> Map Text (TcType, [Syn.Match]) -> Text -> ValueM Expr
desugarInstanceMethod dictionaries methods methodName =
  case Map.lookup methodName methods of
    Just (methodType, matches) -> withDictionaries dictionaries (desugarMatches methodType matches)
    Nothing -> failValue ("missing method " <> T.unpack methodName <> " in instance dictionary")

makeContextDictionary :: Int -> TcDictBinderAnnotation -> ValueM Dictionary
makeContextDictionary index annotation = do
  binder <- freshBinder ("$d" <> T.pack (show index)) (tcDictBinderType annotation)
  case tcDictBinderType annotation of
    TcTyCon classTyCon _ -> pure (Dictionary (ClassPred classTyCon (tcDictBinderArgs annotation)) binder)
    other -> failValue ("invalid checked class dictionary type: " <> show other)

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
        Syn.InstanceItemAnn _ inner -> methodItem methodAnnotation inner
        Syn.InstanceItemBind (Syn.FunctionBind _ matches) ->
          [(tcInstanceMethodName methodAnnotation, (tcInstanceMethodType methodAnnotation, matches))]
        Syn.InstanceItemBind (Syn.PatternBind _ _ rhs) ->
          [(tcInstanceMethodName methodAnnotation, (tcInstanceMethodType methodAnnotation, [emptyMatch rhs]))]
        _ -> []

groupValues :: [Syn.Decl] -> [ValueGroup]
groupValues [] = []
groupValues (declaration : rest) =
  case functionBinding declaration of
    Just (name, matches, checkedType) ->
      let (same, remaining) = span (sameFunction name) rest
          moreMatches = concatMap (maybe [] middle . functionBinding) same
       in FunctionGroup name (matches <> moreMatches) checkedType : groupValues remaining
    Nothing ->
      case patternBinding declaration of
        Just group -> group : groupValues rest
        Nothing -> groupValues rest

functionBinding :: Syn.Decl -> Maybe (Text, [Syn.Match], Maybe TcType)
functionBinding declaration =
  case Syn.peelDeclAnn declaration of
    Syn.DeclValue (Syn.FunctionBind name matches) -> Just (Syn.unqualifiedNameText name, matches, declarationType declaration)
    _ -> Nothing

sameFunction :: Text -> Syn.Decl -> Bool
sameFunction name declaration = maybe False ((== name) . tripleFirst) (functionBinding declaration)

patternBinding :: Syn.Decl -> Maybe ValueGroup
patternBinding declaration =
  case Syn.peelDeclAnn declaration of
    Syn.DeclValue (Syn.PatternBind _ pattern' rhs) -> PatternGroup <$> barePatternName pattern' <*> pure rhs <*> pure (declarationType declaration)
    _ -> Nothing

declarationType :: Syn.Decl -> Maybe TcType
declarationType declaration =
  case declaration of
    Syn.DeclAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> declarationType inner
    _ -> Nothing

tripleFirst :: (a, b, c) -> a
tripleFirst (value, _, _) = value

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
  ty <- lookupCheckedType name
  moduleOrigin <- gets vsModuleOrigin
  pure (TopValue (topName moduleOrigin name) ty group)

groupName :: ValueGroup -> Text
groupName group =
  case group of
    FunctionGroup name _ _ -> name
    PatternGroup name _ _ -> name

groupType :: ValueGroup -> Maybe TcType
groupType group =
  case group of
    FunctionGroup _ _ ty -> ty
    PatternGroup _ _ ty -> ty

desugarTopValue :: TopValue -> ValueM ValDecl
desugarTopValue top = do
  body <-
    case topGroup top of
      FunctionGroup _ matches _ -> desugarMatches (topType top) matches
      PatternGroup _ rhs _ -> desugarMatches (topType top) [emptyMatch rhs]
  ty <- convertCheckedType (topType top)
  pure
    ValDecl
      { valVis = Pub,
        valName = topCoreName top,
        valType = ty,
        valBody = body
      }

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
          (argumentTypes, _) = peelFunctions argumentCount bodyType
      typeBinders <- mapM convertTypeBinder typeVariables
      dictionaries <- zipWithM (freshDictionaryBinder "$d") [0 :: Int ..] predicates
      arguments <- zipWithM freshArgument [0 :: Int ..] argumentTypes
      body <- withDictionaries (zipWith predicateDictionary predicates dictionaries) (desugarMatchArguments arguments matches)
      pure (foldr ExTyLam (foldr ExLam (foldr ExLam body arguments) dictionaries) typeBinders)

desugarMatchArguments :: [Binder] -> [Syn.Match] -> ValueM Expr
desugarMatchArguments [] (match : _) = desugarRhs (Syn.matchRhs match)
desugarMatchArguments [] [] = failValue "pattern match has no result"
desugarMatchArguments (argument : arguments) matches
  | all firstPatternIsVariable matches = do
      let locals = mapMaybe (firstPatternBinding argument) matches
      withLocals locals (desugarMatchArguments arguments (map dropFirstPattern matches))
  | otherwise = do
      caseBinder <- freshBinderFromType "_scrut" (binderType argument)
      alternatives <- mapM (desugarPatternGroup arguments) (groupPatterns matches)
      pure (ExCase (ExVar (binderName argument)) caseBinder alternatives)

desugarPatternGroup :: [Binder] -> (Syn.Pattern, [Syn.Match]) -> ValueM Alt
desugarPatternGroup remaining (pattern', matches) = do
  constructor <- patternConstructor pattern'
  let subpatterns = patternChildren pattern'
  fieldTypes <- mapM requiredPatternType subpatterns
  fields <- zipWithM freshPatternBinder subpatterns fieldTypes
  let localBindings = concat (zipWith patternLocalBindings subpatterns fields)
      expanded = map (expandFirstPattern subpatterns) matches
  body <- withLocals localBindings (desugarMatchArguments (fields <> remaining) expanded)
  pure (Alt constructor fields body)

groupPatterns :: [Syn.Match] -> [(Syn.Pattern, [Syn.Match])]
groupPatterns = List.foldl' insert []
  where
    insert groups match =
      case Syn.matchPats match of
        [] -> groups
        pattern' : _ -> insertPattern pattern' match groups
    insertPattern pattern' match [] = [(pattern', [match])]
    insertPattern pattern' match ((representative, matches) : rest)
      | patternKey pattern' == patternKey representative = (representative, matches <> [match]) : rest
      | otherwise = (representative, matches) : insertPattern pattern' match rest

patternKey :: Syn.Pattern -> Text
patternKey pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> Syn.nameText name
    Syn.PInfix _ name _ -> Syn.nameText name
    Syn.PList [] -> "[]"
    Syn.PList (_ : _) -> ":"
    Syn.PTuple _ fields -> "(" <> T.replicate (max 0 (length fields - 1)) "," <> ")"
    Syn.PLit literal -> T.pack (show (Syn.peelLiteralAnn literal))
    _ -> "_"

firstPatternIsVariable :: Syn.Match -> Bool
firstPatternIsVariable match =
  case Syn.matchPats match of
    pattern' : _ ->
      case peelPattern pattern' of
        Syn.PVar {} -> True
        Syn.PWildcard -> True
        _ -> False
    [] -> False

firstPatternBinding :: Binder -> Syn.Match -> Maybe (Text, (Binder, TcType))
firstPatternBinding binder match =
  case Syn.matchPats match of
    pattern' : _ ->
      case peelPattern pattern' of
        Syn.PVar name -> Just (Syn.unqualifiedNameText name, (binder, fromBinderType pattern'))
        _ -> Nothing
    [] -> Nothing

dropFirstPattern :: Syn.Match -> Syn.Match
dropFirstPattern match = match {Syn.matchPats = drop 1 (Syn.matchPats match)}

expandFirstPattern :: [Syn.Pattern] -> Syn.Match -> Syn.Match
expandFirstPattern children match = match {Syn.matchPats = children <> drop 1 (Syn.matchPats match)}

patternChildren :: Syn.Pattern -> [Syn.Pattern]
patternChildren pattern' =
  case peelPattern pattern' of
    Syn.PCon _ _ children -> children
    Syn.PInfix left _ right -> [left, right]
    Syn.PList [] -> []
    Syn.PList (item : items) -> [item, Syn.PList items]
    Syn.PTuple _ children -> children
    _ -> []

patternConstructor :: Syn.Pattern -> ValueM AltCon
patternConstructor pattern' =
  case peelPattern pattern' of
    Syn.PCon name _ _ -> AltData <$> resolvedTermName name
    Syn.PInfix _ name _ -> AltData <$> resolvedTermName name
    Syn.PList [] -> pure (AltData (primitiveName "GHC.Types" "[]" SortDataConstructor))
    Syn.PList (_ : _) -> pure (AltData (primitiveName "GHC.Types" ":" SortDataConstructor))
    Syn.PTuple _ fields -> pure (AltData (primitiveName "GHC.Types" ("(" <> T.replicate (max 0 (length fields - 1)) "," <> ")") SortDataConstructor))
    Syn.PLit literal -> AltLit <$> patternLiteral literal
    Syn.PWildcard -> pure AltDefault
    Syn.PVar {} -> pure AltDefault
    unsupported -> failValue ("unsupported System FC 2 pattern: " <> take 80 (show unsupported))

patternLiteral :: Syn.Literal -> ValueM Literal
patternLiteral literal =
  case Syn.peelLiteralAnn literal of
    Syn.LitInt value numericType _ -> LitInt <$> convertRuntimeRep (numericRepresentation numericType) <*> pure value
    Syn.LitChar value _ -> LitChar <$> convertRuntimeRep WordRep <*> pure value
    Syn.LitCharHash value _ -> LitChar <$> convertRuntimeRep WordRep <*> pure value
    unsupported -> failValue ("unsupported System FC 2 pattern literal: " <> show unsupported)

freshPatternBinder :: Syn.Pattern -> TcType -> ValueM Binder
freshPatternBinder pattern' = freshBinder (fromMaybe "_pat" (barePatternName pattern'))

patternLocalBindings :: Syn.Pattern -> Binder -> [(Text, (Binder, TcType))]
patternLocalBindings pattern' binder =
  case peelPattern pattern' of
    Syn.PVar name -> [(Syn.unqualifiedNameText name, (binder, fromBinderType pattern'))]
    Syn.PAs name _ -> [(Syn.unqualifiedNameText name, (binder, fromBinderType pattern'))]
    _ -> []

peelPattern :: Syn.Pattern -> Syn.Pattern
peelPattern pattern' =
  case pattern' of
    Syn.PAnn _ inner -> peelPattern inner
    Syn.PParen inner -> peelPattern inner
    Syn.PStrict inner -> peelPattern inner
    Syn.PIrrefutable inner -> peelPattern inner
    Syn.PTypeSig inner _ -> peelPattern inner
    _ -> pattern'

requiredPatternType :: Syn.Pattern -> ValueM TcType
requiredPatternType pattern' =
  case patternType pattern' of
    Just ty -> pure ty
    Nothing -> failValue ("missing checked pattern type: " <> take 80 (show pattern'))

patternType :: Syn.Pattern -> Maybe TcType
patternType pattern' =
  case pattern' of
    Syn.PVar name -> nameTcType name
    Syn.PAnn annotation inner -> (tcAnnType <$> Syn.fromAnnotation annotation) <|> patternType inner
    Syn.PParen inner -> patternType inner
    Syn.PStrict inner -> patternType inner
    Syn.PIrrefutable inner -> patternType inner
    Syn.PAs name inner -> nameTcType name <|> patternType inner
    Syn.PTypeSig inner _ -> patternType inner
    _ -> Nothing

fromBinderType :: Syn.Pattern -> TcType
fromBinderType pattern' = fromMaybe (TcBuiltinTyCon "Type" 0 []) (patternType pattern')

nameTcType :: Syn.UnqualifiedName -> Maybe TcType
nameTcType name =
  tcAnnType <$> listToMaybe (mapMaybe Syn.fromAnnotation (Syn.unqualifiedNameAnns name))

desugarRhs :: Syn.Rhs Syn.Expr -> ValueM Expr
desugarRhs rhs =
  case rhs of
    Syn.UnguardedRhs _ expression Nothing -> desugarExpr expression
    Syn.UnguardedRhs _ expression (Just declarations) -> desugarLocalDecls declarations (desugarExpr expression)
    Syn.GuardedRhss {} -> failValue "guarded right-hand side remains after type checking"

desugarExpr :: Syn.Expr -> ValueM Expr
desugarExpr expression =
  case expression of
    Syn.EAnn annotation inner
      | Just tcAnnotation <- Syn.fromAnnotation annotation -> desugarAnnotatedExpr tcAnnotation inner
      | otherwise -> desugarExpr inner
    Syn.EVar name -> ExVar <$> occurrenceName name
    Syn.EApp function argument -> desugarApplication function argument
    Syn.EInfix left operator right -> do
      operator' <- occurrenceName operator
      (ExApp . ExApp (ExVar operator') <$> desugarExpr left) <*> desugarExpr right
    Syn.EParen inner -> desugarExpr inner
    Syn.ETypeSig inner _ -> desugarExpr inner
    Syn.ETypeApp function _ -> desugarExpr function
    Syn.ELambdaPats patterns body -> desugarLambda patterns body
    Syn.ECase scrutinee alternatives -> desugarCase scrutinee alternatives
    Syn.ELetDecls declarations body -> desugarLocalDecls declarations (desugarExpr body)
    unsupported -> failValue ("unsupported System FC 2 expression: " <> take 80 (show unsupported))

desugarAnnotatedExpr :: TcAnnotation -> Syn.Expr -> ValueM Expr
desugarAnnotatedExpr annotation inner = do
  body <-
    case inner of
      Syn.EVar name -> do
        variable <- occurrenceName name
        types <- mapM convertCheckedType (tcAnnTypeArgs annotation)
        evidence <- mapM desugarEvidence (tcAnnEvidenceTerms annotation)
        pure (foldl ExApp (foldl ExTyApp (ExVar variable) types) evidence)
      Syn.EInt value numericType _
        | numericType /= Syn.TInteger -> do
            representation <- convertRuntimeRep (numericRepresentation numericType)
            pure (ExLit (LitInt representation value))
      Syn.ECharHash value _ -> do
        representation <- convertRuntimeRep WordRep
        pure (ExLit (LitChar representation value))
      Syn.EString value _ -> pure (ExLit (LitString value))
      Syn.EStringHash value _ -> do
        representation <- convertRuntimeRep AddrRep
        pure (ExLit (LitAddr representation (BS.pack (map (fromIntegral . fromEnum) (T.unpack value)))))
      _ -> desugarExpr inner
  typeBinders <- mapM convertTypeBinder (tcAnnTypeBinders annotation)
  pure (foldr ExTyLam body typeBinders)

desugarApplication :: Syn.Expr -> Syn.Expr -> ValueM Expr
desugarApplication function argument = do
  maybeNewtype <- newtypeApplication function
  argument' <- desugarExpr argument
  case maybeNewtype of
    Just (dataType, typeArguments) -> do
      convertedArguments <- mapM convertCheckedType typeArguments
      let tyCon = dtiTyCon dataType
          axiom = Name ("$ax$" <> dtiName dataType) SortAxiom (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))
      pure (ExCast argument' (CoSym (CoAxiom axiom convertedArguments)))
    Nothing -> ExApp <$> desugarExpr function <*> pure argument'

newtypeApplication :: Syn.Expr -> ValueM (Maybe (DataTypeInfo, [TcType]))
newtypeApplication expression = do
  newtypes <- gets vsNewtypeConstructors
  pure $ do
    (name, annotation) <- annotatedHead expression
    key <- resolvedTermKey name
    dataType <- Map.lookup key newtypes
    pure (dataType, tcAnnTypeArgs annotation)

annotatedHead :: Syn.Expr -> Maybe (Syn.Name, TcAnnotation)
annotatedHead = go Nothing
  where
    go maybeAnnotation expression =
      case expression of
        Syn.EAnn annotation inner -> go ((Syn.fromAnnotation annotation :: Maybe TcAnnotation) <|> maybeAnnotation) inner
        Syn.EParen inner -> go maybeAnnotation inner
        Syn.ETypeApp inner _ -> go maybeAnnotation inner
        Syn.EVar name -> (,fromMaybe emptyTcAnnotation maybeAnnotation) <$> Just name
        _ -> Nothing
    emptyTcAnnotation = TcAnnotation (TcBuiltinTyCon "Type" 0 []) [] [] [] []

desugarLambda :: [Syn.Pattern] -> Syn.Expr -> ValueM Expr
desugarLambda patterns body = do
  types <- mapM requiredPatternType patterns
  binders <- zipWithM freshPatternBinder patterns types
  let locals = concat (zipWith patternLocalBindings patterns binders)
  body' <- withLocals locals (desugarExpr body)
  pure (foldr ExLam body' binders)

desugarCase :: Syn.Expr -> [Syn.CaseAlt Syn.Expr] -> ValueM Expr
desugarCase scrutinee alternatives = do
  scrutinee' <- desugarExpr scrutinee
  scrutineeType <- requiredExprType scrutinee
  caseBinder <- freshBinder "_case" scrutineeType
  alts <- mapM (desugarCaseAlt caseBinder scrutineeType) alternatives
  pure (ExCase scrutinee' caseBinder alts)

desugarCaseAlt :: Binder -> TcType -> Syn.CaseAlt Syn.Expr -> ValueM Alt
desugarCaseAlt caseBinder scrutineeType alternative =
  case alternative of
    Syn.CaseAlt _ pattern' rhs -> do
      constructor <- patternConstructor pattern'
      let children = patternChildren pattern'
      types <- mapM requiredPatternType children
      binders <- zipWithM freshPatternBinder children types
      let rootBindings = patternRootBindings pattern' caseBinder scrutineeType
          childBindings = concat (zipWith patternLocalBindings children binders)
      body <- withLocals (rootBindings <> childBindings) (desugarRhs rhs)
      pure (Alt constructor binders body)

patternRootBindings :: Syn.Pattern -> Binder -> TcType -> [(Text, (Binder, TcType))]
patternRootBindings pattern' binder ty =
  case peelPattern pattern' of
    Syn.PVar name -> [(Syn.unqualifiedNameText name, (binder, ty))]
    Syn.PAs name _ -> [(Syn.unqualifiedNameText name, (binder, ty))]
    _ -> []

desugarLocalDecls :: [Syn.Decl] -> ValueM Expr -> ValueM Expr
desugarLocalDecls declarations body = do
  let groups = groupValues declarations
  allocated <- mapM allocateLocal groups
  withLocals [(name, (binder, ty)) | (name, binder, ty, _) <- allocated] $ do
    binds <- mapM desugarLocal allocated
    ExRec binds <$> body
  where
    allocateLocal group = do
      let name = groupName group
      ty <- maybe (lookupCheckedType name) pure (groupType group)
      binder <- freshBinder name ty
      pure (name, binder, ty, group)
    desugarLocal (_, binder, ty, group) = do
      rhs <-
        case group of
          FunctionGroup _ matches _ -> desugarMatches ty matches
          PatternGroup _ sourceRhs _ -> desugarMatches ty [emptyMatch sourceRhs]
      pure (Bind binder rhs)

desugarEvidence :: Ev.EvTerm -> ValueM Expr
desugarEvidence evidence =
  case evidence of
    Ev.EvGiven predicate -> do
      dictionaries <- gets vsDictionaries
      case Map.lookup (predicateKey predicate) dictionaries of
        Just binder -> pure (ExVar (binderName binder))
        Nothing -> failValue ("missing given dictionary for " <> show predicate)
    Ev.EvDict origin dictionaryName types subEvidence -> do
      convertedTypes <- mapM convertCheckedType types
      evidenceArguments <- mapM desugarEvidence subEvidence
      moduleOrigin <- gets vsModuleOrigin
      let (package, moduleName') =
            case origin of
              Just (packageName, originModule) -> (PackageId packageName, originModule)
              Nothing -> moduleOrigin
          name = Name dictionaryName SortValue (OriginTop package moduleName')
      pure (foldl ExApp (foldl ExTyApp (ExVar name) convertedTypes) evidenceArguments)
    Ev.EvCoercion coercion -> ExCast (ExVar (Name "coercion" SortValue (OriginLocal (Unique 0)))) <$> convertCoercion coercion
    Ev.EvCast inner coercion -> ExCast <$> desugarEvidence inner <*> convertCoercion coercion
    unsupported -> failValue ("unsupported System FC 2 evidence: " <> take 80 (show unsupported))

convertCoercion :: Ev.Coercion -> ValueM Coercion
convertCoercion coercion =
  case coercion of
    Ev.CoVar (Ev.EvVar unique) -> pure (CoVar (Name "c" SortValue (OriginLocal unique)))
    Ev.Refl ty -> CoRefl <$> convertCheckedType ty
    Ev.Sym inner -> CoSym <$> convertCoercion inner
    Ev.Trans left right -> CoTrans <$> convertCoercion left <*> convertCoercion right
    Ev.TyConAppCo tyCon arguments -> do
      env <- gets vsConvertEnv
      CoTyConApp (tyConNameFc2 env tyCon) <$> mapM convertCoercion arguments
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
        ResolvedBuiltin name -> pure (Name name SortValue (OriginLocal (Unique 0)))
        ResolvedLocal _ localName -> do
          local <- Map.lookup (Syn.unqualifiedNameText localName) <$> gets vsLocals
          case local of
            Just (binder, _) -> pure (binderName binder)
            Nothing -> failValue ("missing local value " <> T.unpack (Syn.unqualifiedNameText localName))
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

primitiveName :: Text -> Text -> Sort -> Name
primitiveName moduleName' name sort = Name name sort (OriginTop (PackageId "aihc-prim") moduleName')

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
    Nothing -> failValue ("missing checked type for " <> T.unpack name)

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
      operatorType <- lookupCheckedType (Syn.nameText operator)
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
  liftEither (convertType env ty)

convertTypeBinder :: TyVarId -> ValueM Binder
convertTypeBinder tyVar = do
  env <- gets vsConvertEnv
  liftEither (tyVarBinder env tyVar)

convertRuntimeRep :: RuntimeRep -> ValueM Type
convertRuntimeRep runtimeRep = do
  env <- gets vsConvertEnv
  liftEither (convertRep env runtimeRep)

numericRepresentation :: Syn.NumericType -> RuntimeRep
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

predicateDictionary :: Pred -> Binder -> Dictionary
predicateDictionary = Dictionary

withLocals :: [(Text, (Binder, TcType))] -> ValueM a -> ValueM a
withLocals additions action = do
  previous <- gets vsLocals
  modify' (\state -> state {vsLocals = foldr (uncurry Map.insert) previous additions})
  result <- action
  modify' (\state -> state {vsLocals = previous})
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
