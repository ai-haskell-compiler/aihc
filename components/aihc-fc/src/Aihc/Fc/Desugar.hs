{-# LANGUAGE OverloadedStrings #-}

-- | Desugaring from type-checked surface AST to System FC Core.
--
-- The entry point 'desugarModule' takes a parsed module, runs the type
-- checker, and produces an 'FcProgram'.
module Aihc.Fc.Desugar
  ( -- * Entry point
    desugarModule,
    desugarModuleWithBindings,
    desugarModuleWithTcResult,
    DesugarResult (..),
  )
where

import Aihc.Fc.Desugar.Expr (ClassDict (..), DsM, DsState (..), desugarBug, dsEvidence, dsMatches, dsMatchesWithEnclosingDicts, freshUnique, freshVar, lookupType, withDicts)
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Optimize (optimizeProgram)
import Aihc.Fc.Subst (freeRigidTyVars, substType)
import Aihc.Fc.Syntax
import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseSignatureType)
import Aihc.Parser.Syntax
  ( ArrowKind (..),
    CallConv (..),
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl,
    DataDecl (..),
    Decl (..),
    Expr,
    Extension (..),
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NewtypeDecl (..),
    Pattern (..),
    Rhs,
    TupleFlavor (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    nameText,
    peelDeclAnn,
    peelTypeHead,
    unqualifiedNameText,
  )
import Aihc.Resolve (ResolveResult (..), resolve)
import Aihc.Tc (TcBindingResult (..), renderTcSignature, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModule)
import Aihc.Tc.Annotations (TcAnnotation (..), TcClassAnnotation (..), TcClassMethodAnnotation (..), TcDictBinderAnnotation (..), TcForeignAbiType (..), TcForeignEffect (..), TcForeignImportAnnotation (..), TcForeignMarshal (..), TcInstanceAnnotation (..), TcInstanceMethodAnnotation (..))
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, zipWithM)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | Result of desugaring.
data DesugarResult = DesugarResult
  { dsProgram :: !FcProgram,
    dsSuccess :: !Bool,
    dsErrors :: ![String]
  }
  deriving (Show)

-- | Desugar a module: parse, typecheck, then translate to Core.
desugarModule :: Module -> DesugarResult
desugarModule m =
  case resolve [m] of
    ResolveResult {resolvedModules = [resolved], resolveErrors = []} ->
      desugarModuleWithTcResult (typecheckModule resolved) resolved
    ResolveResult {resolveErrors} ->
      DesugarResult
        { dsProgram = FcProgram [],
          dsSuccess = False,
          dsErrors = ["resolve error: " <> show resolveErrors]
        }

-- | Desugar a module using a type-checking result already computed by
-- the caller. This is useful for clients such as the REPL that preload
-- imported bindings into the type-checker environment.
desugarModuleWithTcResult :: Module -> Module -> DesugarResult
desugarModuleWithTcResult tcResult =
  desugarModuleWithBindings (tcModuleBindings tcResult) tcResult

desugarModuleWithBindings :: [TcBindingResult] -> Module -> Module -> DesugarResult
desugarModuleWithBindings bindings tcResult _m =
  if not (tcModuleSuccess tcResult)
    then
      DesugarResult
        { dsProgram = FcProgram [],
          dsSuccess = False,
          dsErrors = showTcFailure tcResult
        }
    else
      let typeEnv = Map.fromList (builtinTypeEntries <> concatMap bindingTypeEntries bindings)
       in case runStateT (dsModule tcResult) (DsState 1000 typeEnv Map.empty Map.empty) of
            Left err ->
              DesugarResult
                { dsProgram = FcProgram [],
                  dsSuccess = False,
                  dsErrors = [err]
                }
            Right (binds, _) ->
              DesugarResult
                { dsProgram = optimizeProgram (lowerConstraintProgram (lowerNewtypes (FcProgram binds))),
                  dsSuccess = True,
                  dsErrors = []
                }

-- | Type-class evidence is ordinary term-level data in FC. Replace qualified
-- source types with explicit dictionary arrows after desugaring has consumed
-- their predicate structure.
lowerConstraintProgram :: FcProgram -> FcProgram
lowerConstraintProgram (FcProgram topBinds) =
  FcProgram (map lowerTopBind topBinds)
  where
    lowerTopBind topBind =
      case topBind of
        FcData name tyVars constructors ->
          FcData name tyVars [(constructor, map lowerConstraintType fields) | (constructor, fields) <- constructors]
        FcAxiom declaration ->
          FcAxiom
            declaration
              { fcAxiomLeft = lowerConstraintType (fcAxiomLeft declaration),
                fcAxiomRight = lowerConstraintType (fcAxiomRight declaration)
              }
        FcNewtype declaration ->
          FcNewtype
            declaration
              { fcNewtypeRepresentation = lowerConstraintType (fcNewtypeRepresentation declaration),
                fcNewtypeResult = lowerConstraintType (fcNewtypeResult declaration)
              }
        FcPrimitive var arity -> FcPrimitive (lowerVar var) arity
        FcForeignImport foreignCall -> FcForeignImport foreignCall
        FcTopBind bind -> FcTopBind (lowerBind bind)

    lowerBind bind =
      case bind of
        FcNonRec var expression -> FcNonRec (lowerVar var) (lowerExpr expression)
        FcRec bindings -> FcRec [(lowerVar var, lowerExpr expression) | (var, expression) <- bindings]

    lowerExpr expression =
      case expression of
        FcVar var -> FcVar (lowerVar var)
        FcLit {} -> expression
        FcApp function argument -> FcApp (lowerExpr function) (lowerExpr argument)
        FcTyApp function ty -> FcTyApp (lowerExpr function) (lowerConstraintType ty)
        FcLam var body -> FcLam (lowerVar var) (lowerExpr body)
        FcTyLam tyVar body -> FcTyLam tyVar (lowerExpr body)
        FcLet bind body -> FcLet (lowerBind bind) (lowerExpr body)
        FcCase scrutinee binder alternatives ->
          FcCase (lowerExpr scrutinee) (lowerVar binder) (map lowerAlt alternatives)
        FcCast inner coercion -> FcCast (lowerExpr inner) (lowerCoercion coercion)
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map lowerExpr arguments)

    lowerAlt alternative =
      alternative
        { altBinders = map lowerVar (altBinders alternative),
          altRhs = lowerExpr (altRhs alternative)
        }

    lowerVar var = var {varType = lowerConstraintType (varType var)}

lowerConstraintType :: TcType -> TcType
lowerConstraintType ty =
  case ty of
    TcTyVar {} -> ty
    TcMetaTv {} -> ty
    TcTyCon tyCon arguments -> TcTyCon tyCon (map lowerConstraintType arguments)
    TcFunTy argument result -> TcFunTy (lowerConstraintType argument) (lowerConstraintType result)
    TcForAllTy tyVar body -> TcForAllTy tyVar (lowerConstraintType body)
    TcQualTy predicates body ->
      foldr (TcFunTy . lowerConstraintType . predType) (lowerConstraintType body) predicates
    TcAppTy function argument -> TcAppTy (lowerConstraintType function) (lowerConstraintType argument)

lowerCoercion :: Coercion -> Coercion
lowerCoercion coercion =
  case coercion of
    CoVar {} -> coercion
    Refl ty -> Refl (lowerConstraintType ty)
    Sym inner -> Sym (lowerCoercion inner)
    Trans left right -> Trans (lowerCoercion left) (lowerCoercion right)
    TyConAppCo tyCon coercions -> TyConAppCo tyCon (map lowerCoercion coercions)
    AxiomInstCo name types -> AxiomInstCo name (map lowerConstraintType types)

-- | Format a binding result for error messages.
showBinding :: TcBindingResult -> String
showBinding b = renderTcSignature (tbDisplayName b) (tbType b)

showTcFailure :: Module -> [String]
showTcFailure tcResult =
  case map show (tcModuleDiagnostics tcResult) of
    [] -> map showBinding (tcModuleBindings tcResult)
    diagnostics -> diagnostics

bindingTypeEntries :: TcBindingResult -> [(Text, TcType)]
bindingTypeEntries b =
  [(tbName b, tbType b)]

builtinTypeEntries :: [(Text, TcType)]
builtinTypeEntries =
  [ (":", TcForAllTy aVar (TcFunTy aTy (TcFunTy listA listA))),
    ("[]", TcForAllTy aVar listA)
  ]
  where
    aVar = TyVarId "a" (Unique (-1000))
    aTy = TcTyVar aVar
    listA = TcTyCon (TyCon "[]" 1) [aTy]

-- | Desugar a module's declarations.
dsModule :: Module -> DsM [FcTopBind]
dsModule m = do
  let decls = moduleDecls m
  -- Phase 1: data declarations and class method selectors.
  dataTops <- concat <$> mapM dsDecl decls
  -- Phase 2: instance dictionaries.
  instanceTops <- concat <$> mapM dsInstanceDecl (moduleInstances decls)
  -- Phase 3: group and desugar value bindings.
  let grouped = groupFunctionBinds decls
  valueTops <- mapM dsGroup grouped
  pure (dataTops ++ instanceTops ++ valueTops)

-- | Desugar a single declaration (data types only; values handled by groups).
dsDecl :: Decl -> DsM [FcTopBind]
dsDecl (DeclData dd) = (: []) <$> dsDataDeclM dd
dsDecl (DeclNewtype nd) = (: []) <$> dsNewtypeDeclM nd
dsDecl (DeclAnn ann inner)
  | Just foreignAnn <- fromAnnotation ann,
    Just (tcAnn, foreignDecl) <- annotatedForeignDecl inner =
      dsForeignImport tcAnn (Just foreignAnn) foreignDecl
dsDecl (DeclAnn ann (DeclForeign foreignDecl))
  | Just tcAnn <- fromAnnotation ann = dsForeignImport tcAnn Nothing foreignDecl
dsDecl (DeclAnn ann (DeclClass classDecl))
  | Just classAnn <- fromAnnotation ann = dsClassDeclM classDecl classAnn
dsDecl (DeclAnn _ inner) = dsDecl inner
dsDecl DeclClass {} = desugarBug "missing type-checker annotation for class declaration"
dsDecl _ = pure []

-- | Desugar a data declaration.
dsDataDeclM :: DataDecl -> DsM FcTopBind
dsDataDeclM dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
  constructorInfos <- mapM dsDataConM (dataDeclConstructors dd)
  let typeVariables =
        case constructorInfos of
          (_, universals, _) : _ -> universals
          [] -> []
      constructors = [(name, fields) | (name, _, fields) <- constructorInfos]
  pure (FcData tyName typeVariables constructors)

-- | Retain the nominal declaration and its representation type as an FC axiom.
-- 'lowerNewtypes' turns all term-level construction and matching into casts.
dsNewtypeDeclM :: NewtypeDecl -> DsM FcTopBind
dsNewtypeDeclM nd = do
  let tyName = unqualifiedNameText (binderHeadName (newtypeDeclHead nd))
  case newtypeDeclConstructor nd of
    Nothing -> desugarBug ("newtype " <> T.unpack tyName <> " has no constructor")
    Just con -> do
      let (conName, arity) = dsDataConPure con
      conTy <- lookupType conName
      case (arity, dropForAlls conTy) of
        (1, TcFunTy fieldTy resultTy@(TcTyCon resultTyCon resultArgs))
          | tyConName resultTyCon == tyName,
            Just tyVars <- traverse asTyVar resultArgs ->
              pure
                ( FcNewtype
                    FcNewtypeDecl
                      { fcNewtypeName = tyName,
                        fcNewtypeTyVars = tyVars,
                        fcNewtypeConstructor = conName,
                        fcNewtypeRepresentation = fieldTy,
                        fcNewtypeResult = resultTy
                      }
                )
        _ -> desugarBug ("newtype constructor " <> T.unpack conName <> " does not have exactly one field")
  where
    asTyVar (TcTyVar tyVar) = Just tyVar
    asTyVar _ = Nothing

annotatedForeignDecl :: Decl -> Maybe (TcAnnotation, ForeignDecl)
annotatedForeignDecl = go Nothing
  where
    go maybeTc decl =
      case decl of
        DeclAnn ann inner -> go (fromAnnotation ann <|> maybeTc) inner
        DeclForeign foreignDecl -> (,foreignDecl) <$> maybeTc
        _ -> Nothing

dsForeignImport :: TcAnnotation -> Maybe TcForeignImportAnnotation -> ForeignDecl -> DsM [FcTopBind]
dsForeignImport tcAnn foreignPlan foreignDecl
  | foreignDirection foreignDecl /= ForeignImport =
      desugarBug "unsupported foreign export after type checking"
  | otherwise =
      case foreignCallConv foreignDecl of
        CPrim -> (: []) <$> dsForeignPrim tcAnn foreignDecl
        CCall ->
          case foreignPlan of
            Just plan -> dsForeignCcall tcAnn plan foreignDecl
            Nothing -> desugarBug "missing type-checker foreign import plan"
        callConv -> desugarBug ("unsupported foreign calling convention after type checking: " <> show callConv)

dsForeignPrim :: TcAnnotation -> ForeignDecl -> DsM FcTopBind
dsForeignPrim tcAnn foreignDecl = do
  let name = unqualifiedNameText (foreignName foreignDecl)
      ty = tcAnnType tcAnn
  arity <- validatePrimitiveImport name ty
  unique <- freshUnique
  pure (FcPrimitive (Var name unique ty) arity)

dsForeignCcall :: TcAnnotation -> TcForeignImportAnnotation -> ForeignDecl -> DsM [FcTopBind]
dsForeignCcall tcAnn foreignPlan foreignDecl = do
  if foreignSafety foreignDecl == Just Unsafe
    then pure ()
    else desugarBug "only unsafe foreign imports are supported"
  symbol <-
    case foreignEntity foreignDecl of
      ForeignEntityNamed name -> pure name
      ForeignEntityStatic (Just name) -> pure name
      ForeignEntityOmitted -> pure (unqualifiedNameText (foreignName foreignDecl))
      _ -> desugarBug "only statically named foreign imports are supported"
  let name = unqualifiedNameText (foreignName foreignDecl)
      wrapperType = tcAnnType tcAnn
      signature =
        FcForeignSignature
          { fcForeignArgumentTypes = map (lowerForeignAbiType . tcForeignAbiType) (tcForeignArguments foreignPlan),
            fcForeignResultType = lowerForeignAbiType (tcForeignAbiType (tcForeignResult foreignPlan)),
            fcForeignEffect = lowerForeignEffect (tcForeignEffect foreignPlan)
          }
      foreignCall =
        FcForeignCall
          { fcForeignCallName = "$ffi$" <> name,
            fcForeignCallSymbol = symbol,
            fcForeignCallSignature = signature
          }
  wrapperVar <- freshVar name wrapperType
  argumentVars <-
    mapM
      (\(index, marshal) -> freshVar ("$ffi_arg_" <> T.pack (show index)) (tcForeignSourceType marshal))
      (zip [0 :: Int ..] (tcForeignArguments foreignPlan))
  wrapperBody <-
    unboxForeignArguments (zip argumentVars (tcForeignArguments foreignPlan)) $ \arguments ->
      case tcForeignEffect foreignPlan of
        TcForeignPure ->
          boxForeignValue (tcForeignResult foreignPlan) (FcCallForeign foreignCall arguments)
        TcForeignRealWorld -> makeForeignIoWrapper foreignCall (tcForeignResult foreignPlan) arguments
  pure
    [ FcForeignImport foreignCall,
      FcTopBind (FcNonRec wrapperVar (foldr FcLam wrapperBody argumentVars))
    ]

lowerForeignAbiType :: TcForeignAbiType -> FcForeignType
lowerForeignAbiType foreignType =
  case foreignType of
    TcForeignInt -> FcForeignInt
    TcForeignInt32 -> FcForeignInt32
    TcForeignWord64 -> FcForeignWord64
    TcForeignAddr -> FcForeignAddr

lowerForeignEffect :: TcForeignEffect -> FcForeignEffect
lowerForeignEffect effect =
  case effect of
    TcForeignPure -> FcForeignPure
    TcForeignRealWorld -> FcForeignRealWorld

unboxForeignArguments :: [(Var, TcForeignMarshal)] -> ([FcExpr] -> DsM FcExpr) -> DsM FcExpr
unboxForeignArguments arguments continuation = go arguments []
  where
    go [] values = continuation (reverse values)
    go ((var, marshal) : rest) values =
      unboxForeignValue (varName var) marshal (FcVar var) $ \value -> go rest (value : values)

unboxForeignValue :: Text -> TcForeignMarshal -> FcExpr -> (FcExpr -> DsM FcExpr) -> DsM FcExpr
unboxForeignValue binderName marshal expression continuation =
  go (tcForeignSourceType marshal) (tcForeignConstructors marshal) expression
  where
    go _ [] value = continuation value
    go valueType (constructor : constructors) value = do
      constructorType <- dropForAlls <$> lookupType constructor
      fieldType <-
        case constructorType of
          TcFunTy field _ -> pure field
          _ -> desugarBug ("foreign marshalling constructor is not unary: " <> T.unpack constructor)
      caseBinder <- freshVar (binderName <> "_case") valueType
      fieldBinder <- freshVar (binderName <> "_field") fieldType
      rhs <- go fieldType constructors (FcVar fieldBinder)
      pure
        ( FcCase
            value
            caseBinder
            [FcAlt (DataAlt constructor) [fieldBinder] rhs]
        )

boxForeignValue :: TcForeignMarshal -> FcExpr -> DsM FcExpr
boxForeignValue marshal =
  applyConstructors (tcForeignSourceType marshal) (tcForeignConstructors marshal)
  where
    applyConstructors _ [] value = pure value
    applyConstructors resultType (constructor : constructors) value = do
      constructorType <- lookupType constructor
      constructorVar <- freshVar constructor constructorType
      (typeArguments, fieldType) <- instantiateUnaryConstructor constructor resultType constructorType
      fieldValue <- applyConstructors fieldType constructors value
      pure (FcApp (foldl FcTyApp (FcVar constructorVar) typeArguments) fieldValue)

instantiateUnaryConstructor :: Text -> TcType -> TcType -> DsM ([TcType], TcType)
instantiateUnaryConstructor constructor expectedResult constructorType = do
  let (typeVariables, body) = collectForAlls constructorType
  case body of
    TcFunTy fieldType resultType ->
      case matchConstructorResult typeVariables resultType expectedResult Map.empty of
        Just substitution
          | Just typeArguments <- traverse (`Map.lookup` substitution) typeVariables ->
              pure (typeArguments, substType substitution fieldType)
        _ ->
          desugarBug
            ( "foreign marshalling constructor result does not match "
                <> show expectedResult
                <> ": "
                <> T.unpack constructor
            )
    _ -> desugarBug ("foreign marshalling constructor is not unary: " <> T.unpack constructor)

matchConstructorResult :: [TyVarId] -> TcType -> TcType -> Map.Map TyVarId TcType -> Maybe (Map.Map TyVarId TcType)
matchConstructorResult quantified patternType actualType substitution =
  case patternType of
    TcTyVar tyVar
      | tyVar `elem` quantified ->
          case Map.lookup tyVar substitution of
            Nothing -> Just (Map.insert tyVar actualType substitution)
            Just existing
              | existing == actualType -> Just substitution
              | otherwise -> Nothing
    TcTyVar tyVar ->
      case actualType of
        TcTyVar actualTyVar | tyVar == actualTyVar -> Just substitution
        _ -> Nothing
    TcMetaTv meta ->
      case actualType of
        TcMetaTv actualMeta | meta == actualMeta -> Just substitution
        _ -> Nothing
    TcTyCon tyCon arguments ->
      case actualType of
        TcTyCon actualTyCon actualArguments
          | tyCon == actualTyCon,
            length arguments == length actualArguments ->
              foldM
                (\current (expectedArgument, actualArgument) -> matchConstructorResult quantified expectedArgument actualArgument current)
                substitution
                (zip arguments actualArguments)
        _ -> Nothing
    TcFunTy argument result ->
      case actualType of
        TcFunTy actualArgument actualResult -> do
          substitution' <- matchConstructorResult quantified argument actualArgument substitution
          matchConstructorResult quantified result actualResult substitution'
        _ -> Nothing
    TcAppTy function argument ->
      case actualType of
        TcAppTy actualFunction actualArgument -> do
          substitution' <- matchConstructorResult quantified function actualFunction substitution
          matchConstructorResult quantified argument actualArgument substitution'
        _ -> Nothing
    TcForAllTy {} -> Nothing
    TcQualTy {} -> Nothing

makeForeignIoWrapper :: FcForeignCall -> TcForeignMarshal -> [FcExpr] -> DsM FcExpr
makeForeignIoWrapper foreignCall resultMarshal arguments = do
  stateVar <- freshVar "$ffi_state" statePrimRealWorldTy
  tupleBinder <- freshVar "$ffi_result" (fcForeignCallResultType (fcForeignCallSignature foreignCall))
  nextStateVar <- freshVar "$ffi_next_state" statePrimRealWorldTy
  rawResultVar <- freshVar "$ffi_raw_result" (tcForeignPrimitiveType resultMarshal)
  boxedResult <- boxForeignValue resultMarshal (FcVar rawResultVar)
  tupleConstructor <-
    freshVar
      "(#,#)"
      (TcFunTy statePrimRealWorldTy (TcFunTy (tcForeignSourceType resultMarshal) (unboxedTupleTy [statePrimRealWorldTy, tcForeignSourceType resultMarshal])))
  ioConstructorType <- lookupType "IO"
  ioConstructor <- freshVar "IO" ioConstructorType
  let resultTuple = FcApp (FcApp (FcVar tupleConstructor) (FcVar nextStateVar)) boxedResult
      call = FcCallForeign foreignCall (arguments <> [FcVar stateVar])
      stateAction =
        FcLam
          stateVar
          ( FcCase
              call
              tupleBinder
              [FcAlt (DataAlt "(#,#)") [nextStateVar, rawResultVar] resultTuple]
          )
  pure (FcApp (FcTyApp (FcVar ioConstructor) (tcForeignSourceType resultMarshal)) stateAction)

validatePrimitiveImport :: Text -> TcType -> DsM Int
validatePrimitiveImport name ty =
  case Map.lookup name primitiveImportSpecs of
    Nothing ->
      desugarBug ("unknown foreign import prim: " <> T.unpack name)
    Just spec
      | primitiveSpecAccepts spec ty -> pure (primitiveSpecArity spec)
      | otherwise ->
          desugarBug
            ( "incorrect type for foreign import prim "
                <> T.unpack name
                <> "; expected "
                <> T.unpack (primitiveSpecSource spec)
            )

data PrimitiveSpec = PrimitiveSpec
  { primitiveSpecSource :: !Text,
    primitiveSpecType :: !PrimitiveType
  }

primitiveSpecArity :: PrimitiveSpec -> Int
primitiveSpecArity = primitiveTypeArity . primitiveSpecType

primitiveSpecAccepts :: PrimitiveSpec -> TcType -> Bool
primitiveSpecAccepts = matchesPrimitiveType . primitiveSpecType

primitiveImportSpecs :: Map.Map Text PrimitiveSpec
primitiveImportSpecs =
  Map.fromList
    [ primitive "+#" "Int# -> Int# -> Int#",
      primitive "-#" "Int# -> Int# -> Int#",
      primitive "*#" "Int# -> Int# -> Int#",
      primitive "addIntC#" "Int# -> Int# -> (# Int#, Int# #)",
      primitive "subIntC#" "Int# -> Int# -> (# Int#, Int# #)",
      primitive "compareInt#" "Int# -> Int# -> Int#",
      primitive "<#" "Int# -> Int# -> Int#",
      primitive "==#" "Int# -> Int# -> Int#",
      primitive "ord#" "Char# -> Int#",
      primitive "chr#" "Int# -> Char#",
      primitive "plusWord#" "Word# -> Word# -> Word#",
      primitive "minusWord#" "Word# -> Word# -> Word#",
      primitive "timesWord#" "Word# -> Word# -> Word#",
      primitive "addWordC#" "Word# -> Word# -> (# Word#, Int# #)",
      primitive "subWordC#" "Word# -> Word# -> (# Word#, Int# #)",
      primitive "timesWord2#" "Word# -> Word# -> (# Word#, Word# #)",
      primitive "quotWord#" "Word# -> Word# -> Word#",
      primitive "remWord#" "Word# -> Word# -> Word#",
      primitive "quotRemWord#" "Word# -> Word# -> (# Word#, Word# #)",
      primitive "quotRemWord2#" "Word# -> Word# -> Word# -> (# Word#, Word# #)",
      primitive "and#" "Word# -> Word# -> Word#",
      primitive "or#" "Word# -> Word# -> Word#",
      primitive "xor#" "Word# -> Word# -> Word#",
      primitive "not#" "Word# -> Word#",
      primitive "uncheckedShiftL#" "Word# -> Int# -> Word#",
      primitive "uncheckedShiftRL#" "Word# -> Int# -> Word#",
      primitive "int2Word#" "Int# -> Word#",
      primitive "word2Int#" "Word# -> Int#",
      primitive "eqWord#" "Word# -> Word# -> Int#",
      primitive "neWord#" "Word# -> Word# -> Int#",
      primitive "ltWord#" "Word# -> Word# -> Int#",
      primitive "leWord#" "Word# -> Word# -> Int#",
      primitive "gtWord#" "Word# -> Word# -> Int#",
      primitive "geWord#" "Word# -> Word# -> Int#",
      primitive "clz#" "Word# -> Word#",
      primitive "ctz#" "Word# -> Word#",
      primitive "popCnt#" "Word# -> Word#",
      primitive "raise#" "a -> b",
      primitive "unsafeCoerce#" "a -> b",
      primitive "realWorld#" "State# RealWorld",
      primitive
        "catch#"
        "(State# RealWorld -> (# State# RealWorld, a #)) -> (b -> State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, a #)",
      primitive
        "fork#"
        "(State# RealWorld -> (# State# RealWorld, a #)) -> State# RealWorld -> (# State# RealWorld, ThreadId# #)",
      primitive "awaitIO#" "Addr# -> State# RealWorld -> State# RealWorld",
      primitive "newMVar#" "State# d -> (# State# d, MVar# d a #)",
      primitive "readMVar#" "MVar# d a -> State# d -> (# State# d, a #)",
      primitive "takeMVar#" "MVar# d a -> State# d -> (# State# d, a #)",
      primitive "putMVar#" "MVar# d a -> a -> State# d -> State# d",
      primitive "newMutVar#" "a -> State# d -> (# State# d, MutVar# d a #)",
      primitive "readMutVar#" "MutVar# d a -> State# d -> (# State# d, a #)",
      primitive "writeMutVar#" "MutVar# d a -> a -> State# d -> State# d",
      primitive "newByteArray#" "Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "newPinnedByteArray#" "Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "newAlignedPinnedByteArray#" "Int# -> Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "isMutableByteArrayPinned#" "MutableByteArray# d -> Int#",
      primitive "isByteArrayPinned#" "ByteArray# -> Int#",
      primitive "byteArrayContents#" "ByteArray# -> Addr#",
      primitive "mutableByteArrayContents#" "MutableByteArray# d -> Addr#",
      primitive "shrinkMutableByteArray#" "MutableByteArray# d -> Int# -> State# d -> State# d",
      primitive "resizeMutableByteArray#" "MutableByteArray# d -> Int# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "unsafeFreezeByteArray#" "MutableByteArray# d -> State# d -> (# State# d, ByteArray# #)",
      primitive "unsafeThawByteArray#" "ByteArray# -> State# d -> (# State# d, MutableByteArray# d #)",
      primitive "sizeofByteArray#" "ByteArray# -> Int#",
      primitive "getSizeofMutableByteArray#" "MutableByteArray# d -> State# d -> (# State# d, Int# #)",
      primitive "copyAddrToByteArray#" "Addr# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d",
      primitive "indexWordArray#" "ByteArray# -> Int# -> Word#",
      primitive "readWordArray#" "MutableByteArray# d -> Int# -> State# d -> (# State# d, Word# #)",
      primitive "writeWordArray#" "MutableByteArray# d -> Int# -> Word# -> State# d -> State# d",
      primitive "copyByteArray#" "ByteArray# -> Int# -> MutableByteArray# d -> Int# -> Int# -> State# d -> State# d",
      primitive "yield#" "State# RealWorld -> State# RealWorld"
    ]

primitive :: Text -> Text -> (Text, PrimitiveSpec)
primitive name source =
  ( name,
    PrimitiveSpec
      { primitiveSpecSource = source,
        primitiveSpecType = parsePrimitiveType source
      }
  )

data PrimitiveType
  = PrimitiveTyVar !Text
  | PrimitiveTyCon !Text ![PrimitiveType]
  | PrimitiveFunTy !PrimitiveType !PrimitiveType
  deriving (Show)

parsePrimitiveType :: Text -> PrimitiveType
parsePrimitiveType source =
  case parseSignatureType primitiveParserConfig source of
    ParseOk ty ->
      case primitiveTypeFromSurface ty of
        Right expected -> expected
        Left err -> invalidPrimitiveType err
    ParseErr errs -> invalidPrimitiveType (show errs)
  where
    invalidPrimitiveType err =
      error ("invalid primitive type specification `" <> T.unpack source <> "`: " <> err)

primitiveParserConfig :: ParserConfig
primitiveParserConfig =
  defaultConfig
    { parserExtensions = [MagicHash, UnboxedTuples]
    }

primitiveTypeFromSurface :: Type -> Either String PrimitiveType
primitiveTypeFromSurface ty =
  case peelTypeHead ty of
    TVar name -> Right (PrimitiveTyVar (unqualifiedNameText name))
    TCon name _ -> Right (PrimitiveTyCon (nameText name) [])
    TApp function argument -> do
      function' <- primitiveTypeFromSurface function
      argument' <- primitiveTypeFromSurface argument
      case function' of
        PrimitiveTyCon name arguments -> Right (PrimitiveTyCon name (arguments <> [argument']))
        _ -> Left ("unsupported type application in " <> show ty)
    TFun ArrowUnrestricted argument result ->
      PrimitiveFunTy <$> primitiveTypeFromSurface argument <*> primitiveTypeFromSurface result
    TTuple Unboxed _ elements ->
      PrimitiveTyCon (unboxedTupleTyConName (length elements)) <$> traverse primitiveTypeFromSurface elements
    unsupported -> Left ("unsupported syntax in " <> show unsupported)

primitiveTypeArity :: PrimitiveType -> Int
primitiveTypeArity (PrimitiveFunTy _ result) = 1 + primitiveTypeArity result
primitiveTypeArity _ = 0

matchesPrimitiveType :: PrimitiveType -> TcType -> Bool
matchesPrimitiveType expected actual =
  case matchPrimitiveType quantified expected body Map.empty of
    Just bindings ->
      let matched = Map.elems bindings
       in length matched == length quantified && all (`elem` matched) quantified
    Nothing -> False
  where
    (quantified, body) = collectForAlls actual

matchPrimitiveType :: [TyVarId] -> PrimitiveType -> TcType -> Map.Map Text TyVarId -> Maybe (Map.Map Text TyVarId)
matchPrimitiveType quantified expected actual bindings =
  case (expected, actual) of
    (PrimitiveTyVar name, TcTyVar actualVar)
      | actualVar `elem` quantified ->
          case Map.lookup name bindings of
            Nothing -> Just (Map.insert name actualVar bindings)
            Just expectedVar
              | expectedVar == actualVar -> Just bindings
            _ -> Nothing
    (PrimitiveTyCon expectedName expectedArguments, TcTyCon actualTyCon actualArguments)
      | expectedName == tyConName actualTyCon,
        length expectedArguments == length actualArguments ->
          foldM
            ( \current (expectedArgument, actualArgument) ->
                matchPrimitiveType quantified expectedArgument actualArgument current
            )
            bindings
            (zip expectedArguments actualArguments)
    (PrimitiveFunTy expectedArgument expectedResult, TcFunTy actualArgument actualResult) -> do
      bindings' <- matchPrimitiveType quantified expectedArgument actualArgument bindings
      matchPrimitiveType quantified expectedResult actualResult bindings'
    _ -> Nothing

statePrimRealWorldTy :: TcType
statePrimRealWorldTy = TcTyCon (TyCon "State#" 1) [realWorldTy]

realWorldTy :: TcType
realWorldTy = TcTyCon (TyCon "RealWorld" 0) []

unboxedTupleTy :: [TcType] -> TcType
unboxedTupleTy tys =
  TcTyCon (TyCon (unboxedTupleTyConName (length tys)) (length tys)) tys

unboxedTupleTyConName :: Int -> Text
unboxedTupleTyConName arity =
  "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"

collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)

dsDataConM :: DataConDecl -> DsM (Text, [TyVarId], [TcType])
dsDataConM con = do
  let (name, arity) = dsDataConPure con
  ty <- lookupType name
  let (quantifiedVariables, qualifiedConstructorTy) = collectForAlls ty
      (predicates, constructorTy) = splitConstructorContext qualifiedConstructorTy
      resultVariables = freeRigidTyVars (constructorResultType constructorTy)
      universalVariables = filter (`elem` resultVariables) quantifiedVariables
  fields <- dataConFieldTypes name arity constructorTy
  pure (name, universalVariables, map predType predicates <> fields)

splitConstructorContext :: TcType -> ([Pred], TcType)
splitConstructorContext (TcQualTy predicates body) = (predicates, body)
splitConstructorContext ty = ([], ty)

constructorResultType :: TcType -> TcType
constructorResultType (TcFunTy _ result) = constructorResultType result
constructorResultType ty = ty

dataConFieldTypes :: Text -> Int -> TcType -> DsM [TcType]
dataConFieldTypes _ 0 _ = pure []
dataConFieldTypes name arity (TcFunTy arg rest) =
  (arg :) <$> dataConFieldTypes name (arity - 1) rest
dataConFieldTypes name arity ty =
  desugarBug ("missing field type information for data constructor " <> T.unpack name <> ": expected " <> show arity <> " more field(s) in " <> show ty)

dsClassDeclM :: ClassDecl -> TcClassAnnotation -> DsM [FcTopBind]
dsClassDeclM classDecl classAnn = do
  let classTyVars = tcClassTyVars classAnn
      superClassFieldTypes = map tcDictBinderType (tcClassSuperClasses classAnn)
  methodFieldTypes <- mapM (classMethodFieldType className classTyVars . tcClassMethodType) methods
  let fieldTypes = superClassFieldTypes <> methodFieldTypes
  selectors <- mapM (dsClassSelector dictionaryConstructor (length superClassFieldTypes) classTyVars fieldTypes) methods
  defaults <- mapM dsClassDefault (classDefaultGroups classDecl)
  let dictionaryDeclaration = FcData className classTyVars [(dictionaryConstructor, fieldTypes)]
  pure (dictionaryDeclaration : selectors <> defaults)
  where
    className = unqualifiedNameText (binderHeadName (classDeclHead classDecl))
    methods = tcClassMethods classAnn
    dictionaryConstructor = fcDictionaryConstructorName className

dsClassSelector :: Text -> Int -> [TyVarId] -> [TcType] -> TcClassMethodAnnotation -> DsM FcTopBind
dsClassSelector dictionaryConstructor superClassCount classTyVars fieldTypes methodAnn = do
  methodUnique <- freshUnique
  dictVars <- zipWithM mkSelectorDict [0 :: Int ..] dictPreds
  classDictionaryVar <-
    case dictVars of
      dictVar : _ -> pure dictVar
      [] -> freshVar "$d" (tcClassMethodDictType methodAnn)
  caseBinder <- freshVar "$dict" (varType classDictionaryVar)
  fieldBinders <- zipWithM (\index -> freshVar ("$method" <> T.pack (show index))) [0 :: Int ..] fieldTypes
  selectedField <-
    case drop (superClassCount + tcClassMethodIndex methodAnn) fieldBinders of
      selected : _ -> pure selected
      [] -> desugarBug ("invalid class method index for " <> T.unpack (tcClassMethodName methodAnn))
  let extraTyVars = filter (`notElem` classTyVars) (tcClassMethodTyVars methodAnn)
      extraDictVars = drop 1 dictVars
      selected =
        foldl
          FcApp
          (foldl FcTyApp (FcVar selectedField) (map TcTyVar extraTyVars))
          (map FcVar extraDictVars)
      selection = FcCase (FcVar classDictionaryVar) caseBinder [FcAlt (DataAlt dictionaryConstructor) fieldBinders selected]
      methodVar = Var (tcClassMethodName methodAnn) methodUnique (tcClassMethodType methodAnn)
      body = foldr FcTyLam (foldr FcLam selection dictVars) (tcClassMethodTyVars methodAnn)
  pure (FcTopBind (FcNonRec methodVar body))
  where
    (_tyVars, afterForAlls) = peelForAlls (tcClassMethodType methodAnn)
    (dictPreds, _bodyTy) = peelQuals afterForAlls
    mkSelectorDict ix pred' =
      freshVar ("$d" <> T.pack (show ix)) (predType pred')

dsClassDefault :: (TcInstanceMethodAnnotation, [Match]) -> DsM FcTopBind
dsClassDefault (methodAnn, matches) = do
  let methodName = tcInstanceMethodName methodAnn
      methodType = tcInstanceMethodType methodAnn
      workerName = defaultMethodName methodName
  worker <- freshVar workerName methodType
  body <- dsMatches methodType matches
  pure (FcTopBind (FcNonRec worker body))

classDefaultGroups :: ClassDecl -> [(TcInstanceMethodAnnotation, [Match])]
classDefaultGroups classDecl = concatMap classDefaultGroup (classDeclItems classDecl)

classDefaultGroup :: ClassDeclItem -> [(TcInstanceMethodAnnotation, [Match])]
classDefaultGroup item =
  case item of
    ClassItemAnn ann inner
      | Just methodAnn <- fromAnnotation ann -> classDefaultWithAnnotation methodAnn inner
      | otherwise -> classDefaultGroup inner
    _ -> []

classDefaultWithAnnotation :: TcInstanceMethodAnnotation -> ClassDeclItem -> [(TcInstanceMethodAnnotation, [Match])]
classDefaultWithAnnotation methodAnn item =
  case item of
    ClassItemAnn _ inner -> classDefaultWithAnnotation methodAnn inner
    ClassItemDefault (FunctionBind _ matches) -> [(methodAnn, matches)]
    ClassItemDefault (PatternBind _ _ rhs) -> [(methodAnn, [zeroArgumentMatch rhs])]
    _ -> []

zeroArgumentMatch :: Rhs Expr -> Match
zeroArgumentMatch rhs =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }

defaultMethodName :: Text -> Text
defaultMethodName methodName = "$dm" <> methodName

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tv rest) =
  let (tvs, inner) = peelForAlls rest
   in (tv : tvs, inner)
peelForAlls ty = ([], ty)

peelQuals :: TcType -> ([Pred], TcType)
peelQuals (TcQualTy preds body) = (preds, body)
peelQuals ty = ([], ty)

predType :: Pred -> TcType
predType (ClassPred className args) = TcTyCon (TyCon className (length args)) args
predType (EqPred left right) = TcTyCon (TyCon "~" 2) [left, right]

dsInstanceDecl :: Decl -> DsM [FcTopBind]
dsInstanceDecl decl =
  case decl of
    DeclAnn ann (DeclInstance instanceDecl)
      | Just instAnn <- fromAnnotation ann -> (: []) <$> dsInstanceDict instAnn instanceDecl
    DeclAnn _ inner -> dsInstanceDecl inner
    DeclInstance {} -> desugarBug "missing type-checker annotation for instance declaration"
    _ -> pure []

dsInstanceDict :: TcInstanceAnnotation -> InstanceDecl -> DsM FcTopBind
dsInstanceDict instAnn instanceDecl = do
  let methods = Map.fromListWith combineMethods (instanceMethodGroups instanceDecl)
  contextDicts <- zipWithM mkContextDict [0 :: Int ..] (tcInstanceContextDicts instAnn)
  className <-
    case dictionaryClassName (tcInstanceDictType instAnn) of
      Just name -> pure name
      Nothing -> desugarBug ("cannot determine class for instance dictionary " <> T.unpack (tcInstanceDictName instAnn))
  let methodOrder = tcInstanceMethodOrder instAnn
      usesDefaultMethod =
        any
          (\methodName -> Map.notMember methodName methods && methodName `elem` tcInstanceDefaultMethods instAnn)
          methodOrder
      selfDictionary dictVar =
        foldl
          FcApp
          (foldl FcTyApp (FcVar dictVar) (map TcTyVar (tcInstanceTyVars instAnn)))
          (map (FcVar . classDictVar) contextDicts)
      desugarFields maybeSelfDictionary = do
        superClassFields <- withDicts contextDicts (mapM (dsEvidence . snd) (tcInstanceSuperClasses instAnn))
        methodFields <-
          mapM
            (dsInstanceMethod contextDicts methods maybeSelfDictionary (tcInstanceHeadTypes instAnn) (tcInstanceDefaultMethods instAnn))
            methodOrder
        pure (superClassFields <> methodFields)
      buildDictionary recursive dictVar fields = do
        methodTypes <- mapM lookupType methodOrder
        let superClassFieldTypes = map tcDictBinderType (tcInstanceClassSuperClasses instAnn)
        (classTyVars, fieldTypes) <-
          case methodTypes of
            [] -> pure (tcInstanceClassTyVars instAnn, superClassFieldTypes)
            _ -> do
              (tyVars, methodFieldTypes) <- classDictionaryLayout className methodTypes
              pure (tyVars, superClassFieldTypes <> methodFieldTypes)
        constructorUnique <- freshUnique
        let dictionaryConstructor = fcDictionaryConstructorName className
            dictionaryType = TcTyCon (TyCon className (length classTyVars)) (map TcTyVar classTyVars)
            constructorType = foldr TcForAllTy (foldr TcFunTy dictionaryType fieldTypes) classTyVars
            constructorVar = Var dictionaryConstructor constructorUnique constructorType
            constructor = foldl FcTyApp (FcVar constructorVar) (tcInstanceHeadTypes instAnn)
            dictionary = foldl FcApp constructor fields
            dictBody = foldr FcTyLam (foldr (FcLam . classDictVar) dictionary contextDicts) (tcInstanceTyVars instAnn)
            bindingGroup
              | recursive = FcRec [(dictVar, dictBody)]
              | otherwise = FcNonRec dictVar dictBody
        pure (FcTopBind bindingGroup)
  if usesDefaultMethod
    then do
      dictVar <- freshVar (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)
      fields <- desugarFields (Just (selfDictionary dictVar))
      buildDictionary True dictVar fields
    else do
      fields <- desugarFields Nothing
      dictVar <- freshVar (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)
      buildDictionary False dictVar fields
  where
    combineMethods (newTy, newMatches) (_oldTy, oldMatches) = (newTy, oldMatches <> newMatches)

dictionaryClassName :: TcType -> Maybe Text
dictionaryClassName ty =
  case ty of
    TcForAllTy _ body -> dictionaryClassName body
    TcQualTy _ body -> dictionaryClassName body
    TcTyCon (TyCon className _) _ -> Just className
    _ -> Nothing

classDictionaryLayout :: Text -> [TcType] -> DsM ([TyVarId], [TcType])
classDictionaryLayout className methodTypes = do
  classTyVars <-
    case methodTypes of
      firstMethod : _ -> classTypeVariables className firstMethod
      [] -> pure []
  fieldTypes <- mapM (classMethodFieldType className classTyVars) methodTypes
  pure (classTyVars, fieldTypes)

classTypeVariables :: Text -> TcType -> DsM [TyVarId]
classTypeVariables className methodType =
  case [args | ClassPred predicateClass args <- predicates, predicateClass == className] of
    args : _ ->
      case traverse asTyVar args of
        Just tyVars -> pure tyVars
        Nothing -> desugarBug ("class predicate has non-variable parameters for " <> T.unpack className)
    [] -> desugarBug ("class method lacks its class predicate for " <> T.unpack className)
  where
    (_, afterForAlls) = peelForAlls methodType
    (predicates, _) = peelQuals afterForAlls
    asTyVar (TcTyVar tyVar) = Just tyVar
    asTyVar _ = Nothing

classMethodFieldType :: Text -> [TyVarId] -> TcType -> DsM TcType
classMethodFieldType className classTyVars methodType = do
  remainingPredicates <-
    case removeClassPredicate predicates of
      Just result -> pure result
      Nothing -> desugarBug ("class method lacks its class predicate for " <> T.unpack className)
  let extraTyVars = filter (`notElem` classTyVars) methodTyVars
      qualifiedBody =
        if null remainingPredicates
          then body
          else TcQualTy remainingPredicates body
  pure (foldr TcForAllTy qualifiedBody extraTyVars)
  where
    (methodTyVars, afterForAlls) = peelForAlls methodType
    (predicates, body) = peelQuals afterForAlls
    removeClassPredicate [] = Nothing
    removeClassPredicate (predicate : rest) =
      case predicate of
        ClassPred predicateClass _
          | predicateClass == className -> Just rest
        _ -> (predicate :) <$> removeClassPredicate rest

mkContextDict :: Int -> TcDictBinderAnnotation -> DsM ClassDict
mkContextDict ix dictAnn = do
  dictVar <- freshVar ("$d" <> T.pack (show ix)) (tcDictBinderType dictAnn)
  pure (ClassDict (tcDictBinderClassName dictAnn) (tcDictBinderArgs dictAnn) dictVar)

dsInstanceMethod :: [ClassDict] -> Map.Map Text (TcType, [Match]) -> Maybe FcExpr -> [TcType] -> [Text] -> Text -> DsM FcExpr
dsInstanceMethod contextDicts methods maybeSelfDictionary headTypes defaults methodName =
  case Map.lookup methodName methods of
    Just (expected, matches) ->
      dsMatchesWithEnclosingDicts contextDicts expected matches
    Nothing
      | methodName `elem` defaults -> do
          selfDictionary <-
            case maybeSelfDictionary of
              Just dictionary -> pure dictionary
              Nothing -> desugarBug ("default method " <> T.unpack methodName <> " requires a recursive instance dictionary")
          let workerName = defaultMethodName methodName
          workerType <- lookupType workerName
          worker <- freshVar workerName workerType
          pure (FcApp (foldl FcTyApp (FcVar worker) headTypes) selfDictionary)
      | otherwise ->
          desugarBug ("missing method " <> T.unpack methodName <> " in instance dictionary")

moduleInstances :: [Decl] -> [Decl]
moduleInstances = filter isInstance
  where
    isInstance decl =
      case peelDeclAnn decl of
        DeclInstance {} -> True
        _ -> False

instanceMethodGroups :: InstanceDecl -> [(Text, (TcType, [Match]))]
instanceMethodGroups instanceDecl =
  concatMap itemMethods (instanceDeclItems instanceDecl)

itemMethods :: InstanceDeclItem -> [(Text, (TcType, [Match]))]
itemMethods item =
  case item of
    InstanceItemAnn ann inner
      | Just methodAnn <- fromAnnotation ann -> itemMethodWithAnnotation methodAnn inner
      | otherwise -> itemMethods inner
    _ -> []

itemMethodWithAnnotation :: TcInstanceMethodAnnotation -> InstanceDeclItem -> [(Text, (TcType, [Match]))]
itemMethodWithAnnotation methodAnn item =
  case item of
    InstanceItemAnn _ inner -> itemMethodWithAnnotation methodAnn inner
    InstanceItemBind (FunctionBind _ matches) ->
      [(tcInstanceMethodName methodAnn, (tcInstanceMethodType methodAnn, matches))]
    InstanceItemBind (PatternBind _ _ rhs) ->
      [ ( tcInstanceMethodName methodAnn,
          ( tcInstanceMethodType methodAnn,
            [ Match
                { matchAnns = [],
                  matchHeadForm = MatchHeadPrefix,
                  matchPats = [],
                  matchRhs = rhs
                }
            ]
          )
        )
      ]
    _ -> []

dropForAlls :: TcType -> TcType
dropForAlls (TcForAllTy _ body) = dropForAlls body
dropForAlls ty = ty

-- | A group of top-level value declarations.
data DeclGroup
  = DeclFunction !Text ![Match]
  | DeclPattern !Text !(Rhs Expr)

dgName :: DeclGroup -> Text
dgName group =
  case group of
    DeclFunction name _ -> name
    DeclPattern name _ -> name

-- | Group consecutive FunctionBind declarations with the same name and keep
-- simple top-level pattern binds.
groupFunctionBinds :: [Decl] -> [DeclGroup]
groupFunctionBinds [] = []
groupFunctionBinds (d : ds) = case extractFunBind d of
  Just (name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        allMatches = matches ++ concatMap (maybe [] snd . extractFunBind) sameNameDecls
     in DeclFunction name allMatches : groupFunctionBinds rest
  Nothing ->
    case extractPatternBind d of
      Just group -> group : groupFunctionBinds ds
      Nothing -> groupFunctionBinds ds

-- | Extract function bind info from a declaration.
extractFunBind :: Decl -> Maybe (Text, [Match])
extractFunBind decl = case peelDeclAnn decl of
  DeclValue (FunctionBind name matches) ->
    Just (unqualifiedNameText name, matches)
  _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: Text -> Decl -> Bool
hasSameName name d = case extractFunBind d of
  Just (n, _) -> n == name
  Nothing -> False

extractPatternBind :: Decl -> Maybe DeclGroup
extractPatternBind decl =
  case peelDeclAnn decl of
    DeclValue (PatternBind _ pat rhs) ->
      DeclPattern <$> barePatternName pat <*> pure rhs
    _ -> Nothing

barePatternName :: Pattern -> Maybe Text
barePatternName pat =
  case pat of
    PVar name -> Just (unqualifiedNameText name)
    PAnn _ inner -> barePatternName inner
    PParen inner -> barePatternName inner
    _ -> Nothing

-- | Desugar a function binding group.
dsGroup :: DeclGroup -> DsM FcTopBind
dsGroup grp = do
  ty <- lookupType (dgName grp)
  u <- freshUnique
  let var = Var (dgName grp) u ty
  body <-
    case grp of
      DeclFunction _ matches -> dsMatches ty matches
      DeclPattern _ rhs -> dsMatches ty [rhsAsMatch rhs]
  pure (FcTopBind (FcNonRec var body))

rhsAsMatch :: Rhs Expr -> Match
rhsAsMatch rhs =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }
