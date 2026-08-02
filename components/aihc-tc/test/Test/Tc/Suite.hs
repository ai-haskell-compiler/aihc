{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Tc.Suite
  ( tcTests,
    tcAnnotatedGoldenTests,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseModule, parseSignatureType)
import Aihc.Parser.Syntax
  ( Annotation,
    CaseAlt (..),
    Decl (..),
    Expr (..),
    Extension (..),
    GuardQualifier (..),
    GuardedRhs (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    Module (..),
    Name (..),
    Pattern (..),
    Rhs (..),
    SourceSpan,
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    stripAnnotations,
  )
import Aihc.Resolve (ResolveResult (..), extractInterface, resolve, resolveWithDeps)
import Aihc.Tc
import Aihc.Tc.Annotations (PendingTcAnnotation, TcClassAnnotation (..), TcClassMethodAnnotation (..), TcInstanceAnnotation (..), TcInstanceMethodAnnotation (..), pendingAnnotation)
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Finalize (finalizeModuleTc)
import Aihc.Tc.Kind (freeTypeVars)
import Aihc.Tc.Monad (emptyTcEnv, initTcState, runTcM)
import Aihc.Tc.Types (mkTyCon)
import Data.Data (Data, gmapQ)
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Typeable (cast)
import TcAnnotatedGolden qualified as TAG
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

tcTests :: TestTree
tcTests =
  testGroup
    "tc-unit"
    [ testGroup "literals" literalTests,
      testGroup "application" applicationTests,
      testGroup "case" caseTests,
      testGroup "data families" dataFamilyTests,
      testGroup "if-then-else" ifTests,
      testGroup "lambda" lambdaTests,
      testGroup "variables" variableTests,
      testGroup "kinds" kindTests,
      testGroup "annotations" annotationTests,
      testGroup "error-cases" errorTests
    ]

-- | Build the inline annotated golden test tree from YAML fixtures.
tcAnnotatedGoldenTests :: IO TestTree
tcAnnotatedGoldenTests = do
  cases <- TAG.loadTcAnnotatedCases
  let tests = map mkAnnotatedGoldenTest cases
  pure (testGroup "tc-annotated-golden" tests)

mkAnnotatedGoldenTest :: TAG.TcAnnotatedCase -> TestTree
mkAnnotatedGoldenTest tcase = testCase (TAG.caseId tcase) $ do
  let (outcome, details) = TAG.evaluateTcAnnotatedCase tcase
  case outcome of
    TAG.OutcomePass -> pure ()
    TAG.OutcomeXFail -> pure ()
    TAG.OutcomeFail ->
      assertFailure
        ( "TC annotated golden test failed: "
            <> TAG.caseId tcase
            <> " details="
            <> details
        )
    TAG.OutcomeXPass ->
      assertFailure
        ( "Unexpected pass in TC annotated golden test: "
            <> TAG.caseId tcase
            <> " details="
            <> details
        )

-- | Typecheck an expression and return the result.
tc :: Text -> TcResult
tc = tcWithDecls ""

tcWithDecls :: Text -> Text -> TcResult
tcWithDecls decls expr =
  let modu =
        typecheckModule $
          parseM
            ( "module Test where\n\
              \data Bool = False | True\n"
                <> decls
                <> "\n__result = "
                <> expr
                <> "\n"
            )
      diags = tcModuleDiagnostics modu
      resultTy =
        case [tbType binding | binding <- tcModuleBindings modu, tbName binding == "__result"] of
          ty : _ -> ty
          [] -> TcMetaTv (Unique (-1))
   in TcResult
        { tcResultType = resultTy,
          tcResultDiagnostics = diags,
          tcResultSuccess = null [diag | diag <- diags, diagSeverity diag == TcError]
        }

hasResolveErrors :: Text -> Bool
hasResolveErrors input =
  let config = defaultConfig {parserSourceName = "<test>"}
      (errs, modu) = parseModule config input
   in null errs
        && case resolve [modu] of
          ResolveResult {resolveErrors} -> not (null resolveErrors)

-- Helper to check that a type is a specific TyCon
isTyCon :: Text -> TcType -> Bool
isTyCon name (TcTyCon tyc []) = tyConName tyc == name
isTyCon _ _ = False

isListOf :: Text -> TcType -> Bool
isListOf name (TcTyCon tyc [TcTyCon elemTyc []]) =
  tyConName tyc == "[]" && tyConName elemTyc == name
isListOf _ _ = False

-- Helper to check function type
isFunTy :: TcType -> Bool
isFunTy (TcFunTy _ _) = True
isFunTy (TcForAllTy _ body) = isFunTy body
isFunTy (TcQualTy _ body) = isFunTy body
isFunTy _ = False

-- | Tests for literal expressions.
literalTests :: [TestTree]
literalTests =
  [ testCase "float literal has type Double" $ do
      let result = tc "3.14"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be Double" (isTyCon "Double" (tcResultType result)),
    testCase "char literal has type Char" $ do
      let result = tc "'a'"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be Char" (isTyCon "Char" (tcResultType result)),
    testCase "string literal has type [Char]" $ do
      let result = tc "\"hello\""
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be [Char]" (isListOf "Char" (tcResultType result))
  ]

-- | Tests for function application.
applicationTests :: [TestTree]
applicationTests =
  [ testCase "application infers result type" $ do
      let result = tcWithDecls "f x = x\narg = True\n" "f arg"
      assertBool "should succeed" (tcResultSuccess result)
  ]

-- | Tests for if-then-else.
ifTests :: [TestTree]
ifTests =
  [ testCase "if-then-else with matching branches" $ do
      let result = tc "if True then False else True"
      let ty = tcResultType result
      assertBool "should produce a type" (ty /= TcMetaTv (Unique (-1)))
  ]

-- | Tests for case expressions.
caseTests :: [TestTree]
caseTests =
  [ testCase "case expression threads scrutinee type into branches" $ do
      let result = tc "\\x -> case x of { y -> y }"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be function type" (isFunTy (tcResultType result))
  ]

dataFamilyTests :: [TestTree]
dataFamilyTests =
  [ testCase "exports standalone data-family instances across module groups" $ do
      let providerResult =
            resolve
              [ parseOnlyWithExtensions
                  [TypeFamilies]
                  "module Provider (Unit(..), Payload(..)) where\n\
                  \data Unit = Unit\n\
                  \data family Payload a\n\
                  \data instance Payload a = PayloadValue a\n"
              ]
          providerExports = extractInterface providerResult
      case providerResult of
        ResolveResult {resolvedModules = providerModules, resolveErrors = []} -> do
          let (checkedProvider, interface) = typecheckModuleSccWithInterface mempty providerModules
          assertBool
            ("provider should typecheck, got: " <> show (concatMap tcModuleDiagnostics checkedProvider))
            (all tcModuleSuccess checkedProvider)
          assertBool
            "checked syntax carries the family representation equation"
            (any (containsParserAnnotation (isJust . fromAnnotation @DataFamilyInstanceInfo)) checkedProvider)
          assertBool "family type constructor exported" ("Payload" `elem` map tciName (tcInterfaceTyCons interface))
          case tcInterfaceDataFamilyInstances interface of
            [familyInstance] -> do
              assertEqual "family name" "Payload" (dfiiFamilyName familyInstance)
              assertEqual "one instance variable" 1 (length (dfiiTyVars familyInstance))
              assertEqual "representation name" "$R$Payload$PayloadValue" (tyConName (dfiiRepresentationTyCon familyInstance))
              assertEqual "axiom name" "$ax$Payload$PayloadValue" (dfiiAxiomName familyInstance)
              assertEqual "constructors" ["PayloadValue"] (dfiiConstructorNames familyInstance)
              assertBool "data instance is not a newtype" (not (dfiiIsNewtype familyInstance))
            familyInstances -> assertFailure ("expected one exported family instance, got: " <> show familyInstances)
          let consumer =
                parseOnlyWithExtensions
                  [TypeFamilies]
                  "module Consumer where\n\
                  \import Provider\n\
                  \unwrap :: Payload Unit -> Unit\n\
                  \unwrap payload = case payload of PayloadValue value -> value\n"
          case resolveWithDeps providerExports [consumer] of
            ResolveResult {resolvedModules = consumerModules, resolveErrors = []} -> do
              let (checkedConsumer, _) = typecheckModuleSccWithInterface interface consumerModules
              assertBool
                ("consumer should typecheck, got: " <> show (concatMap tcModuleDiagnostics checkedConsumer))
                (all tcModuleSuccess checkedConsumer)
            ResolveResult {resolveErrors} ->
              assertFailure ("Resolve error in data-family consumer: " <> show resolveErrors)
        ResolveResult {resolveErrors} ->
          assertFailure ("Resolve error in data-family provider: " <> show resolveErrors),
    testCase "rejects an instance whose head is not a data family" $ do
      let result =
            typecheckModule $
              parseMWithExtensions
                [TypeFamilies]
                "module Test where\n\
                \data Ordinary a = Ordinary a\n\
                \data instance Ordinary a = InvalidOrdinary a\n"
          isInvalidHead diagnostic =
            case diagKind diagnostic of
              OtherError "data-family instance head does not name a data family: Ordinary" -> True
              _ -> False
      assertBool "module should fail" (not (tcModuleSuccess result))
      assertBool ("expected an invalid family-head diagnostic, got: " <> show (tcModuleDiagnostics result)) (any isInvalidHead (tcModuleDiagnostics result))
  ]

-- | Tests for lambda expressions.
lambdaTests :: [TestTree]
lambdaTests =
  [ testCase "lambda produces function type" $ do
      let result = tc "\\x -> True"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be function type" (isFunTy (tcResultType result))
  ]

-- | Tests for variable expressions.
variableTests :: [TestTree]
variableTests =
  [ testCase "unbound variable is rejected by resolver before TC" $ do
      assertBool "resolver should reject unbound variable" (hasResolveErrors "module Test where\nx = undefined_var\n")
  ]

kindTests :: [TestTree]
kindTests =
  [ testCase "explicit forall removes every occurrence of its bound variables" $ do
      case parseSignatureType defaultConfig {parserExtensions = [ExplicitForAll]} "forall a. a -> a" of
        ParseOk signature -> assertEqual "free type variables" [] (freeTypeVars signature)
        ParseErr errors -> assertFailure ("signature should parse: " <> show errors),
    testCase "primitive types retain their runtime-representation kinds" $ do
      assertEqual
        "Int kind"
        liftedTypeKind
        (typeKind (TcTyCon (TyCon "Int" 0) []))
      mapM_
        ( \(name, runtimeRep) ->
            assertEqual
              (show name <> " kind")
              (KTYPE runtimeRep)
              (typeKind (TcTyCon (TyCon name 0) []))
        )
        [ ("Int#", IntRep),
          ("Int8#", Int8Rep),
          ("Int16#", Int16Rep),
          ("Int32#", Int32Rep),
          ("Int64#", Int64Rep),
          ("Word#", WordRep),
          ("Word8#", Word8Rep),
          ("Word16#", Word16Rep),
          ("Word32#", Word32Rep),
          ("Word64#", Word64Rep),
          ("Addr#", AddrRep),
          ("Float#", FloatRep),
          ("Double#", DoubleRep),
          ("Char#", WordRep),
          ("ThreadId#", BoxedRep Unlifted)
        ],
    testCase "constraint dictionaries have a lifted runtime representation" $ do
      let dictionaryTy =
            TcTyCon
              (mkTyCon "Eq" 1 (KFun liftedTypeKind KConstraint))
              [TcTyCon (TyCon "Bool" 0) []]
      assertEqual "dictionary representation" (Right liftedRuntimeRep) (runtimeRepOfType dictionaryTy),
    testCase "records source-level constructor field representations" $ do
      let result =
            typecheckModule $
              parseM
                "{-# LANGUAGE MagicHash #-}\n\
                \module Test where\n\
                \data Box = Box Int#\n"
          boxTypes = [tbType binding | binding <- tcModuleBindings result, tbName binding == "Box"]
      assertBool ("module should typecheck, got: " <> show (tcModuleDiagnostics result)) (tcModuleSuccess result)
      case boxTypes of
        [TcFunTy fieldTy resultTy] -> do
          assertEqual "field representation" (Right IntRep) (runtimeRepOfType fieldTy)
          assertEqual "result representation" (Right liftedRuntimeRep) (runtimeRepOfType resultTy)
          assertBool "constructor type kinds are finalized" (not (hasKindMeta fieldTy || hasKindMeta resultTy))
        other -> assertFailure ("unexpected Box constructor types: " <> show other),
    testCase "preserves dependent TYPE runtime representations" $ do
      let result =
            typecheckModule $
              parseM
                "{-# LANGUAGE DataKinds #-}\n\
                \{-# LANGUAGE KindSignatures #-}\n\
                \{-# LANGUAGE PolyKinds #-}\n\
                \module Test where\n\
                \data R (r :: RuntimeRep) (a :: TYPE r) = R\n"
          constructorTypes = [tbType binding | binding <- tcModuleBindings result, tbName binding == "R"]
      assertBool ("module should typecheck, got: " <> show (tcModuleDiagnostics result)) (tcModuleSuccess result)
      case constructorTypes of
        [TcForAllTy repVar (TcForAllTy valueVar (TcTyCon _ [TcTyVar resultRepVar, TcTyVar resultValueVar]))] -> do
          assertEqual "representation binder kind" KRuntimeRep (tvKind repVar)
          assertEqual
            "value binder kind"
            (KTYPE (RuntimeRepVar (tvUnique repVar)))
            (tvKind valueVar)
          assertEqual "result representation argument" repVar resultRepVar
          assertEqual "result value argument" valueVar resultValueVar
        other -> assertFailure ("unexpected R constructor types: " <> show other),
    testCase "rejects unsaturated type constructor in signature" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \data M a = J a | N\n\
                \fn :: M\n\
                \fn = fn\n"
      assertBool "module should fail" (not (tcModuleSuccess result))
      assertBool "should report a kind mismatch" (any isKindMismatch (tcModuleDiagnostics result)),
    testCase "accepts saturated type constructor in signature" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \data Int = I\n\
                \data M a = J a | N\n\
                \fn :: M Int\n\
                \fn = N\n"
      assertBool "module should typecheck" (tcModuleSuccess result),
    testCase "infers the kind of an imported class parameter" $ do
      let baseResult = resolve [parseOnly "module Base (Monad) where\nclass Monad m where\n"]
          depExports = extractInterface baseResult
          target = parseOnly "module Test where\nimport Base\nliftedId :: Monad m => m a -> m a\nliftedId x = x\n"
          resolved = resolveWithDeps depExports [target]
      case resolved of
        ResolveResult {resolvedModules = [modu], resolveErrors = []} -> do
          let result = typecheckModule modu
          assertBool ("module should typecheck, got: " <> show (tcModuleDiagnostics result)) (tcModuleSuccess result)
        ResolveResult {resolveErrors} ->
          assertFailure ("Resolve error in imported-class kind test: " <> show resolveErrors),
    testCase "uses imported type constructor arities" $ do
      let baseResult = resolve [parseOnly "module Base (Box) where\ndata Box a = Box a\n"]
          depExports = extractInterface baseResult
      case baseResult of
        ResolveResult {resolvedModules = baseModules, resolveErrors = []} -> do
          let (checkedBase, _, importedTyCons) = typecheckModulesWithFullEnv [] [] [] baseModules
          assertBool "dependency should typecheck" (all tcModuleSuccess checkedBase)
          let target = parseOnly "module Test where\nimport Base\ndata Unit = Unit\nkeep :: Box Unit -> Box Unit\nkeep x = x\n"
          case resolveWithDeps depExports [target] of
            ResolveResult {resolvedModules = targetModules, resolveErrors = []} -> do
              let (checkedTarget, _, _) = typecheckModulesWithFullEnv [] importedTyCons [] targetModules
              assertBool
                ("module should typecheck, got: " <> show (concatMap tcModuleDiagnostics checkedTarget))
                (all tcModuleSuccess checkedTarget)
            ResolveResult {resolveErrors} ->
              assertFailure ("Resolve error in imported-type test: " <> show resolveErrors)
        ResolveResult {resolveErrors} ->
          assertFailure ("Resolve error in dependency-type test: " <> show resolveErrors),
    testCase "transports a complete semantic interface between module groups" $ do
      let baseResult =
            resolve
              [ parseOnly
                  "module Base (Unit(..), Identity(..)) where\n\
                  \data Unit = Unit\n\
                  \class Identity a where\n\
                  \  identity :: a -> a\n\
                  \instance Identity Unit where\n\
                  \  identity value = value\n"
              ]
          depExports = extractInterface baseResult
      case baseResult of
        ResolveResult {resolvedModules = baseModules, resolveErrors = []} -> do
          let (checkedBase, interface) = typecheckModuleSccWithInterface mempty baseModules
          assertBool "dependency should typecheck" (all tcModuleSuccess checkedBase)
          assertBool "constructor term exported" ("Unit" `elem` map fst (tcInterfaceTerms interface))
          assertBool "method term exported" ("identity" `elem` map fst (tcInterfaceTerms interface))
          assertBool "type constructor exported" ("Unit" `elem` map tciName (tcInterfaceTyCons interface))
          assertBool "class exported" ("Identity" `elem` map ciName (tcInterfaceClasses interface))
          assertBool "instance exported" ("$fIdentityUnit" `elem` map iiDictName (tcInterfaceInstances interface))
          let target = parseOnly "module Test where\nimport Base\nanswer :: Unit\nanswer = identity Unit\n"
          case resolveWithDeps depExports [target] of
            ResolveResult {resolvedModules = targetModules, resolveErrors = []} -> do
              let (checkedTarget, _) = typecheckModuleSccWithInterface interface targetModules
              assertBool
                ("consumer should typecheck, got: " <> show (concatMap tcModuleDiagnostics checkedTarget))
                (all tcModuleSuccess checkedTarget)
            ResolveResult {resolveErrors} ->
              assertFailure ("Resolve error in semantic-interface consumer: " <> show resolveErrors)
        ResolveResult {resolveErrors} ->
          assertFailure ("Resolve error in semantic-interface provider: " <> show resolveErrors)
  ]
  where
    isKindMismatch diag =
      case diagKind diag of
        KindMismatch {} -> True
        _ -> False

    hasKindMeta ty =
      case ty of
        TcTyVar tv -> kindHasMeta (tvKind tv)
        TcMetaTv {} -> True
        TcTyCon tyCon args -> kindHasMeta (tyConKind tyCon) || any hasKindMeta args
        TcFunTy argument result -> hasKindMeta argument || hasKindMeta result
        TcForAllTy tv body -> kindHasMeta (tvKind tv) || hasKindMeta body
        TcQualTy predicates body -> any predHasKindMeta predicates || hasKindMeta body
        TcAppTy function argument -> hasKindMeta function || hasKindMeta argument
    predHasKindMeta predicate =
      case predicate of
        ClassPred _ args -> any hasKindMeta args
        EqPred left right -> hasKindMeta left || hasKindMeta right
    kindHasMeta kind =
      case kind of
        KMeta {} -> True
        KFun argument result -> kindHasMeta argument || kindHasMeta result
        KTYPE runtimeRep -> runtimeRepHasMeta runtimeRep
        _ -> False
    runtimeRepHasMeta runtimeRep =
      case runtimeRep of
        VecRep {} -> False
        TupleRep reps -> any runtimeRepHasMeta reps
        SumRep reps -> any runtimeRepHasMeta reps
        RuntimeRepVar {} -> False
        RuntimeRepMeta {} -> True
        _ -> False

annotationTests :: [TestTree]
annotationTests =
  [ testCase "typeclass annotations select concrete dictionaries" $ do
      let result = typecheckModule annotationModule
      assertBool "module should typecheck" (tcModuleSuccess result)
      assertBool "Eq Bool evidence" ("$fEqBool" `elem` evidenceDictNames result)
      assertBool "Eq [Bool] evidence" ("$fEqList" `elem` evidenceDictNames result)
      assertBool "Default Bool evidence" ("$fDefaultBool" `elem` evidenceDictNames result)
      assertBool "given Eq a evidence inside list instance" (hasGivenClass "Eq" result),
    testCase "class and instance annotations carry dictionary layout" $ do
      let result = typecheckModule annotationModule
      assertBool "module should typecheck" (tcModuleSuccess result)
      assertBool "Eq class methods annotated" (hasClassMethod "==" 0 result)
      assertBool "Default class method annotated" (hasClassMethod "def" 0 result)
      assertBool "superclass layout annotated" (not (all (null . tcClassSuperClasses) (classAnnotations result)))
      assertBool "class default method annotated" (any (elem "identity" . tcClassDefaultMethods) (classAnnotations result))
      assertBool "Eq Bool instance annotated" (hasInstanceDict "$fEqBool" result)
      assertBool "Eq list instance annotated" (hasInstanceDict "$fEqList" result)
      assertBool "Default Bool instance annotated" (hasInstanceDict "$fDefaultBool" result)
      assertBool "Identity Bool instance annotated" (hasInstanceDict "$fIdentityBool" result)
      assertBool "instance superclass evidence annotated" (not (all (null . tcInstanceSuperClasses) (instanceAnnotations result)))
      assertBool "instance method types annotated" (hasInstanceMethod "==" result && hasInstanceMethod "def" result),
    testCase "AnyClass deriving exports inferred instance contexts" $ do
      let result =
            typecheckModule $
              parseMWithExtensions
                [DefaultSignatures, DeriveAnyClass, DerivingStrategies]
                "{-# LANGUAGE DefaultSignatures, DeriveAnyClass, DerivingStrategies #-}\n\
                \module Test where\n\
                \class Show a where\n\
                \  showValue :: a -> a\n\
                \class C a where\n\
                \  c :: a -> a\n\
                \  default c :: Show a => a -> a\n\
                \  c x = showValue x\n\
                \data T a = T a deriving anyclass C\n\
                \instance Show a => Show (T a) where\n\
                \  showValue x = x\n\
                \data Unit = Unit\n\
                \instance Show Unit where\n\
                \  showValue x = x\n\
                \useDerived :: T Unit -> T Unit\n\
                \useDerived value = c value\n\
                \class Parent a where\n\
                \class Parent a => Child a where\n\
                \data S a = S a deriving anyclass (Child, Parent)\n"
          instances = tcModuleInstances result
          plans = derivingPlans result
          contexts className = [map renderPred (iiContext instanceInfo) | instanceInfo <- instances, iiClassName instanceInfo == className]
      assertBool ("module should typecheck, got: " <> show (tcModuleDiagnostics result)) (tcModuleSuccess result)
      assertEqual "default-signature context" [["Show a"]] (contexts "C")
      assertEqual "sibling superclass context" [[]] (contexts "Child")
      assertEqual "sibling AnyClass instance" [[]] (contexts "Parent")
      assertBool "same-module values can select the derived dictionary" ("$fCTa" `elem` evidenceDictNames result)
      assertBool "derived dictionary type is exported downstream" (any ((== "$fCTa") . tbName) (tcModuleBindings result))
      assertBool "default-method evidence is checked in TC" (not (all (null . tcDerivingDefaultMethodEvidence) plans))
      assertBool "superclass evidence is checked in TC" (not (all (null . tcDerivingSuperClasses) plans)),
    testCase "instance methods retain method-local constraints" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \data Unit = Unit\n\
                \class Required a where\n\
                \  required :: a -> a\n\
                \instance Required Unit where\n\
                \  required value = value\n\
                \class Container a where\n\
                \  applyRequired :: Required b => a -> b -> b\n\
                \instance Container Unit where\n\
                \  applyRequired _ value = required value\n\
                \result = applyRequired Unit Unit\n"
      assertBool "method-local Required b evidence should remain in scope" (tcModuleSuccess result)
      assertBool "Required Unit evidence should be selected at the call site" ("$fRequiredUnit" `elem` evidenceDictNames result),
    testCase "polymorphic occurrence type arguments are finalized" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \f x = x\n\
                \g y = f (f y)\n"
          typeArgs = concatMap tcAnnTypeArgs (exprAnnotations result)
      assertBool "module should typecheck" (tcModuleSuccess result)
      assertBool "expected polymorphic occurrence type arguments" (not (null typeArgs))
      assertBool "type arguments should not leak unsolved metas" (not (any hasMetaTcType typeArgs)),
    testCase "module typechecking finalizes all pending annotations" $ do
      let result = typecheckModule annotationModule
      assertBool "module should typecheck" (tcModuleSuccess result)
      assertBool "pending annotations should not escape" (not (containsPendingTcAnnotation result)),
    testCase "located module diagnostics are attached to syntax nodes" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \bad xs = [ x | x <- xs, () ]\n"
      assertBool "module should fail" (not (tcModuleSuccess result))
      assertBool "diagnostics should be attached to the annotated syntax tree" (containsParserAnnotation isTcDiagnosticAnnotation result)
      assertBool "located diagnostics should not be attached to module annotations" (not (any isTcDiagnosticAnnotation (moduleAnns result))),
    testCase "module diagnostics do not require source span annotations" $ do
      let unspannedModule =
            stripAnnotations $
              parseM
                "module Test where\n\
                \data M a = J a | N\n\
                \fn :: M\n\
                \fn = fn\n"
          result = typecheckModule unspannedModule
      assertBool "input should not carry source span annotations" (not (containsSourceSpanAnnotation unspannedModule))
      assertBool "module should fail" (not (tcModuleSuccess result))
      assertBool "diagnostics should be attached to module annotations" (any isTcDiagnosticAnnotation (moduleAnns result))
      assertBool "diagnostic location should be absent" (not (any hasDiagnosticLocation (tcModuleDiagnostics result))),
    testCase "finalization rewrites pending annotations in arbitrary syntax positions" $ do
      let pendingModule =
            withPendingGuardedRhsAnnotations $
              parseM
                "module Test where\n\
                \f x | x = x\n"
      case runTcM emptyTcEnv initTcState (finalizeModuleTc pendingModule) of
        Left abort ->
          assertFailure ("finalization aborted: " <> show abort)
        Right (finalizedModule, _) -> do
          assertBool "pending annotations should be finalized" (not (containsPendingTcAnnotation finalizedModule))
          assertBool "final annotations should remain" (containsFinalTcAnnotation finalizedModule),
    testCase "type rendering uses unicode syntax" $ do
      let a = TyVarId "a" (Unique 1)
          aTy = TcTyVar a
          eqA = ClassPred "Eq" [aTy]
          ty = TcForAllTy a (TcQualTy [eqA] (TcFunTy aTy aTy))
      assertEqual
        "rendered signature"
        "f ∷ ∀ a. (Eq a) ⇒ a → a"
        (renderTcSignature "f" ty)
  ]

annotationModule :: Module
annotationModule =
  parseM
    "module Test where\n\
    \data Bool = False | True\n\
    \class Eq a where\n\
    \  (==) :: a -> a -> Bool\n\
    \instance Eq Bool where\n\
    \  False == False = True\n\
    \  False == True = False\n\
    \  True == False = False\n\
    \  True == True = True\n\
    \instance Eq a => Eq [a] where\n\
    \  [] == [] = True\n\
    \  [] == (_ : _) = False\n\
    \  (_ : _) == [] = False\n\
    \  (x : _) == (y : _) = x == y\n\
    \eqBool :: Bool -> Bool -> Bool\n\
    \eqBool = (==)\n\
    \eqListBool :: [Bool] -> [Bool] -> Bool\n\
    \eqListBool = (==)\n\
    \class Default a where\n\
    \  def :: a\n\
    \instance Default Bool where\n\
    \  def = True\n\
    \useDefault :: Bool\n\
    \useDefault = def\n\
    \class Eq a => Identity a where\n\
    \  identity :: a -> a\n\
    \  identity x = x\n\
    \instance Identity Bool\n\
    \useIdentity :: Bool\n\
    \useIdentity = identity True\n"

parseM :: Text -> Module
parseM = parseMWithExtensions []

parseMWithExtensions :: [Extension] -> Text -> Module
parseMWithExtensions extensions input =
  case resolve [parseOnlyWithExtensions extensions input] of
    ResolveResult {resolvedModules = [resolved], resolveErrors = []} -> resolved
    ResolveResult {resolveErrors} -> error ("Resolve error in test: " ++ show resolveErrors)

parseOnly :: Text -> Module
parseOnly = parseOnlyWithExtensions []

parseOnlyWithExtensions :: [Extension] -> Text -> Module
parseOnlyWithExtensions extensions input =
  let config = defaultConfig {parserSourceName = "<test>", parserExtensions = extensions}
      (errs, modu) = parseModule config input
   in if null errs
        then modu
        else error ("Parse error in test: " ++ show errs)

evidenceDictNames :: Module -> [Text]
evidenceDictNames =
  concatMap evDictNames . concatMap tcAnnEvidenceTerms . exprAnnotations

evDictNames :: EvTerm -> [Text]
evDictNames (EvDict name _ evidence) = name : concatMap evDictNames evidence
evDictNames (EvSuperClass ev _ _ _) = evDictNames ev
evDictNames (EvCast ev _) = evDictNames ev
evDictNames _ = []

hasGivenClass :: Text -> Module -> Bool
hasGivenClass className =
  any (any isGiven . tcAnnEvidenceTerms) . exprAnnotations
  where
    isGiven (EvGiven (ClassPred cls _)) = cls == className
    isGiven (EvDict _ _ evidence) = any isGiven evidence
    isGiven (EvSuperClass ev _ _ _) = isGiven ev
    isGiven (EvCast ev _) = isGiven ev
    isGiven _ = False

hasMetaTcType :: TcType -> Bool
hasMetaTcType ty =
  case ty of
    TcMetaTv {} -> True
    TcTyCon _ args -> any hasMetaTcType args
    TcFunTy arg result -> hasMetaTcType arg || hasMetaTcType result
    TcForAllTy _ body -> hasMetaTcType body
    TcQualTy preds body -> any hasMetaPred preds || hasMetaTcType body
    TcAppTy fun arg -> hasMetaTcType fun || hasMetaTcType arg
    TcTyVar {} -> False
  where
    hasMetaPred (ClassPred _ args) = any hasMetaTcType args
    hasMetaPred (EqPred left right) = hasMetaTcType left || hasMetaTcType right

hasClassMethod :: Text -> Int -> Module -> Bool
hasClassMethod methodName methodIndex =
  any matches . classAnnotations
  where
    matches classAnn =
      any (\method -> tcClassMethodName method == methodName && tcClassMethodIndex method == methodIndex) (tcClassMethods classAnn)

hasInstanceDict :: Text -> Module -> Bool
hasInstanceDict dictName =
  any ((== dictName) . tcInstanceDictName) . instanceAnnotations

hasInstanceMethod :: Text -> Module -> Bool
hasInstanceMethod methodName =
  any ((== methodName) . tcInstanceMethodName) . instanceMethodAnnotations

classAnnotations :: Module -> [TcClassAnnotation]
classAnnotations =
  concatMap goDecl . moduleDecls
  where
    goDecl (DeclAnn ann inner) =
      maybeToList (fromAnnotation ann) <> goDecl inner
    goDecl _ = []

instanceAnnotations :: Module -> [TcInstanceAnnotation]
instanceAnnotations =
  concatMap goDecl . moduleDecls
  where
    goDecl (DeclAnn ann inner) =
      maybeToList (fromAnnotation ann) <> goDecl inner
    goDecl _ = []

derivingPlans :: Module -> [TcDerivingPlan]
derivingPlans =
  concatMap goDecl . moduleDecls
  where
    goDecl (DeclAnn ann inner) =
      maybe [] tcDerivingPlans (fromAnnotation ann) <> goDecl inner
    goDecl _ = []

instanceMethodAnnotations :: Module -> [TcInstanceMethodAnnotation]
instanceMethodAnnotations =
  concatMap goDecl . moduleDecls
  where
    goDecl (DeclAnn _ inner) = goDecl inner
    goDecl (DeclInstance instanceDecl) = concatMap goItem (instanceDeclItems instanceDecl)
    goDecl _ = []

    goItem (InstanceItemAnn ann inner) =
      maybeToList (fromAnnotation ann) <> goItem inner
    goItem _ = []

exprAnnotations :: Module -> [TcAnnotation]
exprAnnotations =
  concatMap declExprAnnotations . moduleDecls

containsPendingTcAnnotation :: (Data a) => a -> Bool
containsPendingTcAnnotation =
  containsParserAnnotation (isJust . fromAnnotation @PendingTcAnnotation)

containsFinalTcAnnotation :: (Data a) => a -> Bool
containsFinalTcAnnotation =
  containsParserAnnotation (isJust . fromAnnotation @TcAnnotation)

containsSourceSpanAnnotation :: (Data a) => a -> Bool
containsSourceSpanAnnotation =
  containsParserAnnotation (isJust . fromAnnotation @SourceSpan)

isTcDiagnosticAnnotation :: Annotation -> Bool
isTcDiagnosticAnnotation =
  isJust . fromAnnotation @TcDiagnostic

hasDiagnosticLocation :: TcDiagnostic -> Bool
hasDiagnosticLocation =
  isJust . diagLoc

containsParserAnnotation :: (Data a) => (Annotation -> Bool) -> a -> Bool
containsParserAnnotation predicate value =
  case cast value :: Maybe Annotation of
    Just ann -> predicate ann
    Nothing -> or (gmapQ (containsParserAnnotation predicate) value)

withPendingGuardedRhsAnnotations :: Module -> Module
withPendingGuardedRhsAnnotations modu =
  modu {moduleDecls = map goDecl (moduleDecls modu)}
  where
    pendingAnn =
      mkAnnotation (pendingAnnotation (TcTyCon (TyCon "Int" 0) []) [] [] [])

    goDecl (DeclAnn ann inner) =
      DeclAnn ann (goDecl inner)
    goDecl (DeclValue (FunctionBind name matches)) =
      DeclValue (FunctionBind name (map goMatch matches))
    goDecl other =
      other

    goMatch match =
      match {matchRhs = goRhs (matchRhs match)}

    goRhs (GuardedRhss anns guarded maybeDecls) =
      GuardedRhss (pendingAnn : anns) (map goGuardedRhs guarded) maybeDecls
    goRhs other =
      other

    goGuardedRhs guarded =
      guarded
        { guardedRhsAnns = pendingAnn : guardedRhsAnns guarded,
          guardedRhsGuards = map goGuardQualifier (guardedRhsGuards guarded),
          guardedRhsBody = EAnn pendingAnn (guardedRhsBody guarded)
        }

    goGuardQualifier (GuardAnn ann inner) =
      GuardAnn ann (goGuardQualifier inner)
    goGuardQualifier (GuardExpr expr) =
      GuardExpr (EAnn pendingAnn expr)
    goGuardQualifier (GuardPat pat expr) =
      GuardPat (PAnn pendingAnn pat) (EAnn pendingAnn expr)
    goGuardQualifier (GuardLet decls) =
      GuardLet (map goDecl decls)

declExprAnnotations :: Decl -> [TcAnnotation]
declExprAnnotations decl =
  case decl of
    DeclAnn _ inner -> declExprAnnotations inner
    DeclValue valueDecl -> valueDeclExprAnnotations valueDecl
    DeclInstance instanceDecl -> concatMap instanceItemExprAnnotations (instanceDeclItems instanceDecl)
    _ -> []

instanceItemExprAnnotations :: InstanceDeclItem -> [TcAnnotation]
instanceItemExprAnnotations item =
  case item of
    InstanceItemAnn _ inner -> instanceItemExprAnnotations inner
    InstanceItemBind valueDecl -> valueDeclExprAnnotations valueDecl
    _ -> []

valueDeclExprAnnotations :: ValueDecl -> [TcAnnotation]
valueDeclExprAnnotations valueDecl =
  case valueDecl of
    FunctionBind _ matches -> concatMap matchExprAnnotations matches
    PatternBind _ _ rhs -> rhsExprAnnotations rhs

matchExprAnnotations :: Match -> [TcAnnotation]
matchExprAnnotations = rhsExprAnnotations . matchRhs

rhsExprAnnotations :: Rhs Expr -> [TcAnnotation]
rhsExprAnnotations rhs =
  case rhs of
    UnguardedRhs _ expr maybeDecls ->
      exprExprAnnotations expr <> maybe [] (concatMap declExprAnnotations) maybeDecls
    GuardedRhss _ _ maybeDecls ->
      maybe [] (concatMap declExprAnnotations) maybeDecls

exprExprAnnotations :: Expr -> [TcAnnotation]
exprExprAnnotations expr =
  case expr of
    EAnn ann inner ->
      maybeToList (fromAnnotation ann) <> exprExprAnnotations inner
    EApp fun arg -> exprExprAnnotations fun <> exprExprAnnotations arg
    EInfix lhs op rhs -> exprExprAnnotations lhs <> nameExprAnnotations op <> exprExprAnnotations rhs
    EList elems -> concatMap exprExprAnnotations elems
    ETuple _ elems -> concatMap (maybe [] exprExprAnnotations) elems
    EIf cond thenE elseE -> exprExprAnnotations cond <> exprExprAnnotations thenE <> exprExprAnnotations elseE
    ECase scrut alts -> exprExprAnnotations scrut <> concatMap caseAltExprAnnotations alts
    ELetDecls decls body -> concatMap declExprAnnotations decls <> exprExprAnnotations body
    ELambdaPats _ body -> exprExprAnnotations body
    EParen inner -> exprExprAnnotations inner
    ETypeSig inner _ -> exprExprAnnotations inner
    _ -> []

nameExprAnnotations :: Name -> [TcAnnotation]
nameExprAnnotations name =
  mapMaybe fromAnnotation (nameAnns name)

caseAltExprAnnotations :: CaseAlt Expr -> [TcAnnotation]
caseAltExprAnnotations (CaseAlt _ _ rhs) = rhsExprAnnotations rhs

-- | Tests for error cases.
errorTests :: [TestTree]
errorTests =
  [ testCase "unbound variable is not a TC error" $ do
      assertBool "resolver should reject unbound variable" (hasResolveErrors "module Test where\nx = foo\n")
  ]
