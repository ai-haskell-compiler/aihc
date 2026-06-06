{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Tc.Suite
  ( tcTests,
    tcAnnotatedGoldenTests,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr, parseModule)
import Aihc.Parser.Syntax
  ( Annotation,
    CaseAlt (..),
    CompStmt (..),
    Decl (..),
    Expr (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    Module (..),
    Name (..),
    Rhs (..),
    SourceSpan (..),
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (ResolutionAnnotation (..), ResolveResult (..), resolve)
import Aihc.Tc
import Aihc.Tc.Annotations (TcClassAnnotation (..), TcClassMethodAnnotation (..), TcInstanceAnnotation (..), TcInstanceMethodAnnotation (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Data.Data (Data, cast, gmapM)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
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

-- | Parse an expression from text.
parseE :: Text -> Expr
parseE input =
  let config = defaultConfig {parserSourceName = "<test>"}
   in case parseExpr config input of
        ParseOk result -> result
        ParseErr err -> error ("Parse error in test: " ++ show err)

-- | Typecheck an expression and return the result.
tc :: Text -> TcResult
tc = typecheckExpr . parseE

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
isFunTy _ = False

-- | Tests for literal expressions.
literalTests :: [TestTree]
literalTests =
  [ testCase "integer literal has type Int" $ do
      let result = tc "42"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be Int" (isTyCon "Int" (tcResultType result)),
    testCase "float literal has type Double" $ do
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
      let result = tc "f x"
      assertBool "should have errors (unbound)" (not (tcResultSuccess result))
  ]

-- | Tests for if-then-else.
ifTests :: [TestTree]
ifTests =
  [ testCase "if-then-else with matching branches" $ do
      let result = tc "if True then 1 else 2"
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

-- | Tests for lambda expressions.
lambdaTests :: [TestTree]
lambdaTests =
  [ testCase "lambda produces function type" $ do
      let result = tc "\\x -> 42"
      assertBool "should succeed" (tcResultSuccess result)
      assertBool "should be function type" (isFunTy (tcResultType result))
  ]

-- | Tests for variable expressions.
variableTests :: [TestTree]
variableTests =
  [ testCase "unbound variable produces error" $ do
      let result = tc "undefined_var"
      assertBool "should fail" (not (tcResultSuccess result))
      assertBool "should have diagnostics" (not (null (tcResultDiagnostics result)))
  ]

kindTests :: [TestTree]
kindTests =
  [ testCase "rejects unsaturated type constructor in signature" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \data M a = J a | N\n\
                \fn :: M\n\
                \fn = fn\n"
      assertBool "module should fail" (not (tcmSuccess result))
      assertBool "should report a kind mismatch" (any isKindMismatch (tcModuleDiagnostics (tcmModule result))),
    testCase "accepts saturated type constructor in signature" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \data Int = I\n\
                \data M a = J a | N\n\
                \fn :: M Int\n\
                \fn = N\n"
      assertBool "module should typecheck" (tcmSuccess result)
  ]
  where
    isKindMismatch diag =
      case diagKind diag of
        KindMismatch {} -> True
        _ -> False

annotationTests :: [TestTree]
annotationTests =
  [ testCase "typeclass annotations select concrete dictionaries" $ do
      let result = typecheckModule annotationModule
      assertBool "module should typecheck" (tcmSuccess result)
      assertBool "Eq Bool evidence" ("$fEqBool" `elem` evidenceDictNames (tcmModule result))
      assertBool "Eq [Bool] evidence" ("$fEqList" `elem` evidenceDictNames (tcmModule result))
      assertBool "Default Bool evidence" ("$fDefaultBool" `elem` evidenceDictNames (tcmModule result))
      assertBool "given Eq a evidence inside list instance" (hasGivenClass "Eq" (tcmModule result)),
    testCase "class and instance annotations carry dictionary layout" $ do
      let result = typecheckModule annotationModule
      assertBool "module should typecheck" (tcmSuccess result)
      assertBool "Eq class methods annotated" (hasClassMethod "==" 0 (tcmModule result))
      assertBool "Default class method annotated" (hasClassMethod "def" 0 (tcmModule result))
      assertBool "Eq Bool instance annotated" (hasInstanceDict "$fEqBool" (tcmModule result))
      assertBool "Eq list instance annotated" (hasInstanceDict "$fEqList" (tcmModule result))
      assertBool "Default Bool instance annotated" (hasInstanceDict "$fDefaultBool" (tcmModule result))
      assertBool "instance method types annotated" (hasInstanceMethod "==" (tcmModule result) && hasInstanceMethod "def" (tcmModule result)),
    testCase "polymorphic occurrence type arguments are finalized" $ do
      let result =
            typecheckModule $
              parseM
                "module Test where\n\
                \f x = x\n\
                \g y = f (f y)\n"
          typeArgs = concatMap tcAnnTypeArgs (exprAnnotations (tcmModule result))
      assertBool "module should typecheck" (tcmSuccess result)
      assertBool "expected polymorphic occurrence type arguments" (not (null typeArgs))
      assertBool "type arguments should not leak unsolved metas" (not (any hasMetaTcType typeArgs)),
    testCase "type rendering uses unicode syntax" $ do
      let a = TyVarId "a" (Unique 1)
          aTy = TcTyVar a
          eqA = ClassPred "Eq" [aTy]
          ty = TcForAllTy a (TcQualTy [eqA] (TcFunTy aTy aTy))
      assertEqual
        "rendered signature"
        "f ∷ ∀ a. (Eq a) ⇒ a → a"
        (renderTcSignature "f" ty),
    testCase "typecheck annotations do not require source span annotations" $ do
      let result =
            typecheckModule $
              eraseSourceLocations $
                parseM
                  "module Test where\n\
                  \id x = x\n"
      assertBool "module should typecheck" (tcmSuccess result)
      assertBool "binding annotation should be present" (not (null (tcmBindings result))),
    testCase "type error diagnostics do not require source span annotations" $ do
      let result =
            typecheckModule $
              eraseSourceLocations $
                parseM
                  "module Test where\n\
                  \data Bool = True | False\n\
                  \list = [True, ()]\n"
          diagnostics = tcModuleDiagnostics (tcmModule result)
      assertBool "module should fail" (not (tcmSuccess result))
      assertBool "diagnostic should be attached to returned module" (not (null diagnostics))
      assertBool "should report a unification error" (any isUnificationError diagnostics)
  ]
  where
    isUnificationError diagnostic =
      case diagKind diagnostic of
        UnificationError {} -> True
        _ -> False

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
    \useDefault = def\n"

parseM :: Text -> Module
parseM input =
  let config = defaultConfig {parserSourceName = "<test>"}
      (errs, modu) = parseModule config input
   in if null errs
        then case resolve [modu] of
          ResolveResult {resolvedModules = [resolved], resolveErrors = []} -> resolved
          ResolveResult {resolveErrors} -> error ("Resolve error in test: " ++ show resolveErrors)
        else error ("Parse error in test: " ++ show errs)

eraseSourceLocations :: Module -> Module
eraseSourceLocations =
  eraseSourceLocationsData

eraseSourceLocationsData :: (Data a) => a -> a
eraseSourceLocationsData node =
  case cast node of
    Just (anns :: [Annotation]) ->
      fromMaybe node (cast (mapMaybe eraseSourceAnnotation anns))
    Nothing ->
      runIdentity (gmapM (Identity . eraseSourceLocationsData) node)

eraseSourceAnnotation :: Annotation -> Maybe Annotation
eraseSourceAnnotation ann
  | Just (_ :: SourceSpan) <- fromAnnotation ann = Nothing
  | Just resolution <- fromAnnotation ann =
      Just (mkAnnotation (resolution {resolutionSpan = NoSourceSpan}))
  | otherwise = Just ann

evidenceDictNames :: Module -> [Text]
evidenceDictNames =
  concatMap evDictNames . concatMap tcAnnEvidenceTerms . exprAnnotations

evDictNames :: EvTerm -> [Text]
evDictNames (EvDict name _ evidence) = name : concatMap evDictNames evidence
evDictNames (EvSuperClass ev _) = evDictNames ev
evDictNames (EvCast ev _) = evDictNames ev
evDictNames _ = []

hasGivenClass :: Text -> Module -> Bool
hasGivenClass className =
  any (any isGiven . tcAnnEvidenceTerms) . exprAnnotations
  where
    isGiven (EvGiven (ClassPred cls _)) = cls == className
    isGiven (EvDict _ _ evidence) = any isGiven evidence
    isGiven (EvSuperClass ev _) = isGiven ev
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
    EVar name -> nameExprAnnotations name
    EApp fun arg -> exprExprAnnotations fun <> exprExprAnnotations arg
    EInfix lhs op rhs -> exprExprAnnotations lhs <> nameExprAnnotations op <> exprExprAnnotations rhs
    EList elems -> concatMap exprExprAnnotations elems
    ETuple _ elems -> concatMap (maybe [] exprExprAnnotations) elems
    EIf cond thenE elseE -> exprExprAnnotations cond <> exprExprAnnotations thenE <> exprExprAnnotations elseE
    ECase scrut alts -> exprExprAnnotations scrut <> concatMap caseAltExprAnnotations alts
    ELetDecls decls body -> concatMap declExprAnnotations decls <> exprExprAnnotations body
    EListComp body stmts -> exprExprAnnotations body <> concatMap compStmtExprAnnotations stmts
    ELambdaPats _ body -> exprExprAnnotations body
    EParen inner -> exprExprAnnotations inner
    ETypeSig inner _ -> exprExprAnnotations inner
    _ -> []

nameExprAnnotations :: Name -> [TcAnnotation]
nameExprAnnotations name =
  mapMaybe fromAnnotation (nameAnns name)

caseAltExprAnnotations :: CaseAlt Expr -> [TcAnnotation]
caseAltExprAnnotations (CaseAlt _ _ rhs) = rhsExprAnnotations rhs

compStmtExprAnnotations :: CompStmt -> [TcAnnotation]
compStmtExprAnnotations stmt =
  case stmt of
    CompAnn _ inner -> compStmtExprAnnotations inner
    CompGen _ src -> exprExprAnnotations src
    CompGuard guard -> exprExprAnnotations guard
    CompLetDecls decls -> concatMap declExprAnnotations decls
    CompThen expr -> exprExprAnnotations expr
    CompThenBy f byExpr -> exprExprAnnotations f <> exprExprAnnotations byExpr
    CompGroupUsing expr -> exprExprAnnotations expr
    CompGroupByUsing byExpr usingExpr -> exprExprAnnotations byExpr <> exprExprAnnotations usingExpr

-- | Tests for error cases.
errorTests :: [TestTree]
errorTests =
  [ testCase "unbound variable reports error" $ do
      let result = tc "foo"
      assertBool "should not succeed" (not (tcResultSuccess result))
      case tcResultDiagnostics result of
        [] -> assertBool "expected diagnostics" False
        (d : _) -> case diagKind d of
          UnboundVariable name ->
            assertEqual "error should name 'foo'" "foo" name
          other ->
            assertBool ("unexpected error kind: " ++ show other) False
  ]
