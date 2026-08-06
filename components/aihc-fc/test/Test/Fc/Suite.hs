{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for System FC desugaring golden tests.
module Test.Fc.Suite
  ( fcGoldenTests,
    fcDesugarTests,
    fcEvalTests,
    fcEvalFixtureTests,
    fcLoweringTests,
    fcOptimizationTests,
  )
where

import Aihc.Fc
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.Subst (freeRigidTyVarsOf)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax qualified as Surface
import Aihc.Resolve (ResolveResult (..), resolve)
import Aihc.Tc (Kind (..), Levity (..), RuntimeRep (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), tcModuleBindings, tcModuleSuccess, tyConKind, typecheck)
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (mkTyCon)
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import FcGolden
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

-- | Build the golden test tree from fixtures.
fcGoldenTests :: IO TestTree
fcGoldenTests = do
  cases <- loadFcCases
  let tests = map mkTest cases
  pure (testGroup "FC golden tests" tests)

mkTest :: FcCase -> TestTree
mkTest tc = testCase (caseId tc) $ do
  let (outcome, details) = evaluateFcCase tc
  case outcome of
    OutcomePass -> pure ()
    OutcomeXFail -> pure ()
    OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    OutcomeFail -> assertFailure details

fcDesugarTests :: TestTree
fcDesugarTests =
  testGroup
    "FC desugaring"
    [ testCase "counts every label in a grouped record field" $ do
        let (_, parsedModule) = parseModule defaultConfig "module Test where\ndata Pair a = Pair { left, right :: a }\n"
            constructors = concatMap declarationConstructors (Surface.moduleDecls parsedModule)
        case constructors of
          [constructor] -> assertEqual "constructor arity" ("Pair", 2) (dsDataConPure constructor)
          _ -> assertFailure ("expected one parsed constructor, got: " <> show constructors),
      testCase "stock Eq dictionaries pass Core lint" $ do
        let source =
              "{-# LANGUAGE DerivingStrategies #-}\n\
              \module Test where\n\
              \data Bool = False | True\n\
              \class Eq a where\n\
              \  (==) :: a -> a -> Bool\n\
              \  (/=) :: a -> a -> Bool\n\
              \data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving stock Eq\n"
            config = defaultConfig {parserExtensions = [Surface.DerivingStrategies]}
            (parseErrors, parsedModule) = parseModule config source
            result = desugarModule parsedModule
        assertEqual "source parses" [] parseErrors
        assertBool ("desugaring succeeds: " <> show (dsErrors result)) (dsSuccess result)
        assertEqual "Core lint" [] (lintProgram emptyLintEnv (dsProgram result)),
      testCase "qualified imports retain their FC identity and type" $ do
        let sources =
              [ "module Data.List.NonEmpty where\nxor value = value\nlength value = value\ntoList value = value\nindexError value = value\n",
                "module GHC.Bits where\nxor left right = right\n",
                "module Data.Foldable where\nlength left right = right\ntoList left right = right\n",
                "module GHC.Ix where\nindexError left right = right\n",
                "module CollisionRegression where\nimport qualified Data.List.NonEmpty as NonEmpty\nresultXor = NonEmpty.xor 'x'\nresultLength = NonEmpty.length 'l'\nresultToList = NonEmpty.toList 't'\nresultIndexError = NonEmpty.indexError 'i'\n"
              ]
            parsed = map (parseModule defaultConfig) sources
            parseErrors = concatMap fst parsed
        assertEqual "source parses" [] parseErrors
        case resolve (map snd parsed) of
          ResolveResult {resolvedModules, resolveErrors = []} -> do
            let tcResults = typecheck resolvedModules
                allBindings = concatMap tcModuleBindings tcResults
                desugared = zipWith (desugarModuleWithBindings allBindings) tcResults resolvedModules
            assertBool "type checking succeeds" (all tcModuleSuccess tcResults)
            assertBool ("desugaring succeeds: " <> show (concatMap dsErrors desugared)) (all dsSuccess desugared)
            case reverse desugared of
              consumer : _ -> do
                let program = dsProgram consumer
                    rendered = renderProgram program
                assertBool "renders a readable qualifier prefix" ("prefix 1 = \"Data.List.NonEmpty\"" `isInfixOf` rendered)
                assertBool "uses the qualifier prefix in variable labels" ("1.xor" `isInfixOf` rendered)
                assertEqual "declares each free variable's type once" 1 (length (filter (isInfixOf "1.xor :") (lines rendered)))
                assertBool "keeps free variable uses bare" (not ("(1.xor :" `isInfixOf` rendered))
                case parseProgram (T.pack rendered) of
                  Left parseError -> assertFailure ("canonical FC does not parse: " <> parseError)
                  Right reparsed -> assertEqual "canonical FC round-trip" rendered (renderProgram reparsed)
                mapM_
                  (assertQualifiedUnaryBinding program)
                  [ ("resultXor", "xor"),
                    ("resultLength", "length"),
                    ("resultToList", "toList"),
                    ("resultIndexError", "indexError")
                  ]
              [] -> assertFailure "expected a consumer FC program"
          ResolveResult {resolveErrors} -> assertFailure ("resolution failed: " <> show resolveErrors),
      testCase "data declarations supply constructor types to Core text" $ do
        let localType = TcTyCon (TyCon "Local" 0) []
            localConstructor = Var "Box" (Unique 31) localType
            remoteConstructor = (Var "Box" (Unique 32) localType) {varResolvedName = Just "Remote.Box"}
            program =
              FcProgram
                [ FcData "Local" [] [("Box", [])],
                  FcTopBind (FcNonRec (Var "local" (Unique 33) localType) (FcVar localConstructor)),
                  FcTopBind (FcNonRec (Var "remote" (Unique 34) localType) (FcVar remoteConstructor))
                ]
            rendered = renderProgram program
        assertBool "local constructor has no redundant signature" (not ("Test.Box :" `isInfixOf` rendered))
        assertBool "local constructor use is unqualified" ("local : Local =\n  Box;" `isInfixOf` rendered)
        assertBool "same-typed imported constructor keeps its signature" ("1.Box : Local;" `isInfixOf` rendered)
        case parseProgram (T.pack rendered) of
          Left parseError -> assertFailure ("constructor-aware canonical FC does not parse: " <> parseError)
          Right reparsed -> assertEqual "constructor-aware canonical FC round-trip" rendered (renderProgram reparsed),
      testCase "every global reference obtains metadata from one declaration" $ do
        let intType = TcTyCon (TyCon "Int" 0) []
            functionType = TcFunTy intType intType
            argument = Var "x" (Unique 41) intType
            localFunction = Var "fn" (Unique 42) functionType
            importedFirst = (Var "fn" (Unique 43) functionType) {varResolvedName = Just "Remote.fn"}
            importedSecond = (Var "fn" (Unique 44) functionType) {varResolvedName = Just "Remote.fn"}
            primitive = Var "inc#" (Unique 45) functionType
            program =
              FcProgram
                [ FcPrimitive primitive 1,
                  FcTopBind (FcNonRec localFunction (FcLam argument (FcVar argument))),
                  FcTopBind (FcNonRec (Var "useLocal" (Unique 46) functionType) (FcVar localFunction)),
                  FcTopBind (FcNonRec (Var "usePrimitive" (Unique 47) functionType) (FcVar primitive)),
                  FcTopBind (FcNonRec (Var "useImportedFirst" (Unique 48) functionType) (FcVar importedFirst)),
                  FcTopBind (FcNonRec (Var "useImportedSecond" (Unique 49) functionType) (FcVar importedSecond))
                ]
            rendered = renderProgram program
        assertEqual "one imported declaration" 1 (length (filter (isInfixOf "1.fn :") (lines rendered)))
        assertBool "local definition is the only local signature" (not ("Test.fn :" `isInfixOf` rendered))
        assertBool "local use is a bare reference" ("useLocal : Int → Int =\n  fn;" `isInfixOf` rendered)
        assertBool "primitive use is a bare reference" ("usePrimitive : Int → Int =\n  inc#;" `isInfixOf` rendered)
        case parseProgram (T.pack rendered) of
          Left parseError -> assertFailure ("declaration-owned canonical FC does not parse: " <> parseError)
          Right reparsed -> do
            assertEqual "declaration-owned canonical FC round-trip" rendered (renderProgram reparsed)
            case (topVariable "fn" reparsed, topReference "useLocal" reparsed, topVariable "inc#" reparsed, topReference "usePrimitive" reparsed, topReference "useImportedFirst" reparsed, topReference "useImportedSecond" reparsed) of
              (Just parsedLocal, Just parsedLocalUse, Just parsedPrimitive, Just parsedPrimitiveUse, Just parsedImportedFirst, Just parsedImportedSecond) -> do
                assertEqual "local reference resolves to definition" (varUnique parsedLocal) (varUnique parsedLocalUse)
                assertEqual "primitive reference resolves to declaration" (varUnique parsedPrimitive) (varUnique parsedPrimitiveUse)
                assertEqual "external occurrences resolve to one declaration" (varUnique parsedImportedFirst) (varUnique parsedImportedSecond)
                assertEqual "external identity is preserved" (Just "Remote.fn") (varResolvedName parsedImportedFirst)
                assertEqual "external type is recovered" functionType (varType parsedImportedFirst)
              _ -> assertFailure "expected all declaration-owned references after parsing",
      testCase "foreign call sites reference their declaration by name" $ do
        let intType = TcTyCon (TyCon "Int#" 0) []
            foreignCall = FcForeignCall "$ffi$id" "identity" (FcForeignSignature [FcForeignInt] FcForeignInt FcForeignPure)
            argument = Var "x" (Unique 51) intType
            result = Var "result" (Unique 52) intType
            program = FcProgram [FcForeignImport foreignCall, FcTopBind (FcNonRec result (FcCallForeign foreignCall [FcVar argument]))]
            rendered = renderProgram program
        assertEqual "descriptor appears once" 1 (length (filter (isInfixOf "= \"identity\"") (lines rendered)))
        assertBool "call site contains only declaration name and arguments" ("ccall $ffi$id [x]" `isInfixOf` rendered)
        case parseProgram (T.pack rendered) of
          Left parseError -> assertFailure ("foreign-declaration canonical FC does not parse: " <> parseError)
          Right reparsed -> do
            assertEqual "foreign-declaration canonical FC round-trip" rendered (renderProgram reparsed)
            assertEqual "call descriptor is recovered from its declaration" (Just foreignCall) (topForeignCall "result" reparsed)
        let mismatched = foreignCall {fcForeignCallSymbol = "different"}
            invalid = FcProgram [FcForeignImport foreignCall, FcTopBind (FcNonRec result (FcCallForeign mismatched []))]
        assertBool "descriptor disagreement is rejected" (case renderProgramChecked invalid of Left _ -> True; Right _ -> False),
      testCase "newtype heads own their result type and kind" $ do
        let unliftedKind = KTYPE (BoxedRep Unlifted)
            resultType = TcTyCon (mkTyCon "Unlifted" 0 unliftedKind) []
            representation = TcTyCon (TyCon "Int#" 0) []
            declaration = FcNewtypeDecl "Unlifted" [] "MkUnlifted" representation resultType
            rendered = renderProgram (FcProgram [FcNewtype declaration])
        assertBool "result kind is declared on the head" ("newtype Unlifted :: TYPE UnliftedRep = MkUnlifted Int#;" `isInfixOf` rendered)
        assertBool "result application is not repeated" (not ("represents" `isInfixOf` rendered))
        case parseProgram (T.pack rendered) of
          Left parseError -> assertFailure ("newtype-head canonical FC does not parse: " <> parseError)
          Right (FcProgram [FcNewtype reparsed]) -> do
            assertEqual "newtype result type" resultType (fcNewtypeResult reparsed)
            case fcNewtypeResult reparsed of
              TcTyCon tyCon _ -> assertEqual "newtype result kind" unliftedKind (tyConKind tyCon)
              other -> assertFailure ("expected newtype result constructor, got " <> show other)
          Right other -> assertFailure ("expected one reparsed newtype, got " <> show other),
      testCase "prints uniques only where lexical scope needs them" $ do
        let valueTy = TcTyCon (TyCon "Value" 0) []
            firstX = Var "x" (Unique 11) valueTy
            secondX = Var "x" (Unique 12) valueTy
            firstA = TyVarId "a" (Unique 21)
            secondA = TyVarId "a" (Unique 22)
            separateScopes =
              FcProgram
                [ FcTopBind (FcNonRec (Var "left" (Unique 1) (TcFunTy valueTy valueTy)) (FcLam firstX (FcVar firstX))),
                  FcTopBind (FcNonRec (Var "right" (Unique 2) (TcFunTy valueTy valueTy)) (FcLam secondX (FcVar secondX))),
                  FcData "LeftBox" [firstA] [("LeftBox", [TcTyVar firstA])],
                  FcData "RightBox" [secondA] [("RightBox", [TcTyVar secondA])]
                ]
            shadowed =
              FcProgram
                [ FcTopBind
                    ( FcNonRec
                        (Var "useOuter" (Unique 3) (TcFunTy valueTy (TcFunTy valueTy valueTy)))
                        (FcLam firstX (FcLam secondX (FcApp (FcVar firstX) (FcVar secondX))))
                    ),
                  FcAxiom
                    ( FcAxiomDecl
                        "Shadowed"
                        []
                        FcNominal
                        (TcForAllTy firstA (TcForAllTy secondA (TcFunTy (TcTyVar firstA) (TcTyVar secondA))))
                        valueTy
                    )
                ]
            separateText = renderProgram separateScopes
            shadowedText = renderProgram shadowed
        assertBool "disjoint term scopes omit uniques" (not ("x{" `isInfixOf` separateText))
        assertBool "disjoint type scopes omit uniques" (not ("a{" `isInfixOf` separateText))
        assertBool "shadowed outer term binder is disambiguated" ("x{11}" `isInfixOf` shadowedText)
        assertBool "innermost term binder stays plain" (not ("x{12}" `isInfixOf` shadowedText))
        assertBool "shadowed outer type binder is disambiguated" ("a{21}" `isInfixOf` shadowedText)
        assertBool "innermost type binder stays plain" (not ("a{22}" `isInfixOf` shadowedText))
        case parseProgram (T.pack shadowedText) of
          Left parseError -> assertFailure ("scoped canonical FC does not parse: " <> parseError)
          Right reparsed -> assertEqual "scoped canonical FC round-trip" shadowedText (renderProgram reparsed)
    ]
  where
    declarationConstructors declaration =
      case declaration of
        Surface.DeclAnn _ inner -> declarationConstructors inner
        Surface.DeclData dataDeclaration -> Surface.dataDeclConstructors dataDeclaration
        _ -> []

    assertQualifiedUnaryBinding program (bindingName, importedName) =
      case [ expression
           | FcTopBind (FcNonRec binder expression) <- fcTopBinds program,
             varName binder == bindingName
           ] of
        [expression] ->
          case appliedHeadVar expression of
            Just imported -> do
              assertEqual
                ("resolved identity of " <> show bindingName)
                (Just ("Data.List.NonEmpty." <> importedName))
                (varResolvedName imported)
              assertEqual ("term arity of " <> show bindingName) (1 :: Int) (termArity (varType imported))
            Nothing -> assertFailure ("expected " <> show bindingName <> " to apply an imported variable")
        bindings -> assertFailure ("expected one FC binding named " <> show bindingName <> ", got " <> show (length bindings))

    appliedHeadVar expression =
      case expression of
        FcVar variable -> Just variable
        FcApp function _ -> appliedHeadVar function
        FcTyApp function _ -> appliedHeadVar function
        _ -> Nothing

    termArity fcType =
      case fcType of
        TcForAllTy _ body -> termArity body
        TcFunTy _ result -> 1 + termArity result
        _ -> 0

    topVariable name (FcProgram tops) =
      case [ variable
           | top <- tops,
             variable <- case top of
               FcPrimitive primitive _ -> [primitive | varName primitive == name]
               FcTopBind (FcNonRec binder _) -> [binder | varName binder == name]
               FcTopBind (FcRec bindings) -> [binder | (binder, _) <- bindings, varName binder == name]
               _ -> []
           ] of
        [variable] -> Just variable
        _ -> Nothing

    topReference name (FcProgram tops) =
      case [variable | FcTopBind (FcNonRec binder (FcVar variable)) <- tops, varName binder == name] of
        [variable] -> Just variable
        _ -> Nothing

    topForeignCall name (FcProgram tops) =
      case [foreignCall | FcTopBind (FcNonRec binder (FcCallForeign foreignCall _)) <- tops, varName binder == name] of
        [foreignCall] -> Just foreignCall
        _ -> Nothing

fcEvalTests :: TestTree
fcEvalTests =
  testGroup
    "FC evaluator"
    [ testCase "renders string literals" $
        assertEvalExpr "\"hello world\"" (FcLit (LitString "hello world")),
      testCase "renders char literals" $
        assertEvalExpr "'x'#" (FcLit (LitChar WordRep 'x')),
      testCase "renders int literals" $
        assertEvalExpr "42" (FcLit (LitInt IntRep 42)),
      testCase "records Addr# literal type and representation" $ do
        let literal = LitAddr "hello"
        assertEqual "runtime representation" AddrRep (literalRuntimeRep literal)
        assertEqual "literal type" (Just (TcTyCon (TyCon "Addr#" 0) [])) (literalType literal),
      testCase "keeps free rigid variables in first-occurrence order" $ do
        let first = TyVarId "first" (Unique 1)
            second = TyVarId "second" (Unique 2)
        assertEqual "ordered variables" [first, second] (freeRigidTyVarsOf [TcTyVar first, TcTyVar second, TcTyVar first]),
      testCase "applies lambdas" $
        assertEvalExpr
          "\"ok\""
          (FcApp (FcLam (var "x" stringTy) (FcVar (var "x" stringTy))) (FcLit (LitString "ok"))),
      testCase "evaluates top-level bindings" $
        let program =
              FcProgram
                [ FcTopBind
                    (FcNonRec (var "answer" stringTy) (FcLit (LitString "top")))
                ]
         in do
              result <- evalProgramBinding "answer" program >>= renderEvalResult
              assertEqual "result" (Right "\"top\"") result,
      testCase "renders raw constructor values" $ do
        result <-
          renderRawValue
            (VConstructor ":" [VConstructor "C#" [VLit (LitChar WordRep 'x')], VConstructor "[]" []])
        assertEqual
          "raw result"
          (Right ": 'x' []")
          result,
      testCase "evaluates wide Word# multiplication" $ do
        let wordTy = ty "Word#"
            primitive = Var "timesWord2#" (Unique 100) (TcFunTy wordTy (TcFunTy wordTy wordTy))
            result = Var "wideProduct" (Unique 101) wordTy
            expression =
              FcApp
                (FcApp (FcVar primitive) (FcLit (LitInt WordRep 0xffffffffffffffff)))
                (FcLit (LitInt WordRep 2))
            program = FcProgram [FcPrimitive primitive 2, FcTopBind (FcNonRec result expression)]
        actual <- evalProgramBinding "wideProduct" program >>= traverse renderRawValue
        assertEqual "high and low words" (Right (Right "(1,18446744073709551614)")) actual
    ]

fcLoweringTests :: TestTree
fcLoweringTests =
  testGroup
    "FC compulsory lowering"
    [ testCase "expands saturated seq to a case on its first argument" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            second = Var "second" (Unique 2) boolTy
            seqVar = Var "$aihc.seq" (Unique 3) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "result" (Unique 4) boolTy
            source =
              FcProgram
                [ FcTopBind
                    (FcNonRec result (FcApp (FcApp (FcVar seqVar) (FcVar first)) (FcVar second)))
                ]
            caseBinder = Var "$seq_whnf" (Unique 5) stringTy
            expected = FcProgram [FcTopBind (FcNonRec result (Aihc.Fc.FcCase (FcVar first) caseBinder [FcAlt DefaultAlt [] (FcVar second)]))]
        assertEqual "lowered program" expected (lowerPseudoOps source),
      testCase "expands partially applied seq to an explicit lambda" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            seqVar = Var "$aihc.seq" (Unique 2) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "forceFirst" (Unique 3) (TcFunTy boolTy boolTy)
            second = Var "$seq_second" (Unique 4) boolTy
            caseBinder = Var "$seq_whnf" (Unique 5) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcApp (FcVar seqVar) (FcVar first)))]
            expected = FcProgram [FcTopBind (FcNonRec result (FcLam second (Aihc.Fc.FcCase (FcVar first) caseBinder [FcAlt DefaultAlt [] (FcVar second)])))]
        assertEqual "lowered program" expected (lowerPseudoOps source),
      testCase "uses the evaluated case binder for later references to the first argument" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            consume = Var "consume" (Unique 2) (TcFunTy stringTy boolTy)
            seqVar = Var "$aihc.seq" (Unique 3) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "result" (Unique 4) boolTy
            source =
              FcProgram
                [ FcTopBind
                    ( FcNonRec
                        result
                        (FcApp (FcApp (FcVar seqVar) (FcVar first)) (FcApp (FcVar consume) (FcVar first)))
                    )
                ]
            caseBinder = Var "$seq_whnf" (Unique 5) stringTy
            expected =
              FcProgram
                [ FcTopBind
                    ( FcNonRec
                        result
                        (Aihc.Fc.FcCase (FcVar first) caseBinder [FcAlt DefaultAlt [] (FcApp (FcVar consume) (FcVar caseBinder))])
                    )
                ]
        assertEqual "lowered program" expected (lowerPseudoOps source)
    ]

fcOptimizationTests :: TestTree
fcOptimizationTests =
  testGroup
    "FC optional optimizations"
    [ testCase "does not perform compulsory pseudo-op lowering" $ do
        let boolTy = ty "Bool"
            first = Var "first" (Unique 1) stringTy
            second = Var "second" (Unique 2) boolTy
            seqVar = Var "$aihc.seq" (Unique 3) (TcFunTy stringTy (TcFunTy boolTy boolTy))
            result = Var "result" (Unique 4) boolTy
            source =
              FcProgram
                [ FcTopBind
                    (FcNonRec result (FcApp (FcApp (FcVar seqVar) (FcVar first)) (FcVar second)))
                ]
        assertEqual "optimization leaves pseudo-op intact" source (optimizeProgram source),
      testCase "copy propagates simple non-recursive lets" $ do
        let value = Var "value" (Unique 1) stringTy
            alias = Var "alias" (Unique 2) stringTy
            consume = Var "consume" (Unique 3) (TcFunTy stringTy stringTy)
            result = Var "result" (Unique 4) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcNonRec alias (FcVar value)) (FcApp (FcVar consume) (FcVar alias))))]
            expected = FcProgram [FcTopBind (FcNonRec result (FcApp (FcVar consume) (FcVar value)))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "runs the Core optimization set to a fixpoint" $ do
        let value = Var "value" (Unique 5) stringTy
            outer = Var "outer" (Unique 6) stringTy
            inner = Var "inner" (Unique 7) stringTy
            result = Var "result" (Unique 8) stringTy
            source =
              FcProgram
                [ FcTopBind
                    ( FcNonRec
                        result
                        ( FcLet
                            (FcNonRec outer (FcLet (FcNonRec inner (FcVar value)) (FcVar inner)))
                            (FcVar outer)
                        )
                    )
                ]
            expected = FcProgram [FcTopBind (FcNonRec result (FcVar value))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "copy propagates non-recursive singleton rec groups" $ do
        let value = Var "value" (Unique 13) stringTy
            alias = Var "alias" (Unique 14) stringTy
            result = Var "result" (Unique 15) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcRec [(alias, FcVar value)]) (FcVar alias)))]
            expected = FcProgram [FcTopBind (FcNonRec result (FcVar value))]
        assertEqual "optimized program" expected (optimizeProgram source),
      testCase "retains genuinely recursive singleton aliases" $ do
        let alias = Var "alias" (Unique 16) stringTy
            result = Var "result" (Unique 17) stringTy
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcRec [(alias, FcVar alias)]) (FcVar alias)))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "retains non-trivial let right-hand sides" $ do
        let value = Var "value" (Unique 9) stringTy
            binder = Var "computed" (Unique 10) stringTy
            identity = Var "identity" (Unique 11) (TcFunTy stringTy stringTy)
            result = Var "result" (Unique 12) stringTy
            computed = FcApp (FcVar identity) (FcVar value)
            source = FcProgram [FcTopBind (FcNonRec result (FcLet (FcNonRec binder computed) (FcVar binder)))]
        assertEqual "optimized program" source (optimizeProgram source),
      testCase "eliminates values and types unreachable from the entry point" $ do
        let liveTy = ty "Live"
            leafTy = ty "Leaf"
            deadTy = ty "Dead"
            mainVar = Var "main" (Unique 1) liveTy
            helperVar = Var "helper" (Unique 2) liveTy
            deadVar = Var "dead" (Unique 3) deadTy
            liveData = FcData "Live" [] [("Live", [leafTy])]
            leafData = FcData "Leaf" [] [("Leaf", [])]
            deadData = FcData "Dead" [] [("Dead", [])]
            helper = FcTopBind (FcNonRec helperVar (FcVar (Var "Live" (Unique 4) (TcFunTy leafTy liveTy))))
            mainBinding = FcTopBind (FcNonRec mainVar (FcVar helperVar))
            deadBinding = FcTopBind (FcNonRec deadVar (FcVar (Var "Dead" (Unique 5) deadTy)))
            program = FcProgram [deadData, deadBinding, leafData, liveData, helper, mainBinding]
        assertEqual
          "reachable program"
          (FcProgram [leafData, liveData, helper, mainBinding])
          (eliminateDeadCode "main" program),
      testCase "does not confuse a local binder with a top-level definition" $ do
        let valueTy = ty "Value"
            local = Var "shadowed" (Unique 10) valueTy
            global = Var "shadowed" (Unique 11) valueTy
            mainVar = Var "main" (Unique 12) (TcFunTy valueTy valueTy)
            program =
              FcProgram
                [ FcTopBind (FcNonRec global (FcVar global)),
                  FcTopBind (FcNonRec mainVar (FcLam local (FcVar local)))
                ]
        assertEqual
          "reachable program"
          (FcProgram [FcTopBind (FcNonRec mainVar (FcLam local (FcVar local)))])
          (eliminateDeadCode "main" program),
      testCase "retains ordinary dictionary constructor declarations" $ do
        let dictionaryTy = ty "Test"
            dictionaryData = FcData "Test" [] [("$Dict$Test", [stringTy])]
            dictionaryConstructor = Var "$Dict$Test" (Unique 14) (TcFunTy stringTy dictionaryTy)
            dictionaryBinder = Var "$dictionary" (Unique 15) dictionaryTy
            methodBinder = Var "$method" (Unique 16) stringTy
            mainBinding =
              FcTopBind
                ( FcNonRec
                    (Var "main" (Unique 13) stringTy)
                    ( Aihc.Fc.FcCase
                        (FcApp (FcVar dictionaryConstructor) (FcLit (LitString "method")))
                        dictionaryBinder
                        [FcAlt (DataAlt "$Dict$Test") [methodBinder] (FcVar methodBinder)]
                    )
                )
            program = FcProgram [dictionaryData, mainBinding]
        assertEqual
          "reachable dictionary declaration"
          program
          (eliminateDeadCode "main" program)
        assertEqual "Core lint" [] (lintProgram emptyLintEnv program),
      testCase "lowers newtype construction to a linted representational cast" $ do
        let metersTy = ty "Meters"
            intHashTy = ty "Int#"
            declaration =
              FcNewtypeDecl
                { fcNewtypeName = "Meters",
                  fcNewtypeTyVars = [],
                  fcNewtypeConstructor = "Meters",
                  fcNewtypeRepresentation = intHashTy,
                  fcNewtypeResult = metersTy
                }
            constructor = Var "Meters" (Unique 20) (TcFunTy intHashTy metersTy)
            value = Var "value" (Unique 21) metersTy
            source =
              FcProgram
                [ FcNewtype declaration,
                  FcTopBind (FcNonRec value (FcApp (FcVar constructor) (FcLit (LitInt IntRep 42))))
                ]
            lowered = lowerNewtypes source
        assertEqual "idempotent lowering" lowered (lowerNewtypes lowered)
        assertEqual "Core lint" [] (lintProgram emptyLintEnv lowered)
        result <- evalProgramBinding "value" lowered >>= renderEvalResult
        assertEqual "runtime representation" (Right "42") result,
      testCase "lowers dependency newtypes without merging separate units" $ do
        let wrapperTy = ty "Wrapper"
            intHashTy = ty "Int#"
            declaration =
              FcNewtypeDecl
                { fcNewtypeName = "Wrapper",
                  fcNewtypeTyVars = [],
                  fcNewtypeConstructor = "Wrap",
                  fcNewtypeRepresentation = intHashTy,
                  fcNewtypeResult = wrapperTy
                }
            constructor = Var "Wrap" (Unique 30) (TcFunTy intHashTy wrapperTy)
            value = Var "value" (Unique 31) wrapperTy
            literal = FcLit (LitInt IntRep 42)
            provider = FcProgram [FcNewtype declaration]
            consumer = FcProgram [FcTopBind (FcNonRec value (FcApp (FcVar constructor) literal))]
            loweredConsumer = FcProgram [FcTopBind (FcNonRec value (FcCast literal (Sym (AxiomInstCo "Wrapper" []))))]
        assertEqual
          "consumer body"
          loweredConsumer
          (lowerNewtypesWithInterface (extractNewtypeInterface provider) consumer)
        assertEqual
          "consumer lint"
          []
          (lintProgramWithAxiomInterface (extractAxiomInterface provider) emptyLintEnv loweredConsumer),
      testCase "lints explicit equality axioms" $ do
        let familyTy = ty "Family"
            representationTy = ty "Int#"
            declaration =
              FcAxiomDecl
                { fcAxiomName = "axFamily",
                  fcAxiomTyVars = [],
                  fcAxiomRole = FcNominal,
                  fcAxiomLeft = familyTy,
                  fcAxiomRight = representationTy
                }
            value = Var "main" (Unique 40) familyTy
            binding = FcTopBind (FcNonRec value (FcCast (FcLit (LitInt IntRep 42)) (Sym (AxiomInstCo "axFamily" []))))
            program = FcProgram [FcAxiom declaration, binding]
        assertEqual "Core lint" [] (lintProgram emptyLintEnv program)
        assertEqual "reachable axiom" program (eliminateDeadCode "main" program),
      testCase "imports equality axioms across compilation units" $ do
        let parameter = TyVarId "a" (Unique 42)
            representationTy = ty "Int#"
            familyTy argument = TcTyCon (TyCon "Family" 1) [argument]
            declaration =
              FcAxiomDecl
                { fcAxiomName = "axFamily",
                  fcAxiomTyVars = [parameter],
                  fcAxiomRole = FcRepresentational,
                  fcAxiomLeft = familyTy (TcTyVar parameter),
                  fcAxiomRight = TcTyVar parameter
                }
            value = Var "main" (Unique 41) (familyTy representationTy)
            provider = FcProgram [FcAxiom declaration]
            consumer = FcProgram [FcTopBind (FcNonRec value (FcCast (FcLit (LitInt IntRep 42)) (Sym (AxiomInstCo "axFamily" [representationTy]))))]
            wrongArity = FcProgram [FcTopBind (FcNonRec value (FcCast (FcLit (LitInt IntRep 42)) (Sym (AxiomInstCo "axFamily" []))))]
            interface = extractAxiomInterface provider
        assertBool "consumer needs imported axiom" (not (null (lintProgram emptyLintEnv consumer)))
        assertBool "axiom arity is checked" (not (null (lintProgramWithAxiomInterface interface emptyLintEnv wrongArity)))
        assertEqual "consumer lint" [] (lintProgramWithAxiomInterface interface emptyLintEnv consumer)
        assertEqual "serialized interface" interface (read (show interface))
    ]

fcEvalFixtureTests :: IO TestTree
fcEvalFixtureTests = do
  cases <- EvalGolden.loadEvalCases
  let tests = map mkEvalFixtureTest cases
  pure (testGroup "shared evaluation fixtures via FC" tests)

mkEvalFixtureTest :: EvalGolden.EvalCase -> TestTree
mkEvalFixtureTest tc = testCase (EvalGolden.evalCaseId tc) $ do
  (outcome, details) <- EvalGolden.evaluateEvalCase evaluateFcProgram tc
  case outcome of
    EvalGolden.OutcomePass -> pure ()
    EvalGolden.OutcomeXFail -> pure ()
    EvalGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    EvalGolden.OutcomeFail -> assertFailure details

evaluateFcProgram :: Text -> FcProgram -> IO (Either String Text)
evaluateFcProgram name program = do
  result <- evalProgramBinding name program
  case result of
    Left err -> pure (Left (show err))
    Right value -> do
      rendered <- renderRawValue value
      pure $
        case rendered of
          Left err -> Left (show err)
          Right text -> Right text

assertEvalExpr :: Text -> FcExpr -> IO ()
assertEvalExpr expected expr = do
  result <- evalExpr expr >>= renderEvalResult
  assertEqual "result" (Right expected) result

renderEvalResult :: Either EvalError Value -> IO (Either EvalError Text)
renderEvalResult result =
  case result of
    Left err -> pure (Left err)
    Right value -> renderValue value

var :: Text -> TcType -> Var
var name = Var name (Unique 0)

stringTy :: TcType
stringTy = TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []]

ty :: Text -> TcType
ty name = TcTyCon (TyCon name 0) []
