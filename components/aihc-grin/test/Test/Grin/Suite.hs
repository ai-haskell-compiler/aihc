{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Suite
  ( grinUnitTests,
    grinGoldenTests,
    grinEvalFixtureTests,
  )
where

import Aihc.Fc.Newtype (extractNewtypeInterface, lowerNewtypes, lowerNewtypesWithInterface)
import Aihc.Fc.Syntax
import Aihc.Grin
import Aihc.Tc (Levity (..), RuntimeRep (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), runtimeRepOfType)
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Testing.EvalFixture qualified as EvalGolden
import Control.Monad (forM_)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GrinGolden qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)

grinUnitTests :: TestTree
grinUnitTests =
  testGroup
    "GRIN"
    [ testCase "pretty printer parenthesizes compound applications" $ do
        let first = GrinVar "$grin_tuple_2720" 2720 (BoxedRep Lifted)
            second = GrinVar "$grin_tuple_2721" 2721 (BoxedRep Lifted)
            function =
              GrinFunction
                { grinFunctionName = FunctionName "$grin_thunk_4",
                  grinFunctionLinkName = Nothing,
                  grinFunctionParameters = [first, second],
                  grinFunctionResultRep = BoxedRep Lifted,
                  grinFunctionBody = GrinApply (BoxedRep Lifted) (GrinVarValue first) [GrinVarValue second]
                }
            program =
              GrinProgram
                { grinConstructors = [],
                  grinPrimitives = [],
                  grinForeignCalls = [],
                  grinExternalGlobals = [],
                  grinExternalFunctions = [],
                  grinWhnfGlobals = [],
                  grinCafs = [],
                  grinFunctions = [function]
                }
        assertEqual
          "rendered program"
          ( unlines
              [ "$grin_thunk_4 ($grin_tuple_2720%2720 :: BoxedRep Lifted) ($grin_tuple_2721%2721 :: BoxedRep Lifted) -> BoxedRep Lifted =",
                "  apply @(BoxedRep Lifted) ($grin_tuple_2720%2720 :: BoxedRep Lifted) ($grin_tuple_2721%2721 :: BoxedRep Lifted)"
              ]
          )
          (renderProgram program <> "\n"),
      testCase "FC lowering passes known WHNF arguments without thunk cells" $ do
        let program = lowerProgram applicationProgram
            rendered = renderProgram program
            storedNodes = concatMap (expressionStoredNodes . grinFunctionBody) (grinFunctions program)
        assertEqual "lint" [] (lintProgram program)
        assertBool "does not allocate a constructor argument thunk" (not (any isThunkNode storedNodes))
        assertBool "contains explicit application" ("apply " `isInfixOf` rendered),
      testCase "FC lowering evaluates case and apply operands explicitly" $ do
        let caseProgram = lowerProgram dictionaryProgram
            applyProgram = lowerProgram applicationProgram
        assertEqual "case lint" [] (lintProgram caseProgram)
        assertEqual "apply lint" [] (lintProgram applyProgram)
        assertBool "case operand is produced by eval" (any (containsExplicitOperand GrinCaseOperand) (grinFunctions caseProgram))
        assertBool "apply operand is produced by eval" (any (containsExplicitOperand GrinApplyOperand) (grinFunctions applyProgram)),
      testCase "interpreter case does not evaluate its operand" $ do
        result <- interpretProgramBinding "answer" implicitCaseEvaluationProgram
        case result of
          Left (InterpretNoMatchingAlternative RuntimeLocation {}) -> pure ()
          other -> assertFailure ("expected a non-matching heap pointer, got " <> show other),
      testCase "interpreter apply does not evaluate its operand" $ do
        result <- interpretProgramBinding "answer" implicitApplyEvaluationProgram
        case result of
          Left (InterpretApplyNonFunction RuntimeLocation {}) -> pure ()
          other -> assertFailure ("expected a non-function heap pointer, got " <> show other),
      testCase "interpreter eval preserves a stored WHNF pointer" $ do
        result <- interpretProgramFunctionSnapshot storedWhnfEntry storedWhnfEvalProgram
        assertEqual
          "snapshot"
          (Right "return: @0\nheap:\n  @0 = CBox\n")
          (renderHeapSnapshot <$> result),
      testCase "interpreter represents a static WHNF as a heap pointer" $ do
        result <- interpretProgramFunctionSnapshot staticWhnfEntry staticWhnfProgram
        assertEqual
          "snapshot"
          (Right "return: @0\nheap:\n  @0 = CBox\n")
          (renderHeapSnapshot <$> result),
      testCase "interpreter applies a stored WHNF closure" $ do
        result <- interpretProgramFunctionSnapshot storedClosureEntry storedClosureApplyProgram
        assertEqual
          "snapshot"
          (Right "return: @0\nheap:\n  @0 = CBox\n")
          (renderHeapSnapshot <$> result),
      testCase "interpreter matches a stored WHNF constructor" $ do
        result <- interpretProgramFunctionSnapshot storedWhnfCaseEntry storedWhnfCaseProgram
        assertEqual
          "snapshot"
          (Right "return: @0\nheap:\n  @0 = CBox\n")
          (renderHeapSnapshot <$> result),
      testCase "interpreter implements fetch and update" $ do
        result <- interpretProgramBinding "answer" heapProgram
        assertEqual "result" (Right "Box 2") result,
      testCase "interpreter rejects unlifted thunk results" $ do
        result <- interpretProgramBinding "bad" (unliftedHeapProgram IntRep)
        assertEqual "result" (Left (InterpretInvalidThunkResultRep unliftedThunkFunction IntRep)) result,
      testCase "interpreter rejects unlifted heap updates" $ do
        result <- interpretProgramBinding "answer" invalidUpdateProgram
        assertEqual "result" (Left (InterpretInvalidUpdateValue (RuntimeLit (GrinLitInt IntRep 2)))) result,
      testCase "direct interpreter rejects CPS-only expressions" $
        forM_ cpsOnlyExpressions $ \expression -> do
          result <- interpretProgramFunctionSnapshot cpsOnlyFunction (cpsOnlyProgram expression)
          assertEqual (show expression) (Left (InterpretCpsExpression expression)) result,
      testCase "lint rejects unlifted thunk results and heap updates" $
        forM_ unliftedRuntimeReps $ \runtimeRep ->
          let errors = lintProgram (unliftedHeapProgram runtimeRep)
           in do
                assertBool
                  ("accepted thunk returning " <> show runtimeRep)
                  (GrinLintThunkResult unliftedThunkFunction runtimeRep `elem` errors)
                assertBool
                  ("accepted heap update with " <> show runtimeRep)
                  (GrinLintUpdateNonLifted runtimeRep `elem` errors),
      testCase "lint rejects constructor field representation mismatches" $ do
        let program = heapProgram {grinConstructors = [("Box", [[WordRep]])]}
        assertBool
          "constructor layout mismatch"
          (GrinLintConstructorLayout "Box" [WordRep] [IntRep] `elem` lintProgram program),
      testCase "FC lowering makes exception control explicit" $ do
        let program = lowerProgram exceptionProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains explicit throw" ("throw " `isInfixOf` rendered)
        assertBool "contains explicit catch" ("catch " `isInfixOf` rendered),
      testCase "CPS-GRIN allocates and applies ordinary continuation closures" $ do
        cps <- expectCpsGrin callBindProgram
        let program = cpsGrinProgram cps
            rendered = renderProgram program
        assertEqual "transformed lint" [] (lintProgram program)
        assertBool "allocates a continuation closure" ("store (P$cps$" `isInfixOf` rendered)
        assertBool "invokes a continuation closure" ("continue ($cps_return" `isInfixOf` rendered)
        assertBool "passes an explicit continuation to the call" (any callEndsInContinuation (grinFunctions program))
        assertBool "generated continuation captures its environment" (any continuationHasCaptures (grinFunctions program)),
      testCase "CPS-GRIN treats a multi-value result as one logical argument" $ do
        cps <- expectCpsGrin multiValueContinuationProgram
        case grinFunctions (cpsGrinProgram cps) of
          function : _ ->
            case grinFunctionBody function of
              GrinBind _ (GrinStore (GrinNode (GrinClosure _ layouts) _)) _ ->
                assertEqual "one multi-value argument layout" [[IntRep, WordRep]] layouts
              body -> assertFailure ("expected a stored continuation closure, got " <> show body)
          [] -> assertFailure "expected transformed functions",
      testCase "CPS-GRIN reifies only transferring binds" $ do
        forM_ transferringExpressions $ \valueExpression -> do
          let source = singleBindProgram valueExpression
          cps <- expectCpsGrin source
          assertEqual (show valueExpression) 1 (Set.size (cpsContinuationFunctions cps `Set.difference` Set.singleton (cpsUpdateFunction cps)))
        primitiveCps <- expectCpsGrin (singleBindProgram (GrinPrimitiveCall (BoxedRep Lifted) "primitive" []))
        assertEqual
          "primitive calls stay direct"
          Set.empty
          (cpsContinuationFunctions primitiveCps `Set.difference` Set.singleton (cpsUpdateFunction primitiveCps)),
      testCase "CPS-GRIN reifies scheduler primitive continuations" $ do
        forM_ ["fork#", "yield#"] $ \name -> do
          cps <- expectCpsGrin (controlPrimitiveBindProgram name)
          let program = cpsGrinProgram cps
          assertEqual "transformed lint" [] (lintProgram program)
          assertBool ("missing CPS primitive " <> T.unpack name) (any (containsCpsPrimitive name . grinFunctionBody) (grinFunctions program))
          assertEqual
            "one reified continuation"
            1
            (Set.size (cpsContinuationFunctions cps `Set.difference` Set.singleton (cpsUpdateFunction cps))),
      testCase "CPS-GRIN keeps non-call binds in direct style" $ do
        cps <- expectCpsGrin directBindProgram
        let rendered = renderProgram (cpsGrinProgram cps)
        assertBool "constant bind remains direct" ("constant" `isInfixOf` rendered)
        assertBool "store bind remains direct" ("store (CBox" `isInfixOf` rendered)
        assertEqual
          "case does not allocate a continuation"
          Set.empty
          (cpsContinuationFunctions cps `Set.difference` Set.singleton (cpsUpdateFunction cps)),
      testCase "GC lowering returns relocated live pointer roots in SSA" $ do
        cps <- expectCpsGrin gcRootProgram
        let gc = lowerGc cps
            gcProgram = gcGrinProgram gc
            reservations = concatMap (ensureHeapReservations . grinFunctionBody) (grinFunctions gcProgram)
            roots = map snd reservations
            rendered = renderProgram gcProgram
        assertEqual "preserves continuation entries" (cpsContinuationFunctions cps) (gcContinuationFunctions gc)
        assertEqual "preserves computation continuations" (cpsFunctionContinuations cps) (gcFunctionContinuations gc)
        assertEqual "preserves update entry" (cpsUpdateFunction cps) (gcUpdateFunction gc)
        assertEqual "GC-GRIN lint" [] (lintProgram gcProgram)
        assertBool "contains an explicit reservation" (not (null reservations))
        assertBool "uses a one-word header for a one-field node" (2 `elem` map fst reservations)
        assertBool "roots contain only pointer representations" (all (all (isPointerRuntimeRep . grinValueRuntimeRep)) roots)
        assertBool "renames relocated roots" ("$gc" `isInfixOf` rendered)
        assertBool "uses unchecked stores" ("store-unchecked " `isInfixOf` rendered)
        assertBool "eliminates checked function stores" (not ("\n  store " `isInfixOf` rendered)),
      testCase "CPS-GRIN gives every computation entry a return continuation" $ do
        cps <- expectCpsGrin callBindProgram
        forM_ (Map.toList (cpsFunctionContinuations cps)) $ \(name, continuation) ->
          case [function | function <- grinFunctions (cpsGrinProgram cps), grinFunctionName function == name] of
            [function] ->
              assertEqual "hidden final parameter" (Just continuation) (lastMaybe (grinFunctionParameters function))
            _ -> assertFailure ("missing computation entry " <> show name),
      testCase "CPS-GRIN makes thunk update an explicit continuation" $ do
        cps <- expectCpsGrin heapProgram
        let program = cpsGrinProgram cps
            updateName = cpsUpdateFunction cps
        case [function | function <- grinFunctions program, grinFunctionName function == updateName] of
          [function] -> do
            assertBool "updates only a blackhole" (containsUpdateBlackhole (grinFunctionBody function))
            assertBool "re-forces the updated result" (containsCpsEval (grinFunctionBody function))
          _ -> assertFailure "missing unique update continuation",
      testCase "CPS-GRIN requires exception control to be eliminated" $ do
        assertEqual
          "throw"
          (Left (CpsGrinUnexpectedThrow exceptionBoundaryFunction))
          (toCpsGrin (exceptionBoundaryProgram (GrinThrow (GrinLitValue (GrinLitInt IntRep 1)))))
        assertEqual
          "catch"
          (Left (CpsGrinUnexpectedCatch exceptionBoundaryFunction))
          (toCpsGrin (exceptionBoundaryProgram (GrinCatch IntRep exceptionAction exceptionHandler []))),
      testCase "FC lowering evaluates Int# arguments without allocating thunks" $ do
        let program = lowerProgram unboxedApplicationProgram
            rendered = renderProgram program
            storedNodes = concatMap (expressionStoredNodes . grinFunctionBody) (grinFunctions program)
        assertEqual "lint" [] (lintProgram program)
        assertBool "records IntRep" ("IntRep" `isInfixOf` rendered)
        assertBool "does not allocate an argument thunk" (not (any isThunkNode storedNodes)),
      testCase "FC lowering calls saturated primitives without global slots" $ do
        let program = lowerProgram primitiveCallProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains a direct primitive call" ("primitive-call @IntRep +#" `isInfixOf` rendered)
        assertBool "primitive is not global" (not ("global +#" `isInfixOf` rendered)),
      testCase "FC lowering wraps partially applied primitives in ordinary closures" $ do
        let program = lowerProgram partialPrimitiveProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "allocates a provenance-named ordinary closure" ("PmakeAdder_primitive_" `isInfixOf` rendered)
        assertBool "wrapper makes a saturated primitive call" ("primitive-call @IntRep +#" `isInfixOf` rendered),
      testCase "FC lowering counts zero-width arguments in closure arity" $ do
        let program = lowerProgram zeroWidthSaturatedApplicationProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "one logical argument remains" ("P$entry$zeroWidthTarget/1" `isInfixOf` rendered)
        assertBool "closure is not saturated" (not ("P$entry$zeroWidthTarget/0" `isInfixOf` rendered)),
      testCase "FC lowering preserves a zero-width application" $ do
        let program = lowerProgram zeroWidthUnknownApplicationProgram
        assertEqual "lint" [] (lintProgram program)
        case grinFunctions program of
          [function] ->
            case grinFunctionBody function of
              GrinBind [evaluated] (GrinEval _ _) (GrinApply _ (GrinVarValue applied) argumentValues) -> do
                assertEqual "applies the explicitly evaluated function" evaluated applied
                assertEqual "zero runtime values" [] argumentValues
              body -> assertFailure ("expected an explicit evaluation followed by application, got " <> show body)
          functions -> assertFailure ("expected one function, got " <> show (length functions)),
      testCase "interpreter consumes one logical zero-width argument" $ do
        assertEqual "lint" [] (lintProgram zeroWidthApplyProgram)
        result <- interpretProgramBinding "answer" zeroWidthApplyProgram
        assertEqual "result" (Right "Box 1") result,
      testCase "FC lowering counts zero-width constructor fields" $ do
        let program = lowerProgram zeroWidthConstructorProgram
        assertEqual "lint" [] (lintProgram program)
        assertEqual "field layouts" [("StateBox", [[], [BoxedRep Lifted]])] (grinConstructors program)
        case grinFunctions program of
          [function] ->
            assertEqual
              "one logical field remains"
              (GrinStore (GrinNode (GrinConstructor "StateBox" 1) []))
              (grinFunctionBody function)
          functions -> assertFailure ("expected one function, got " <> show (length functions)),
      testCase "lint rejects saturated closure nodes" $ do
        let target = FunctionName "target"
            wrapper = FunctionName "wrapper"
            program =
              heapProgram
                { grinCafs = [],
                  grinFunctions =
                    [ GrinFunction
                        { grinFunctionName = target,
                          grinFunctionLinkName = Nothing,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = BoxedRep Lifted,
                          grinFunctionBody = GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)])
                        },
                      GrinFunction
                        { grinFunctionName = wrapper,
                          grinFunctionLinkName = Nothing,
                          grinFunctionParameters = [],
                          grinFunctionResultRep = BoxedRep Lifted,
                          grinFunctionBody = GrinStore (GrinNode (GrinClosure target []) [])
                        }
                    ]
                }
        assertBool "saturated closure" (GrinLintSaturatedClosure target `elem` lintProgram program),
      testCase "FC lowering stores saturated constructor nodes" $ do
        let program = lowerProgram saturatedConstructorProgram
        assertEqual "lint" [] (lintProgram program)
        case grinFunctions program of
          [function] ->
            assertEqual
              "constructor body"
              (GrinStore (GrinNode (GrinConstructor "I32#" 0) [GrinVarValue (GrinVar "value" 62 Int32Rep)]))
              (grinFunctionBody function)
          functions -> assertFailure ("expected one constructor function, got " <> show (length functions)),
      testCase "FC lowering returns a tail constructor store directly" $ do
        let program = lowerProgram tailStoredTupleProgram
        assertEqual "lint" [] (lintProgram program)
        case grinFunctions program of
          [function] ->
            assertEqual
              "function body"
              (GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinVarValue (GrinVar "value" 81 IntRep)]))
              (grinFunctionBody function)
          functions -> assertFailure ("expected one constructor function, got " <> show (length functions)),
      testCase "FC lowering emits saturated strict foreign calls" $ do
        let program = lowerProgram foreignProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "contains a direct foreign call" ("foreign-call $ffi$abs" `isInfixOf` rendered)
        assertBool "does not apply a foreign function value" (not ("apply @Int32Rep $ffi$abs" `isInfixOf` rendered)),
      testCase "lint rejects undersaturated foreign calls" $ do
        let program = lowerProgram undersaturatedForeignProgram
        assertBool
          "foreign arity mismatch"
          (GrinLintForeignArity "$ffi$abs" 1 0 `elem` lintProgram program),
      testCase "FC lowering distinguishes shadowing locals from globals" $ do
        let program = lowerProgram shadowingProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "raw local is emitted as a constant" ("constant (answer%2 :: IntRep)" `isInfixOf` rendered)
        assertBool "raw local is not treated as a global cell" (not ("eval @IntRep (answer%2" `isInfixOf` rendered)),
      testCase "FC lowering still evaluates CAF references" $ do
        let program = lowerProgram cafReferenceProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool "CAF reference is evaluated" ("eval @(BoxedRep Lifted) (source%" `isInfixOf` rendered),
      testCase "FC lowering initializes static constructor values without CAF cells" $ do
        let program = lowerProgram staticConstructorProgram
        assertEqual "lint" [] (lintProgram program)
        assertEqual "static globals" ["source"] (map (grinVarName . fst) (grinWhnfGlobals program))
        assertEqual "no CAFs" [] (grinCafs program),
      testCase "FC lowering only makes computed function values CAFs" $ do
        let program = lowerProgram functionClassificationProgram
        assertEqual "lint" [] (lintProgram program)
        assertEqual "direct functions are not globals" [] (map (grinVarName . fst) (grinWhnfGlobals program))
        assertEqual "computed function CAFs" ["computed"] (map (grinVarName . fst) (grinCafs program)),
      testCase "FC lowering flattens top-level lambdas into one exported entry" $ do
        let program = lowerProgram functionClassificationProgram
            exported = [function | function <- grinFunctions program, grinFunctionLinkName function == Just "direct"]
        case exported of
          [function] -> assertEqual "runtime parameters" 2 (length (grinFunctionParameters function))
          _ -> assertFailure ("expected one exported direct entry, got " <> show (length exported)),
      testCase "FC lowering names CAF thunks after their source binding" $ do
        let program = lowerProgram etaExpandedKnownFunctionProgram
        case grinCafs program of
          [(caf, GrinNode (GrinThunk thunkName) [])] -> do
            assertEqual "CAF name" "$fApplicativeIO" (grinVarName caf)
            assertEqual "source-derived thunk name" (FunctionName "$fApplicativeIO_thunk") thunkName
          cafs -> assertFailure ("expected one capture-free CAF thunk, got " <> show cafs),
      testCase "FC lowering eliminates known-function eta wrappers" $ do
        let program = lowerProgram etaExpandedKnownFunctionProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertBool
          "stores the original function with all arguments remaining"
          ("store (P$entry$pureIO/2)" `isInfixOf` rendered)
        assertEqual
          "emits no eta-wrapper entry"
          [FunctionName "$entry$pureIO", FunctionName "$fApplicativeIO_thunk"]
          (map grinFunctionName (grinFunctions program)),
      testCase "FC lowering retains known-function lambdas that are not eta expansions" $ do
        let program = lowerProgram nonEtaExpandedKnownFunctionProgram
            generatedNames = map (unFunctionName . grinFunctionName) (grinFunctions program)
        assertEqual "lint" [] (lintProgram program)
        assertBool "retains the argument-changing wrapper" (any ("$fApplicativeIO_closure_" `T.isPrefixOf`) generatedNames),
      testCase "FC lowering names local helpers after their Core provenance" $ do
        let program = lowerProgram provenanceNamingProgram
            generatedNames = map (unFunctionName . grinFunctionName) (grinFunctions program)
        assertEqual "lint" [] (lintProgram program)
        assertBool "names the returned closure after its owner" (any ("owner_closure_" `T.isPrefixOf`) generatedNames)
        assertBool "names the local thunk after its binder" (any ("delayed_thunk_" `T.isPrefixOf`) generatedNames)
        assertBool "names helpers inside the thunk after its binder" (any ("delayed_closure_" `T.isPrefixOf`) generatedNames)
        assertBool "emits no anonymous closure names" (not (any (T.isPrefixOf "$grin_closure_") generatedNames))
        assertBool "emits no anonymous thunk names" (not (any (T.isPrefixOf "$grin_thunk_") generatedNames)),
      testCase "FC lowering flattens lambdas hidden by cast aliases" $ do
        let program = lowerProgram castAliasClosureProgram
            closureFunctions = [function | function <- grinFunctions program, "dictionary_closure_" `T.isPrefixOf` unFunctionName (grinFunctionName function)]
            storedClosures =
              [ layouts
              | function <- grinFunctions program,
                GrinNode (GrinClosure _ layouts) _ <- expressionStoredNodes (grinFunctionBody function)
              ]
        assertEqual "lint" [] (lintProgram program)
        assertEqual "one flattened closure entry" 1 (length closureFunctions)
        assertEqual "two logical arguments" [2] (map length storedClosures),
      testCase "FC lowering preserves computed-let closure boundaries" $ do
        let program = lowerProgram computedLetClosureProgram
            closureFunctions = [function | function <- grinFunctions program, "dictionary_closure_" `T.isPrefixOf` unFunctionName (grinFunctionName function)]
        assertEqual "lint" [] (lintProgram program)
        assertEqual "two closure entries around computed work" 2 (length closureFunctions),
      testCase "FC lowering avoids outlining an immediately forced and discarded let" $ do
        let program = lowerProgram immediatelyForcedLetProgram
            ownerFunctions = [function | function <- grinFunctions program, grinFunctionLinkName function == Just "forceOnce"]
            thunkNames = [grinFunctionName function | function <- grinFunctions program, "delayed_thunk_" `T.isPrefixOf` unFunctionName (grinFunctionName function)]
        assertEqual "lint" [] (lintProgram program)
        case (ownerFunctions, thunkNames) of
          ([_], []) -> pure ()
          _ -> assertFailure ("expected one owner and no local thunk entry, got " <> show (length ownerFunctions, length thunkNames)),
      testCase "FC lowering retains an immediately forced thunk cell when it remains shared" $ do
        let program = lowerProgram sharedForcedLetProgram
            ownerFunctions = [function | function <- grinFunctions program, grinFunctionLinkName function == Just "forceShared"]
            thunkNames = [grinFunctionName function | function <- grinFunctions program, "delayed_thunk_" `T.isPrefixOf` unFunctionName (grinFunctionName function)]
        assertEqual "lint" [] (lintProgram program)
        case (ownerFunctions, thunkNames) of
          ([owner], [thunkName]) -> do
            assertBool "retains the shared thunk cell" (any (isNamedThunkNode thunkName) (expressionStoredNodes (grinFunctionBody owner)))
            assertBool "does not bypass the shared cell" (thunkName `notElem` expressionCalledFunctions (grinFunctionBody owner))
          _ -> assertFailure ("expected one owner and one local thunk entry, got " <> show (length ownerFunctions, length thunkNames)),
      testCase "recursive function bindings store closures without thunk wrappers" $ do
        let program = lowerProgram recursiveFunctionProgram
            recursiveNodes =
              [ node
              | function <- grinFunctions program,
                GrinStoreRec bindings _ <- [grinFunctionBody function],
                (_, node) <- bindings
              ]
        assertEqual "lint" [] (lintProgram program)
        case recursiveNodes of
          [GrinNode GrinClosure {} _] -> pure ()
          nodes -> assertFailure ("expected one recursive closure node, got " <> show nodes)
        result <- interpretProgramBinding "answer" program
        assertEqual "result" (Right "<function>") result,
      testCase "FC dictionaries lower to ordinary constructor nodes and cases" $ do
        let program = lowerProgram dictionaryProgram
            rendered = renderProgram program
        assertEqual "lint" [] (lintProgram program)
        assertEqual
          "dictionary constructor has an ordinary declared layout"
          [("Box", [[IntRep]]), ("$Dict$Test", [[BoxedRep Lifted], [BoxedRep Lifted]])]
          (grinConstructors program)
        assertBool ("explicit dictionary constructor store:\n" <> rendered) ("store (C$Dict$Test" `isInfixOf` rendered)
        assertBool "ordinary dictionary case" ("$Dict$Test" `isInfixOf` rendered && "case " `isInfixOf` rendered)
        assertBool "no tuple encoding" (not ("C(,)" `isInfixOf` rendered || "C()" `isInfixOf` rendered))
        assertBool "no projection operation" (not ("project " `isInfixOf` rendered))
        result <- interpretProgramBinding "answer" program
        assertEqual "selected field is evaluated" (Right "Box 2") result,
      testCase "separate FC units do not capture dependency globals" $ do
        case separatePrograms of
          [providerCore, consumerCore] -> do
            let consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
                parameters = concatMap grinFunctionParameters (grinFunctions consumer)
            assertBool "dependency CAF remains global" (all ((/= "source") . grinVarName) parameters)
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs)),
      testCase "separate FC units call dependency code entries directly" $ do
        case separateFunctionPrograms of
          [providerCore, consumerCore] -> do
            let consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
                rendered = renderProgram consumer
            assertEqual "lint" [] (lintProgram consumer)
            assertEqual "external code" ["identity"] (map grinCodeSourceName (grinExternalFunctions consumer))
            assertBool "direct dependency call" ("call @(BoxedRep Lifted) $entry$identity" `isInfixOf` rendered)
            assertBool "dependency function has no global slot" (not ("global identity" `isInfixOf` rendered))
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs)),
      testCase "separate FC units store saturated dependency constructors directly" $ do
        case separateConstructorPrograms of
          [providerCore, consumerCore] -> do
            let consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
            case grinFunctions consumer of
              [function] ->
                assertEqual
                  "constructor body"
                  (GrinStore (GrinNode (GrinConstructor "I32#" 0) [GrinVarValue (GrinVar "value" 72 Int32Rep)]))
                  (grinFunctionBody function)
              functions -> assertFailure ("expected one constructor function, got " <> show (length functions))
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs)),
      testCase "separate FC units erase dependency newtypes" $ do
        case separateNewtypePrograms of
          [providerCore, sourceConsumer] -> do
            let consumerCore = lowerNewtypesWithInterface (extractNewtypeInterface providerCore) sourceConsumer
                provider = lowerProgram providerCore
                consumer = lowerProgramWithInterface (extractGrinInterface providerCore) consumerCore
            assertEqual "newtype declaration emits no constructor" [] (grinConstructors provider)
            assertEqual "consumer lint" [] (lintProgram consumer)
            assertBool "newtype constructor is erased across units" (not ("Wrap" `isInfixOf` renderProgram consumer))
          programs -> assertFailure ("expected two FC programs, got " <> show (length programs))
    ]

expectCpsGrin :: GrinProgram -> IO CpsGrinProgram
expectCpsGrin program =
  case toCpsGrin program of
    Left err -> assertFailure ("expected CPS-GRIN conversion to succeed, got " <> show err)
    Right cps -> pure cps

ensureHeapReservations :: GrinExpr -> [(Int, [GrinValue])]
ensureHeapReservations expression =
  case expression of
    GrinBind _ valueExpression body -> ensureHeapReservations valueExpression <> ensureHeapReservations body
    GrinEnsureHeap requiredWords roots -> [(requiredWords, roots)]
    GrinStoreRec _ body -> ensureHeapReservations body
    GrinStoreRecUnchecked _ body -> ensureHeapReservations body
    GrinCase _ _ alternatives -> concatMap (ensureHeapReservations . grinAltRhs) alternatives
    _ -> []

expressionStoredNodes :: GrinExpr -> [GrinNode]
expressionStoredNodes expression =
  case expression of
    GrinBind _ valueExpression body -> expressionStoredNodes valueExpression <> expressionStoredNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> map snd bindings <> expressionStoredNodes body
    GrinStoreRecUnchecked bindings body -> map snd bindings <> expressionStoredNodes body
    GrinCase _ _ alternatives -> concatMap (expressionStoredNodes . grinAltRhs) alternatives
    _ -> []

expressionCalledFunctions :: GrinExpr -> [FunctionName]
expressionCalledFunctions expression =
  case expression of
    GrinBind _ valueExpression body -> expressionCalledFunctions valueExpression <> expressionCalledFunctions body
    GrinStoreRec _ body -> expressionCalledFunctions body
    GrinStoreRecUnchecked _ body -> expressionCalledFunctions body
    GrinCall _ functionName _ -> [functionName]
    GrinCase _ _ alternatives -> concatMap (expressionCalledFunctions . grinAltRhs) alternatives
    _ -> []

isThunkNode :: GrinNode -> Bool
isThunkNode node =
  case grinNodeTag node of
    GrinThunk {} -> True
    _ -> False

isNamedThunkNode :: FunctionName -> GrinNode -> Bool
isNamedThunkNode expected node =
  case grinNodeTag node of
    GrinThunk actual -> actual == expected
    _ -> False

continuationHasCaptures :: GrinFunction -> Bool
continuationHasCaptures function =
  "$cps$" `T.isInfixOf` unFunctionName (grinFunctionName function)
    && length (grinFunctionParameters function) > 1

data ExplicitOperand
  = GrinCaseOperand
  | GrinApplyOperand
  deriving (Eq)

containsExplicitOperand :: ExplicitOperand -> GrinFunction -> Bool
containsExplicitOperand operand = go Set.empty . grinFunctionBody
  where
    go evaluated expression =
      case expression of
        GrinBind vars value body ->
          go evaluated value
            || go (recordEvaluation evaluated vars value) body
        GrinStoreRec _ body -> go evaluated body
        GrinApply _ (GrinVarValue function) _ ->
          operand == GrinApplyOperand && function `Set.member` evaluated
        GrinCase (GrinVarValue scrutinee) _ alternatives ->
          (operand == GrinCaseOperand && scrutinee `Set.member` evaluated)
            || any (go evaluated . grinAltRhs) alternatives
        GrinCase _ _ alternatives -> any (go evaluated . grinAltRhs) alternatives
        _ -> False
    recordEvaluation evaluated vars value =
      case (vars, value) of
        ([var], GrinEval {}) -> Set.insert var evaluated
        _ -> evaluated `Set.difference` Set.fromList vars

grinGoldenTests :: IO TestTree
grinGoldenTests =
  testGroup "GRIN golden tests" . map goldenTest <$> GrinGolden.loadGrinCases
  where
    goldenTest fixture = testCase (GrinGolden.caseId fixture) $ do
      let (outcome, details) = GrinGolden.evaluateGrinCase fixture
      case outcome of
        GrinGolden.OutcomePass -> pure ()
        GrinGolden.OutcomeXFail -> pure ()
        GrinGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
        GrinGolden.OutcomeFail -> assertFailure details

grinEvalFixtureTests :: IO TestTree
grinEvalFixtureTests = do
  cases <- EvalGolden.loadEvalCases
  let tests = map mkEvalFixtureTest cases
  pure (testGroup "shared evaluation fixtures via GRIN" tests)

mkEvalFixtureTest :: EvalGolden.EvalCase -> TestTree
mkEvalFixtureTest evalCase = testCase (EvalGolden.evalCaseId evalCase) $ do
  (outcome, details) <- EvalGolden.evaluateEvalCase evaluateGrinProgram evalCase
  case outcome of
    EvalGolden.OutcomePass -> pure ()
    EvalGolden.OutcomeXFail -> pure ()
    EvalGolden.OutcomeXPass -> assertFailure ("unexpected pass (xpass): " <> details)
    EvalGolden.OutcomeFail -> assertFailure details

evaluateGrinProgram :: Text -> FcProgram -> IO (Either String Text)
evaluateGrinProgram name fcProgram = do
  case prepareEvalProgram name (lowerNewtypes fcProgram) of
    Left err -> pure (Left err)
    Right (preparedProgram, entry, unwrapResult) -> do
      let program = lowerProgram preparedProgram
      case lintProgram program of
        [] -> do
          result <-
            case entry of
              EvaluateBinding -> interpretProgramBinding name program
              RunIoAction -> interpretProgramIoBinding name program
          pure $
            case result of
              Left err -> Left (show err)
              Right value -> Right (unwrapResult value)
        lintErrors -> pure (Left ("GRIN lint error: " <> show lintErrors))

data BindingEntry
  = EvaluateBinding
  | RunIoAction

prepareEvalProgram :: Text -> FcProgram -> Either String (FcProgram, BindingEntry, Text -> Text)
prepareEvalProgram name program@(FcProgram topBinds) =
  case break isEvalBinding topBinds of
    (_, []) -> Left ("missing evaluation binding " <> T.unpack name)
    (_, FcTopBind (FcRec _) : _) -> Left "recursive evaluation binding is unsupported"
    (before, FcTopBind (FcNonRec var rhs) : after) -> do
      runtimeRep <- runtimeRepOfType (varType var)
      if runtimeRep == BoxedRep Lifted
        then Right (program, bindingEntry (varType var), id)
        else do
          let componentCount = length (runtimeRepComponents runtimeRep)
              constructorName = evalResultConstructor componentCount
              wrapperType = TcTyCon (TyCon "__AihcEvalResultType" 0) []
              constructorVar = Var constructorName (Unique (-1000000)) (TcFunTy (varType var) wrapperType)
              wrappedVar = var {varType = wrapperType}
              declaration = FcData "__AihcEvalResultType" [] [(constructorName, [varType var])]
              wrappedBinding = FcTopBind (FcNonRec wrappedVar (FcApp (FcVar constructorVar) rhs))
          Right
            ( FcProgram (declaration : before <> (wrappedBinding : after)),
              EvaluateBinding,
              unwrapEvalResult componentCount constructorName
            )
    (_, _ : _) -> Left "invalid evaluation binding"
  where
    isEvalBinding topBind =
      case topBind of
        FcTopBind (FcNonRec var _) -> varName var == name
        FcTopBind (FcRec bindings) -> any ((== name) . varName . fst) bindings
        _ -> False

bindingEntry :: TcType -> BindingEntry
bindingEntry ty
  | isIOType ty = RunIoAction
  | otherwise = EvaluateBinding

isIOType :: TcType -> Bool
isIOType ty =
  case ty of
    TcTyCon (TyCon name arity) [_] -> name == "IO" && arity == 1
    TcForAllTy _ body -> isIOType body
    TcQualTy _ body -> isIOType body
    _ -> False

callEndsInContinuation :: GrinFunction -> Bool
callEndsInContinuation function =
  case grinFunctionBody function of
    GrinBind _ _ body -> callEndsInContinuation function {grinFunctionBody = body}
    GrinCall _ _ arguments ->
      case reverse arguments of
        GrinVarValue continuation : _ -> grinVarName continuation == "$cps_continuation"
        _ -> False
    _ -> False

containsUpdateBlackhole :: GrinExpr -> Bool
containsUpdateBlackhole expression =
  case expression of
    GrinUpdateBlackhole {} -> True
    GrinBind _ value body -> containsUpdateBlackhole value || containsUpdateBlackhole body
    GrinStoreRec _ body -> containsUpdateBlackhole body
    GrinCase _ _ alternatives -> any (containsUpdateBlackhole . grinAltRhs) alternatives
    _ -> False

containsCpsEval :: GrinExpr -> Bool
containsCpsEval expression =
  case expression of
    GrinCpsEval {} -> True
    GrinBind _ value body -> containsCpsEval value || containsCpsEval body
    GrinStoreRec _ body -> containsCpsEval body
    GrinCase _ _ alternatives -> any (containsCpsEval . grinAltRhs) alternatives
    _ -> False

lastMaybe :: [value] -> Maybe value
lastMaybe values =
  case reverse values of
    value : _ -> Just value
    [] -> Nothing

evalResultConstructor :: Int -> Text
evalResultConstructor componentCount
  | componentCount == 0 = "()"
  | componentCount == 1 = "__AihcEvalResult"
  | otherwise = "(" <> T.replicate (componentCount - 1) "," <> ")"

unwrapEvalResult :: Int -> Text -> Text -> Text
unwrapEvalResult componentCount constructorName rendered
  | componentCount == 0 = "<state>"
  | componentCount == 1 = fromMaybe rendered (T.stripPrefix (constructorName <> " ") rendered)
  | otherwise = rendered

applicationProgram :: FcProgram
applicationProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcApp
                (FcLam argumentVar (FcVar argumentVar))
                (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 42)))
            )
        )
    ]
  where
    answerVar = Var "answer" (Unique 1) boxedIntTy
    argumentVar = Var "argument" (Unique 2) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 3) (TcFunTy intTy boxedIntTy)

provenanceNamingProgram :: FcProgram
provenanceNamingProgram =
  FcProgram
    [ FcTopBind
        ( FcNonRec
            ownerVar
            ( FcLam
                inputVar
                ( FcLet
                    ( FcNonRec
                        delayedVar
                        (FcApp (FcLam evaluatedVar (FcVar evaluatedVar)) (FcVar inputVar))
                    )
                    (FcLam returnedVar (FcVar returnedVar))
                )
            )
        )
    ]
  where
    functionTy = TcFunTy boxedIntTy boxedIntTy
    ownerVar = Var "owner" (Unique 160) (TcFunTy boxedIntTy functionTy)
    inputVar = Var "input" (Unique 161) boxedIntTy
    delayedVar = Var "delayed" (Unique 162) boxedIntTy
    evaluatedVar = Var "evaluated" (Unique 163) boxedIntTy
    returnedVar = Var "returned" (Unique 164) boxedIntTy

castAliasClosureProgram :: FcProgram
castAliasClosureProgram = hiddenLambdaProgram castAlias
  where
    firstVar = Var "first" (Unique 172) boxedIntTy
    castAlias = FcCast (FcVar firstVar) (AxiomInstCo "FunctionWrapper" [])

computedLetClosureProgram :: FcProgram
computedLetClosureProgram = hiddenLambdaProgram computed
  where
    unaryFunctionTy = TcFunTy boxedIntTy boxedIntTy
    evaluatedVar = Var "evaluated" (Unique 175) unaryFunctionTy
    parameterVar = Var "parameter" (Unique 176) boxedIntTy
    computed = FcApp (FcLam evaluatedVar (FcVar evaluatedVar)) (FcLam parameterVar (FcVar parameterVar))

hiddenLambdaProgram :: FcExpr -> FcProgram
hiddenLambdaProgram aliasRhs =
  FcProgram
    [ FcData "Holder" [] [("Holder", [binaryFunctionTy])],
      FcTopBind
        ( FcNonRec
            dictionaryVar
            (FcApp (FcVar holderVar) castHiddenLambda)
        )
    ]
  where
    binaryFunctionTy = TcFunTy boxedIntTy (TcFunTy boxedIntTy boxedIntTy)
    unaryFunctionTy = TcFunTy boxedIntTy boxedIntTy
    holderTy = TcTyCon (TyCon "Holder" 0) []
    dictionaryVar = Var "dictionary" (Unique 170) holderTy
    holderVar = Var "Holder" (Unique 171) (TcFunTy binaryFunctionTy holderTy)
    firstVar = Var "first" (Unique 172) boxedIntTy
    aliasVar = Var "alias" (Unique 173) unaryFunctionTy
    secondVar = Var "second" (Unique 174) boxedIntTy
    castHiddenLambda =
      FcLam firstVar $
        FcLet
          (FcNonRec aliasVar aliasRhs)
          (FcCast (FcLam secondVar (FcApp (FcVar aliasVar) (FcVar secondVar))) (Refl unaryFunctionTy))

immediatelyForcedLetProgram :: FcProgram
immediatelyForcedLetProgram =
  FcProgram
    [ FcTopBind
        ( FcNonRec
            ownerVar
            ( FcLam inputVar $
                FcLet
                  (FcNonRec delayedVar computedFunction)
                  (FcApp (FcVar delayedVar) (FcVar inputVar))
            )
        )
    ]
  where
    unaryFunctionTy = TcFunTy boxedIntTy boxedIntTy
    ownerVar = Var "forceOnce" (Unique 180) unaryFunctionTy
    inputVar = Var "input" (Unique 181) boxedIntTy
    delayedVar = Var "delayed" (Unique 182) unaryFunctionTy
    identityVar = Var "identity" (Unique 183) unaryFunctionTy
    argumentVar = Var "argument" (Unique 184) boxedIntTy
    computedFunction = FcApp (FcLam identityVar (FcVar identityVar)) (FcLam argumentVar (FcVar argumentVar))

sharedForcedLetProgram :: FcProgram
sharedForcedLetProgram =
  FcProgram
    [ FcData "FunctionPair" [] [("FunctionPair", [unaryFunctionTy, unaryFunctionTy])],
      FcTopBind
        ( FcNonRec
            ownerVar
            ( FcLam inputVar $
                FcLet
                  (FcNonRec delayedVar computedFunction)
                  ( FcCase
                      (FcVar delayedVar)
                      evaluatedVar
                      [FcAlt DefaultAlt [] (FcApp (FcApp (FcVar pairVar) (FcVar evaluatedVar)) (FcVar delayedVar))]
                  )
            )
        )
    ]
  where
    unaryFunctionTy = TcFunTy boxedIntTy boxedIntTy
    pairTy = TcTyCon (TyCon "FunctionPair" 0) []
    ownerVar = Var "forceShared" (Unique 185) (TcFunTy boxedIntTy pairTy)
    inputVar = Var "input" (Unique 186) boxedIntTy
    delayedVar = Var "delayed" (Unique 187) unaryFunctionTy
    identityVar = Var "identity" (Unique 188) unaryFunctionTy
    argumentVar = Var "argument" (Unique 189) boxedIntTy
    evaluatedVar = Var "evaluated" (Unique 190) unaryFunctionTy
    pairVar = Var "FunctionPair" (Unique 191) (TcFunTy unaryFunctionTy (TcFunTy unaryFunctionTy pairTy))
    computedFunction = FcApp (FcLam identityVar (FcVar identityVar)) (FcLam argumentVar (FcVar argumentVar))

unboxedApplicationProgram :: FcProgram
unboxedApplicationProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcApp
                (FcVar boxConstructorVar)
                (FcApp (FcLam argumentVar (FcVar argumentVar)) (FcLit (LitInt IntRep 42)))
            )
        )
    ]
  where
    answerVar = Var "answer" (Unique 10) boxedIntTy
    argumentVar = Var "argument" (Unique 11) intTy
    boxConstructorVar = Var "BoxedInt" (Unique 12) (TcFunTy intTy boxedIntTy)

shadowingProgram :: FcProgram
shadowingProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcApp
                (FcVar boxConstructorVar)
                (FcApp (FcLam localAnswerVar (FcVar localAnswerVar)) (FcLit (LitInt IntRep 42)))
            )
        )
    ]
  where
    answerVar = Var "answer" (Unique 1) boxedIntTy
    localAnswerVar = Var "answer" (Unique 2) intTy
    boxConstructorVar = Var "BoxedInt" (Unique 3) (TcFunTy intTy boxedIntTy)

cafReferenceProgram :: FcProgram
cafReferenceProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            sourceVar
            ( FcApp
                (FcLam computedVar (FcVar computedVar))
                (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 1)))
            )
        ),
      FcTopBind (FcNonRec answerVar (FcVar sourceVar))
    ]
  where
    sourceVar = Var "source" (Unique 20) boxedIntTy
    answerVar = Var "answer" (Unique 21) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 22) (TcFunTy intTy boxedIntTy)
    computedVar = Var "computed" (Unique 23) boxedIntTy

staticConstructorProgram :: FcProgram
staticConstructorProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            sourceVar
            (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 1)))
        )
    ]
  where
    sourceVar = Var "source" (Unique 24) boxedIntTy
    boxConstructorVar = Var "BoxedInt" (Unique 25) (TcFunTy intTy boxedIntTy)

recursiveFunctionProgram :: FcProgram
recursiveFunctionProgram =
  FcProgram
    [ FcTopBind
        ( FcNonRec
            answerVar
            ( FcLet
                (FcRec [(functionVar, FcLam argumentVar (FcVar argumentVar))])
                (FcVar functionVar)
            )
        )
    ]
  where
    functionTy = TcFunTy boxedIntTy boxedIntTy
    answerVar = Var "answer" (Unique 26) functionTy
    functionVar = Var "function" (Unique 27) functionTy
    argumentVar = Var "argument" (Unique 28) boxedIntTy

primitiveCallProgram :: FcProgram
primitiveCallProgram =
  FcProgram
    [ FcPrimitive addVar 2,
      FcTopBind
        ( FcNonRec
            addOneVar
            (FcLam argumentVar (FcApp (FcApp (FcVar addVar) (FcVar argumentVar)) (FcLit (LitInt IntRep 1))))
        )
    ]
  where
    addVar = Var "+#" (Unique 29) (TcFunTy intTy (TcFunTy intTy intTy))
    addOneVar = Var "addOne" (Unique 30) (TcFunTy intTy intTy)
    argumentVar = Var "argument" (Unique 31) intTy

partialPrimitiveProgram :: FcProgram
partialPrimitiveProgram =
  FcProgram
    [ FcPrimitive addVar 2,
      FcTopBind
        ( FcNonRec
            makeAdderVar
            (FcLam firstVar (FcApp (FcVar addVar) (FcVar firstVar)))
        )
    ]
  where
    addVar = Var "+#" (Unique 130) (TcFunTy intTy (TcFunTy intTy intTy))
    makeAdderVar = Var "makeAdder" (Unique 131) (TcFunTy intTy (TcFunTy intTy intTy))
    firstVar = Var "first" (Unique 132) intTy

zeroWidthSaturatedApplicationProgram :: FcProgram
zeroWidthSaturatedApplicationProgram =
  FcProgram
    [ FcTopBind
        ( FcNonRec
            targetVar
            (FcLam targetValueVar (FcLam stateVar (FcVar targetValueVar)))
        ),
      FcTopBind
        ( FcNonRec
            callerVar
            (FcLam callerValueVar (FcApp (FcVar targetVar) (FcVar callerValueVar)))
        )
    ]
  where
    stateTy = TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]
    targetVar = Var "zeroWidthTarget" (Unique 32) (TcFunTy boxedIntTy (TcFunTy stateTy boxedIntTy))
    callerVar = Var "zeroWidthCaller" (Unique 33) (TcFunTy boxedIntTy (TcFunTy stateTy boxedIntTy))
    targetValueVar = Var "targetValue" (Unique 34) boxedIntTy
    stateVar = Var "state" (Unique 35) stateTy
    callerValueVar = Var "callerValue" (Unique 36) boxedIntTy

zeroWidthUnknownApplicationProgram :: FcProgram
zeroWidthUnknownApplicationProgram =
  FcProgram
    [ FcTopBind
        ( FcNonRec
            callerVar
            (FcLam actionVar (FcLam stateVar (FcApp (FcVar actionVar) (FcVar stateVar))))
        )
    ]
  where
    stateTy = TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]
    actionTy = TcFunTy stateTy boxedIntTy
    callerVar = Var "applyZeroWidth" (Unique 37) (TcFunTy actionTy actionTy)
    actionVar = Var "action" (Unique 38) actionTy
    stateVar = Var "state" (Unique 39) stateTy

zeroWidthConstructorProgram :: FcProgram
zeroWidthConstructorProgram =
  FcProgram
    [ FcData "StateBox" [] [("StateBox", [stateTy, boxedIntTy])],
      FcTopBind
        ( FcNonRec
            partialVar
            (FcLam stateVar (FcApp (FcVar constructorVar) (FcVar stateVar)))
        )
    ]
  where
    stateTy = TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]
    resultTy = TcTyCon (TyCon "StateBox" 0) []
    constructorVar = Var "StateBox" (Unique 40) (TcFunTy stateTy (TcFunTy boxedIntTy resultTy))
    partialVar = Var "partialStateBox" (Unique 41) (TcFunTy stateTy (TcFunTy boxedIntTy resultTy))
    stateVar = Var "state" (Unique 42) stateTy

functionClassificationProgram :: FcProgram
functionClassificationProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcTopBind
        ( FcNonRec
            directVar
            (FcTyLam typeVar (FcLam dictionaryVar (FcLam directArgumentVar (FcVar directArgumentVar))))
        ),
      FcTopBind
        ( FcNonRec
            computedVar
            ( FcLet
                (FcNonRec expensiveVar (FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep 42))))
                (FcLam computedArgumentVar (FcVar expensiveVar))
            )
        )
    ]
  where
    typeVar = TyVarId "a" (Unique 30)
    typeVarTy = TcTyVar typeVar
    dictionaryVar = Var "$dictionary" (Unique 31) boxedIntTy
    directArgumentVar = Var "argument" (Unique 32) typeVarTy
    directVar = Var "direct" (Unique 33) (TcForAllTy typeVar (TcFunTy boxedIntTy (TcFunTy typeVarTy typeVarTy)))
    expensiveVar = Var "expensive" (Unique 34) boxedIntTy
    computedArgumentVar = Var "argument" (Unique 35) boxedIntTy
    computedVar = Var "computed" (Unique 36) (TcFunTy boxedIntTy boxedIntTy)
    boxConstructorVar = Var "BoxedInt" (Unique 37) (TcFunTy intTy boxedIntTy)

etaExpandedKnownFunctionProgram :: FcProgram
etaExpandedKnownFunctionProgram = knownFunctionWrapperProgram True

nonEtaExpandedKnownFunctionProgram :: FcProgram
nonEtaExpandedKnownFunctionProgram = knownFunctionWrapperProgram False

knownFunctionWrapperProgram :: Bool -> FcProgram
knownFunctionWrapperProgram isEtaExpansion =
  FcProgram
    [ FcData "Applicative" [] [("$Dict$Applicative", [unaryFunctionTy])],
      FcTopBind
        ( FcNonRec
            pureIoVar
            (FcLam pureValueVar (FcLam stateVar (FcVar pureValueVar)))
        ),
      FcTopBind
        ( FcNonRec
            dictionaryVar
            ( FcApp
                (FcVar dictionaryConstructorVar)
                (FcLam valueVar (FcApp (FcVar pureIoVar) argument))
            )
        )
    ]
  where
    unaryFunctionTy = TcFunTy boxedIntTy boxedIntTy
    pureIoTy = TcFunTy boxedIntTy unaryFunctionTy
    dictionaryTy = TcTyCon (TyCon "Applicative" 0) []
    pureIoVar = Var "pureIO" (Unique 140) pureIoTy
    pureValueVar = Var "value" (Unique 141) boxedIntTy
    stateVar = Var "state" (Unique 142) boxedIntTy
    dictionaryVar = Var "$fApplicativeIO" (Unique 143) dictionaryTy
    dictionaryConstructorVar = Var "$Dict$Applicative" (Unique 144) (TcFunTy unaryFunctionTy dictionaryTy)
    valueVar = Var "value" (Unique 145) boxedIntTy
    argument
      | isEtaExpansion = FcVar valueVar
      | otherwise = FcLit (LitString "changed")

dictionaryProgram :: FcProgram
dictionaryProgram =
  FcProgram
    [ FcData "Box" [] [("Box", [intTy])],
      FcData "Test" [] [("$Dict$Test", [boxedIntTy, boxedIntTy])],
      FcTopBind
        ( FcNonRec
            answerVar
            ( FcCase
                dictionary
                dictionaryBinder
                [FcAlt (DataAlt "$Dict$Test") [firstMethod, secondMethod] (FcVar secondMethod)]
            )
        )
    ]
  where
    dictionaryTy = TcTyCon (TyCon "Test" 0) []
    answerVar = Var "answer" (Unique 40) boxedIntTy
    boxConstructorVar = Var "Box" (Unique 41) (TcFunTy intTy boxedIntTy)
    dictionaryConstructorVar = Var "$Dict$Test" (Unique 42) (TcFunTy boxedIntTy (TcFunTy boxedIntTy dictionaryTy))
    dictionaryBinder = Var "$dictionary" (Unique 43) dictionaryTy
    firstMethod = Var "$method0" (Unique 44) boxedIntTy
    secondMethod = Var "$method1" (Unique 45) boxedIntTy
    dictionary = FcApp (FcApp (FcVar dictionaryConstructorVar) (boxed 1)) (boxed 2)
    boxed value = FcApp (FcVar boxConstructorVar) (FcLit (LitInt IntRep value))

exceptionProgram :: FcProgram
exceptionProgram =
  FcProgram
    [ FcData "BoxedInt" [] [("BoxedInt", [intTy])],
      FcPrimitive raiseVar 1,
      FcPrimitive catchVar 3,
      FcTopBind (FcNonRec answerVar (FcApp (FcVar boxConstructorVar) caughtExpression))
    ]
  where
    raiseVar = Var "raise#" (Unique 1) (TcFunTy intTy intTy)
    catchVar = Var "catch#" (Unique 2) (TcFunTy actionTy (TcFunTy handlerTy (TcFunTy intTy intTy)))
    answerVar = Var "answer" (Unique 3) boxedIntTy
    actionState = Var "actionState" (Unique 4) intTy
    exception = Var "exception" (Unique 5) intTy
    handlerState = Var "handlerState" (Unique 6) intTy
    actionTy = TcFunTy intTy intTy
    handlerTy = TcFunTy intTy (TcFunTy intTy intTy)
    action = FcLam actionState (FcApp (FcVar raiseVar) (FcLit (LitInt IntRep 10)))
    handler = FcLam exception (FcLam handlerState (FcVar exception))
    boxConstructorVar = Var "BoxedInt" (Unique 7) (TcFunTy intTy boxedIntTy)
    caughtExpression =
      FcApp
        (FcApp (FcApp (FcVar catchVar) action) handler)
        (FcLit (LitInt IntRep 0))

multiValueContinuationProgram :: GrinProgram
multiValueContinuationProgram =
  GrinProgram
    { grinConstructors = [("Box", [[IntRep]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "multiValue",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [first, second]
                  (GrinCall multiValueRep calleeName [])
                  (GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinVarValue first]))
            },
          GrinFunction
            { grinFunctionName = calleeName,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = multiValueRep,
              grinFunctionBody = GrinConstant [GrinLitValue (GrinLitInt IntRep 1), GrinLitValue (GrinLitInt WordRep 2)]
            }
        ]
    }
  where
    first = GrinVar "first" 1 IntRep
    second = GrinVar "second" 2 WordRep
    calleeName = FunctionName "multiValueCallee"
    multiValueRep = TupleRep [IntRep, WordRep]

zeroWidthApplyProgram :: GrinProgram
zeroWidthApplyProgram =
  GrinProgram
    { grinConstructors = [("Box", [[IntRep]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [(answer, GrinNode (GrinThunk wrapperName) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = wrapperName,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [closurePointer]
                  (GrinStore (GrinNode (GrinClosure targetName [[]]) []))
                  ( GrinBind
                      [closure]
                      (GrinEval (BoxedRep Lifted) (GrinVarValue closurePointer))
                      (GrinApply (BoxedRep Lifted) (GrinVarValue closure) [])
                  )
            },
          GrinFunction
            { grinFunctionName = targetName,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)])
            }
        ]
    }
  where
    answer = GrinVar "answer" 1 (BoxedRep Lifted)
    closurePointer = GrinVar "closure_pointer" 2 (BoxedRep Lifted)
    closure = GrinVar "closure" 3 (BoxedRep Lifted)
    wrapperName = FunctionName "zeroWidthWrapper"
    targetName = FunctionName "zeroWidthTarget"

heapProgram :: GrinProgram
heapProgram =
  GrinProgram
    { grinConstructors = [("Box", [[IntRep]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs =
        [ ( answer,
            GrinNode (GrinThunk functionName) []
          )
        ],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = functionName,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [pointer] (GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)])) $
                  GrinBind [fetched] (GrinFetch (BoxedRep Lifted) (GrinVarValue pointer)) $
                    GrinBind [replacementPointer] (GrinStore replacementBox) $
                      GrinBind [replacement] (GrinFetch (BoxedRep Lifted) (GrinVarValue replacementPointer)) $
                        GrinBind [updated] (GrinUpdate (GrinVarValue pointer) (GrinVarValue replacement)) $
                          GrinEval (BoxedRep Lifted) (GrinVarValue pointer)
            }
        ]
    }
  where
    answer = GrinVar "answer" 1 (BoxedRep Lifted)
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    fetched = GrinVar "fetched" 3 (BoxedRep Lifted)
    replacementPointer = GrinVar "replacement_pointer" 4 (BoxedRep Lifted)
    replacement = GrinVar "replacement" 5 (BoxedRep Lifted)
    updated = GrinVar "updated" 6 (BoxedRep Lifted)
    replacementBox = GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 2)]
    functionName = FunctionName "answer_code"

implicitCaseEvaluationProgram :: GrinProgram
implicitCaseEvaluationProgram =
  implicitEvaluationProgram $
    GrinCase
      (GrinVarValue implicitPointer)
      implicitValue
      [ GrinAlt
          (GrinDataAlt "Box")
          [implicitField]
          (GrinConstant [GrinVarValue implicitValue])
      ]

implicitApplyEvaluationProgram :: GrinProgram
implicitApplyEvaluationProgram =
  implicitEvaluationProgram $
    GrinApply
      (BoxedRep Lifted)
      (GrinVarValue implicitPointer)
      []

implicitEvaluationProgram :: GrinExpr -> GrinProgram
implicitEvaluationProgram operation =
  GrinProgram
    { grinConstructors = [("Box", [[IntRep]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [(implicitAnswer, GrinNode (GrinThunk implicitAnswerFunction) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = implicitAnswerFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [implicitPointer]
                  (GrinStore (GrinNode (GrinThunk implicitValueFunction) []))
                  operation
            },
          GrinFunction
            { grinFunctionName = implicitValueFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinStore
                  (GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)])
            }
        ]
    }

implicitAnswer :: GrinVar
implicitAnswer = GrinVar "answer" 100 (BoxedRep Lifted)

implicitPointer :: GrinVar
implicitPointer = GrinVar "pointer" 101 (BoxedRep Lifted)

implicitValue :: GrinVar
implicitValue = GrinVar "value" 102 (BoxedRep Lifted)

implicitField :: GrinVar
implicitField = GrinVar "field" 103 IntRep

implicitAnswerFunction :: FunctionName
implicitAnswerFunction = FunctionName "implicit_answer"

implicitValueFunction :: FunctionName
implicitValueFunction = FunctionName "implicit_value"

storedWhnfEvalProgram :: GrinProgram
storedWhnfEvalProgram =
  emptyGrinProgram
    { grinConstructors = [("Box", [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = storedWhnfEntry,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [storedWhnfPointer]
                  (GrinStore (GrinNode (GrinConstructor "Box" 0) []))
                  (GrinEval (BoxedRep Lifted) (GrinVarValue storedWhnfPointer))
            }
        ]
    }

staticWhnfProgram :: GrinProgram
staticWhnfProgram =
  emptyGrinProgram
    { grinConstructors = [("Box", [])],
      grinWhnfGlobals = [(staticWhnfGlobal, GrinNode (GrinConstructor "Box" 0) [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = staticWhnfEntry,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinEval (BoxedRep Lifted) (GrinVarValue staticWhnfGlobal)
            }
        ]
    }

storedClosureApplyProgram :: GrinProgram
storedClosureApplyProgram =
  emptyGrinProgram
    { grinConstructors = [("Box", [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = storedClosureEntry,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [storedClosurePointer]
                  (GrinStore (GrinNode (GrinClosure storedClosureTarget [[BoxedRep Lifted]]) []))
                  ( GrinBind
                      [storedClosureArgument]
                      (GrinStore (GrinNode (GrinConstructor "Box" 0) []))
                      ( GrinApply
                          (BoxedRep Lifted)
                          (GrinVarValue storedClosurePointer)
                          [GrinVarValue storedClosureArgument]
                      )
                  )
            },
          GrinFunction
            { grinFunctionName = storedClosureTarget,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [storedClosureParameter],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinConstant [GrinVarValue storedClosureParameter]
            }
        ]
    }

storedWhnfCaseProgram :: GrinProgram
storedWhnfCaseProgram =
  emptyGrinProgram
    { grinConstructors = [("Box", [])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = storedWhnfCaseEntry,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [storedWhnfPointer]
                  (GrinStore (GrinNode (GrinConstructor "Box" 0) []))
                  ( GrinCase
                      (GrinVarValue storedWhnfPointer)
                      storedWhnfBinder
                      [ GrinAlt
                          (GrinDataAlt "Box")
                          []
                          (GrinConstant [GrinVarValue storedWhnfBinder])
                      ]
                  )
            }
        ]
    }

emptyGrinProgram :: GrinProgram
emptyGrinProgram =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions = []
    }

storedWhnfEntry, staticWhnfEntry, storedWhnfCaseEntry, storedClosureEntry, storedClosureTarget :: FunctionName
storedWhnfEntry = FunctionName "stored_whnf"
staticWhnfEntry = FunctionName "static_whnf"
storedWhnfCaseEntry = FunctionName "stored_whnf_case"
storedClosureEntry = FunctionName "stored_closure"
storedClosureTarget = FunctionName "stored_closure_target"

storedWhnfPointer, staticWhnfGlobal, storedWhnfBinder, storedClosurePointer, storedClosureArgument, storedClosureParameter :: GrinVar
storedWhnfPointer = GrinVar "stored_whnf_pointer" 104 (BoxedRep Lifted)
staticWhnfGlobal = GrinVar "static_whnf_global" 105 (BoxedRep Lifted)
storedWhnfBinder = GrinVar "stored_whnf_binder" 106 (BoxedRep Lifted)
storedClosurePointer = GrinVar "stored_closure_pointer" 107 (BoxedRep Lifted)
storedClosureArgument = GrinVar "stored_closure_argument" 108 (BoxedRep Lifted)
storedClosureParameter = GrinVar "stored_closure_parameter" 109 (BoxedRep Lifted)

directBindProgram :: GrinProgram
directBindProgram =
  heapProgram
    { grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "direct_binds",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [constant] (GrinConstant [zero]) $
                  GrinBind [pointer] (GrinStore box) $
                    GrinBind [selected] (GrinCase (GrinVarValue constant) caseBinder [alternative]) $
                      GrinConstant [GrinVarValue selected]
            }
        ]
    }
  where
    constant = GrinVar "constant" 1 IntRep
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    caseBinder = GrinVar "case" 3 IntRep
    selected = GrinVar "selected" 4 (BoxedRep Lifted)
    zero = GrinLitValue (GrinLitInt IntRep 0)
    box = GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)]
    alternative =
      GrinAlt
        { grinAltCon = GrinDefaultAlt,
          grinAltBinders = [],
          grinAltRhs = GrinConstant [GrinVarValue pointer]
        }

gcRootProgram :: GrinProgram
gcRootProgram =
  heapProgram
    { grinConstructors = [("Box", [[BoxedRep Lifted]])],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "gc_roots",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [root, scalar],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind
                  [result]
                  (GrinStore (GrinNode (GrinConstructor "Box" 0) [GrinVarValue root]))
                  (GrinConstant [GrinVarValue result])
            }
        ]
    }
  where
    root = GrinVar "root" 201 (BoxedRep Lifted)
    scalar = GrinVar "scalar" 202 IntRep
    result = GrinVar "result" 203 (BoxedRep Lifted)

callBindProgram :: GrinProgram
callBindProgram =
  heapProgram
    { grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = caller,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [capture],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [result] (GrinCall (BoxedRep Lifted) callee []) $
                  GrinConstant [GrinVarValue capture]
            },
          GrinFunction
            { grinFunctionName = callee,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = GrinStore box
            }
        ]
    }
  where
    caller = FunctionName "caller"
    callee = FunctionName "callee"
    capture = GrinVar "capture" 1 (BoxedRep Lifted)
    result = GrinVar "result" 2 (BoxedRep Lifted)
    box = GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)]

transferringExpressions :: [GrinExpr]
transferringExpressions =
  [ GrinEval lifted string,
    GrinCall lifted (FunctionName "callee") [],
    GrinApply lifted string []
  ]
  where
    lifted = BoxedRep Lifted
    string = GrinLitValue (GrinLitString "function")

cpsOnlyExpressions :: [GrinExpr]
cpsOnlyExpressions =
  [ GrinCpsEval lifted string string string,
    GrinCpsPrimitiveCall lifted "fork#" [string] string,
    GrinCpsApply lifted string [] string,
    GrinContinue string [],
    GrinUpdateBlackhole string string,
    GrinHalt []
  ]
  where
    lifted = BoxedRep Lifted
    string = GrinLitValue (GrinLitString "continuation")

controlPrimitiveBindProgram :: Text -> GrinProgram
controlPrimitiveBindProgram name =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [(GrinVar name 1 lifted, primitiveArity)],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "control_primitive",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = lifted,
              grinFunctionBody = GrinBind resultVars primitiveCall (GrinConstant [string])
            }
        ]
    }
  where
    lifted = BoxedRep Lifted
    string = GrinLitValue (GrinLitString "result")
    (primitiveArity, resultVars, primitiveCall) =
      case name of
        "fork#" ->
          ( 2,
            [GrinVar "thread_id" 2 (BoxedRep Unlifted)],
            GrinPrimitiveCall (TupleRep [TupleRep [], BoxedRep Unlifted]) name [GrinLitValue (GrinLitString "action")]
          )
        _ -> (1, [], GrinPrimitiveCall (TupleRep []) name [])

containsCpsPrimitive :: Text -> GrinExpr -> Bool
containsCpsPrimitive name expression =
  case expression of
    GrinCpsPrimitiveCall _ actual _ _ -> actual == name
    GrinBind _ value body -> containsCpsPrimitive name value || containsCpsPrimitive name body
    GrinStoreRec _ body -> containsCpsPrimitive name body
    GrinCase _ _ alternatives -> any (containsCpsPrimitive name . grinAltRhs) alternatives
    _ -> False

cpsOnlyFunction :: FunctionName
cpsOnlyFunction = FunctionName "cps_only"

cpsOnlyProgram :: GrinExpr -> GrinProgram
cpsOnlyProgram expression =
  directBindProgram
    { grinFunctions =
        [ GrinFunction
            { grinFunctionName = cpsOnlyFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody = expression
            }
        ]
    }

singleBindProgram :: GrinExpr -> GrinProgram
singleBindProgram valueExpression =
  directBindProgram
    { grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "single_bind",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [result] valueExpression $
                  GrinConstant [GrinVarValue result]
            }
        ]
    }
  where
    result = GrinVar "result" 1 (BoxedRep Lifted)

exceptionBoundaryProgram :: GrinExpr -> GrinProgram
exceptionBoundaryProgram body =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = exceptionBoundaryFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = IntRep,
              grinFunctionBody = body
            }
        ]
    }

exceptionBoundaryFunction :: FunctionName
exceptionBoundaryFunction = FunctionName "exception_boundary"

exceptionAction :: GrinValue
exceptionAction = GrinLitValue (GrinLitString "action")

exceptionHandler :: GrinValue
exceptionHandler = GrinLitValue (GrinLitString "handler")

invalidUpdateProgram :: GrinProgram
invalidUpdateProgram =
  heapProgram
    { grinFunctions =
        [ GrinFunction
            { grinFunctionName = FunctionName "answer_code",
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [],
              grinFunctionResultRep = BoxedRep Lifted,
              grinFunctionBody =
                GrinBind [pointer] (GrinStore initialBox) $
                  GrinBind [updated] (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt IntRep 2))) $
                    GrinConstant [GrinLitValue (GrinLitInt IntRep 0)]
            }
        ]
    }
  where
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    updated = GrinVar "updated" 3 IntRep
    initialBox = GrinNode (GrinConstructor "Box" 0) [GrinLitValue (GrinLitInt IntRep 1)]

unliftedRuntimeReps :: [RuntimeRep]
unliftedRuntimeReps =
  [ IntRep,
    WordRep,
    BoxedRep Unlifted,
    TupleRep [IntRep, WordRep],
    TupleRep []
  ]

unliftedHeapProgram :: RuntimeRep -> GrinProgram
unliftedHeapProgram runtimeRep =
  GrinProgram
    { grinConstructors = [],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinExternalGlobals = [],
      grinExternalFunctions = [],
      grinWhnfGlobals = [],
      grinCafs = [(GrinVar "bad" 1 (BoxedRep Lifted), GrinNode (GrinThunk unliftedThunkFunction) [GrinLitValue (GrinLitString "capture")])],
      grinFunctions =
        [ GrinFunction
            { grinFunctionName = unliftedThunkFunction,
              grinFunctionLinkName = Nothing,
              grinFunctionParameters = [pointer],
              grinFunctionResultRep = runtimeRep,
              grinFunctionBody =
                GrinBind
                  [updated]
                  (GrinUpdate (GrinVarValue pointer) (GrinLitValue (GrinLitInt runtimeRep 0)))
                  ( GrinConstant
                      [ GrinLitValue (GrinLitInt componentRep 0)
                      | componentRep <- runtimeRepComponents runtimeRep
                      ]
                  )
            }
        ]
    }
  where
    pointer = GrinVar "pointer" 2 (BoxedRep Lifted)
    updated = GrinVar "updated" 3 runtimeRep

unliftedThunkFunction :: FunctionName
unliftedThunkFunction = FunctionName "unlifted_thunk"

intTy :: TcType
intTy = TcTyCon (TyCon "Int#" 0) []

boxedIntTy :: TcType
boxedIntTy = TcTyCon (TyCon "Int" 0) []

foreignProgram :: FcProgram
foreignProgram =
  FcProgram
    [ FcData "BoxedInt32" [] [("BoxedInt32", [int32Ty])],
      FcForeignImport foreignCall,
      FcTopBind
        ( FcNonRec
            foreignAnswerVar
            (FcApp (FcVar foreignBoxConstructorVar) (FcCallForeign foreignCall [FcLit (LitInt Int32Rep 42)]))
        )
    ]

undersaturatedForeignProgram :: FcProgram
undersaturatedForeignProgram =
  FcProgram
    [ FcData "BoxedInt32" [] [("BoxedInt32", [int32Ty])],
      FcForeignImport foreignCall,
      FcTopBind (FcNonRec foreignAnswerVar (FcApp (FcVar foreignBoxConstructorVar) (FcCallForeign foreignCall [])))
    ]

foreignCall :: FcForeignCall
foreignCall =
  FcForeignCall
    { fcForeignCallName = "$ffi$abs",
      fcForeignCallSymbol = "abs",
      fcForeignCallSignature =
        FcForeignSignature
          { fcForeignArgumentTypes = [FcForeignInt32],
            fcForeignResultType = FcForeignInt32,
            fcForeignEffect = FcForeignPure
          }
    }

foreignAnswerVar :: Var
foreignAnswerVar = Var "answer" (Unique 50) boxedIntTy

foreignBoxConstructorVar :: Var
foreignBoxConstructorVar = Var "BoxedInt32" (Unique 51) (TcFunTy int32Ty boxedIntTy)

int32Ty :: TcType
int32Ty = TcTyCon (TyCon "Int32#" 0) []

saturatedConstructorProgram :: FcProgram
saturatedConstructorProgram =
  FcProgram
    [ FcData "Int32" [] [("I32#", [int32Ty])],
      FcTopBind
        ( FcNonRec
            int32WrapperVar
            (FcLam int32ArgumentVar (FcApp (FcVar int32ConstructorVar) (FcVar int32ArgumentVar)))
        )
    ]
  where
    int32WrapperVar = Var "wrapInt32" (Unique 60) (TcFunTy int32Ty boxedInt32Ty)
    int32ConstructorVar = Var "I32#" (Unique 61) (TcFunTy int32Ty boxedInt32Ty)

tailStoredTupleProgram :: FcProgram
tailStoredTupleProgram =
  FcProgram
    [ FcData "Box" [] [("Box", [intTy])],
      FcPrimitive tailStoredStateVar 0,
      FcTopBind
        ( FcNonRec
            tailStoredFunctionVar
            ( FcLam
                tailStoredValueVar
                ( FcApp
                    (FcApp (FcVar tailStoredTupleConstructorVar) (FcVar tailStoredStateVar))
                    (FcApp (FcVar tailStoredBoxConstructorVar) (FcVar tailStoredValueVar))
                )
            )
        )
    ]

tailStoredFunctionVar :: Var
tailStoredFunctionVar = Var "tailStored" (Unique 80) (TcFunTy intTy tailStoredTupleTy)

tailStoredValueVar :: Var
tailStoredValueVar = Var "value" (Unique 81) intTy

tailStoredStateVar :: Var
tailStoredStateVar = Var "realWorld#" (Unique 82) tailStoredStateTy

tailStoredBoxConstructorVar :: Var
tailStoredBoxConstructorVar = Var "Box" (Unique 83) (TcFunTy intTy tailStoredBoxTy)

tailStoredTupleConstructorVar :: Var
tailStoredTupleConstructorVar =
  Var "(#,#)" (Unique 84) (TcFunTy tailStoredStateTy (TcFunTy tailStoredBoxTy tailStoredTupleTy))

tailStoredStateTy :: TcType
tailStoredStateTy = TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]

tailStoredBoxTy :: TcType
tailStoredBoxTy = TcTyCon (TyCon "Box" 0) []

tailStoredTupleTy :: TcType
tailStoredTupleTy = TcTyCon (TyCon "(#,#)" 2) [tailStoredStateTy, tailStoredBoxTy]

boxedInt32Ty :: TcType
boxedInt32Ty = TcTyCon (TyCon "Int32" 0) []

int32ArgumentVar :: Var
int32ArgumentVar = Var "value" (Unique 62) int32Ty

separateConstructorPrograms :: [FcProgram]
separateConstructorPrograms =
  [ FcProgram [FcData "Int32" [] [("I32#", [int32Ty])]],
    FcProgram
      [ FcTopBind
          ( FcNonRec
              wrapperVar
              (FcLam argumentVar (FcApp (FcVar constructorVar) (FcVar argumentVar)))
          )
      ]
  ]
  where
    wrapperVar = Var "wrapInt32" (Unique 70) (TcFunTy int32Ty boxedInt32Ty)
    constructorVar = Var "I32#" (Unique 71) (TcFunTy int32Ty boxedInt32Ty)
    argumentVar = Var "value" (Unique 72) int32Ty

separatePrograms :: [FcProgram]
separatePrograms =
  [ FcProgram [FcTopBind (FcNonRec sourceVar (FcLit (LitString "provider")))],
    FcProgram [FcTopBind (FcNonRec answerVar (FcLam argumentVar (FcVar sourceVar)))]
  ]
  where
    sourceVar = Var "source" (Unique 30) boxedIntTy
    answerVar = Var "answer" (Unique 31) (TcFunTy boxedIntTy boxedIntTy)
    argumentVar = Var "argument" (Unique 32) boxedIntTy

separateFunctionPrograms :: [FcProgram]
separateFunctionPrograms =
  [ FcProgram
      [ FcTopBind (FcNonRec sourceVar (FcLit (LitString "provider"))),
        FcTopBind (FcNonRec identityVar (FcLam argumentVar (FcVar argumentVar)))
      ],
    FcProgram [FcTopBind (FcNonRec answerVar (FcApp (FcVar identityVar) (FcVar sourceVar)))]
  ]
  where
    sourceVar = Var "source" (Unique 80) boxedIntTy
    identityVar = Var "identity" (Unique 81) (TcFunTy boxedIntTy boxedIntTy)
    argumentVar = Var "argument" (Unique 82) boxedIntTy
    answerVar = Var "answer" (Unique 83) boxedIntTy

separateNewtypePrograms :: [FcProgram]
separateNewtypePrograms =
  [ FcProgram [FcNewtype declaration],
    FcProgram [FcTopBind (FcNonRec answerVar (FcLam argumentVar (FcApp (FcVar constructorVar) (FcLit (LitInt IntRep 42)))))]
  ]
  where
    declaration =
      FcNewtypeDecl
        { fcNewtypeName = "Wrapper",
          fcNewtypeTyVars = [],
          fcNewtypeConstructor = "Wrap",
          fcNewtypeRepresentation = intTy,
          fcNewtypeResult = wrapperTy
        }
    wrapperTy = TcTyCon (TyCon "Wrapper" 0) []
    constructorVar = Var "Wrap" (Unique 40) (TcFunTy intTy wrapperTy)
    answerVar = Var "answer" (Unique 41) (TcFunTy boxedIntTy wrapperTy)
    argumentVar = Var "argument" (Unique 42) boxedIntTy
