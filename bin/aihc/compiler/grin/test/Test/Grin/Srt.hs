{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Srt (tests) where

import Aihc.Grin.Srt
import Aihc.Grin.Syntax
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "static reference tables"
    [ testCase "thunks, closures, and objects with fields are traced" $
        assertEqual
          "traced objects"
          (Set.fromList ["box", "caf", "closure", "unboxed"])
          (staticReferenceTracedObjects (programStaticReferences program)),
      testCase "nullary constructors become untraced static objects" $
        assertEqual
          "static objects"
          [ ("box", True),
            ("caf", True),
            ("closure", True),
            ("nil", False),
            ("unboxed", True),
            ("Unit", False)
          ]
          [(staticObjectName object, staticObjectTraced object) | object <- programStaticObjects program],
      testCase "an object from another compilation unit enters the table" $
        assertEqual
          "usesImported"
          (Just (StaticReferenceTable ["imported"] []))
          (tableOf "usesImported"),
      testCase "a free variable naming another unit's object enters the table" $
        assertEqual
          "usesImportedByVar"
          (Just (StaticReferenceTable ["importedByVar"] []))
          (tableOf "usesImportedByVar"),
      testCase "a mentioned traced object enters the table" $
        assertEqual
          "usesCaf"
          (Just (StaticReferenceTable ["caf"] []))
          (tableOf "usesCaf"),
      testCase "a free variable names a static object" $
        assertEqual
          "usesCafByVar"
          (Just (StaticReferenceTable ["caf"] []))
          (tableOf "usesCafByVar"),
      testCase "a bound variable shadows an equally named static object" $
        assertEqual
          "shadowsCaf"
          Nothing
          (tableOf "shadowsCaf"),
      testCase "mentioning only untraced objects needs no table" $
        assertEqual
          "usesNil"
          Nothing
          (tableOf "usesNil"),
      testCase "a known call becomes a child table" $
        assertEqual
          "callsUsesCaf"
          (Just (StaticReferenceTable [] [FunctionName "usesCaf"]))
          (tableOf "callsUsesCaf"),
      testCase "children are transitive across known calls" $
        assertEqual
          "callsCallsUsesCaf"
          (Just (StaticReferenceTable [] [FunctionName "callsUsesCaf"]))
          (tableOf "callsCallsUsesCaf"),
      testCase "a call to a table-free function adds no child" $
        assertEqual
          "callsUsesNil"
          Nothing
          (tableOf "callsUsesNil"),
      testCase "storing a thunk does not inherit its table" $
        assertEqual
          "storesThunk"
          Nothing
          (tableOf "storesThunk"),
      testCase "self recursion drops the self child" $
        assertEqual
          "recursesOnCaf"
          (Just (StaticReferenceTable ["caf"] []))
          (tableOf "recursesOnCaf"),
      testCase "mutual recursion reaches a fixed point" $
        assertEqual
          "mutual tables"
          [ Just (StaticReferenceTable [] [FunctionName "mutualB"]),
            Just (StaticReferenceTable ["caf"] [FunctionName "mutualA"])
          ]
          [tableOf "mutualA", tableOf "mutualB"],
      testCase "only reaching functions get a table" $
        assertEqual
          "table names"
          [ "callsCallsUsesCaf",
            "callsUsesCaf",
            "mutualA",
            "mutualB",
            "recursesOnCaf",
            "usesCaf",
            "usesCafByVar",
            "usesImported",
            "usesImportedByVar"
          ]
          (map unFunctionName (Map.keys (staticReferenceTables (programStaticReferences program))))
    ]
  where
    tableOf name = lookupStaticReferenceTable (programStaticReferences program) (FunctionName name)

program :: GrinProgram
program =
  GrinProgram
    { grinConstructors = [("Unit", []), ("Box", [[liftedGrinRep]]), ("Counter", [[IntRep]])],
      grinPrimitives = [],
      grinForeignCalls = [],
      grinGlobals =
        [ ("box", GrinNode (GrinConstructor "Box" 0) [GrinGlobalValue "caf"]),
          ("caf", GrinNode (GrinThunk (FunctionName "cafEntry")) []),
          ("closure", GrinNode (GrinClosure (FunctionName "usesCaf") [[]]) []),
          ("nil", GrinNode (GrinConstructor "Unit" 0) []),
          ("unboxed", GrinNode (GrinConstructor "Counter" 0) [GrinLitValue (GrinLitInt IntRep 3)])
        ],
      grinFunctions =
        [ function "cafEntry" [] (GrinConstant [GrinGlobalValue "nil"]),
          function "usesCaf" [] (GrinConstant [GrinGlobalValue "caf"]),
          function "usesImported" [] (GrinConstant [GrinGlobalValue "imported"]),
          function "usesImportedByVar" [] (GrinConstant [GrinVarValue (boxed "importedByVar")]),
          function "usesCafByVar" [] (GrinConstant [GrinVarValue (boxed "caf")]),
          function
            "shadowsCaf"
            []
            (GrinBind [boxed "caf"] (GrinConstant [GrinGlobalValue "nil"]) (GrinConstant [GrinVarValue (boxed "caf")])),
          function "usesNil" [] (GrinConstant [GrinGlobalValue "nil"]),
          function "callsUsesCaf" [] (call "usesCaf"),
          function "callsCallsUsesCaf" [] (call "callsUsesCaf"),
          function "callsUsesNil" [] (call "usesNil"),
          function "storesThunk" [] (GrinStore (GrinNode (GrinThunk (FunctionName "usesCaf")) [])),
          function
            "recursesOnCaf"
            []
            (GrinBind [boxed "value"] (GrinConstant [GrinGlobalValue "caf"]) (call "recursesOnCaf")),
          function "mutualA" [] (call "mutualB"),
          function
            "mutualB"
            []
            (GrinBind [boxed "value"] (GrinConstant [GrinGlobalValue "caf"]) (call "mutualA"))
        ]
    }
  where
    call name = GrinCall liftedGrinRep (FunctionName name) []
    function name parameters = GrinFunction (FunctionName name) parameters liftedGrinRep

boxed :: Text -> GrinVar
boxed name = GrinVar name 0 liftedGrinRep
