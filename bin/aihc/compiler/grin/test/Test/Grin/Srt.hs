{-# LANGUAGE OverloadedStrings #-}

module Test.Grin.Srt (tests) where

import Aihc.Grin.Srt
import Aihc.Grin.Syntax
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
          "callsTwo"
          (Just (StaticReferenceTable [] [owner "usesCaf", owner "usesImported"]))
          (tableOf "callsTwo"),
      testCase "a table with no objects and one child uses the table of the child" $
        assertEqual
          "callsUsesCaf"
          (ownerOf "usesCaf")
          (ownerOf "callsUsesCaf"),
      testCase "a chain of such tables uses the table at its end" $
        assertEqual
          "callsCallsUsesCaf"
          (ownerOf "usesCaf")
          (ownerOf "callsCallsUsesCaf"),
      testCase "children that become one table make the table shared" $
        assertEqual
          "callsSameTwice"
          (ownerOf "usesCaf")
          (ownerOf "callsSameTwice"),
      testCase "functions with the same table share one table" $
        assertEqual
          "usesCafByVar"
          (ownerOf "usesCaf")
          (ownerOf "usesCafByVar"),
      testCase "a call to a table-free function adds no child" $
        assertEqual
          "callsUsesNil"
          Nothing
          (tableOf "callsUsesNil"),
      testCase "a future thunk inherits its table" $
        assertEqual
          "storesThunk"
          (ownerOf "usesCaf")
          (ownerOf "storesThunk"),
      testCase "self recursion drops the self child" $
        assertEqual
          "recursesOnCaf"
          (Just (StaticReferenceTable ["caf"] []))
          (tableOf "recursesOnCaf"),
      testCase "mutual recursion reaches a fixed point" $
        assertEqual
          "mutual tables"
          [ (ownerOf "usesCaf", Just (StaticReferenceTable ["caf"] [])),
            (ownerOf "usesCaf", Just (StaticReferenceTable ["caf"] []))
          ]
          [(ownerOf "mutualA", tableOf "mutualA"), (ownerOf "mutualB", tableOf "mutualB")],
      testCase "only reaching functions get a table" $
        assertEqual
          "table names"
          [ "callsTwo",
            "mutualB",
            "usesImported",
            "usesImportedByVar"
          ]
          (map unFunctionName (Map.keys (staticReferenceTables (programStaticReferences program))))
    ]
  where
    tableOf name = lookupStaticReferenceTable (programStaticReferences program) (FunctionName name)
    ownerOf name = staticReferenceOwner (programStaticReferences program) (FunctionName name)
    owner name = fromMaybe (FunctionName name) (ownerOf name)

program :: GrinProgram
program =
  GrinProgram
    { grinConstructors = [pubConstructor "Unit" 0 [], pubConstructor "Box" 0 [[liftedGrinRep]], pubConstructor "Counter" 0 [[IntRep]]],
      grinPrimitives = [],
      grinExternFunctions = [],
      grinExportedFunctions = [],
      grinForeignCalls = [],
      grinGlobals =
        [ pubGlobal "box" (GrinNode (GrinConstructor "Box" 0) [GrinGlobalValue "caf"]),
          pubGlobal "caf" (GrinNode (GrinThunk (FunctionName "cafEntry")) []),
          pubGlobal "closure" (GrinNode (GrinClosure (FunctionName "usesCaf") [[]]) []),
          pubGlobal "nil" (GrinNode (GrinConstructor "Unit" 0) []),
          pubGlobal "unboxed" (GrinNode (GrinConstructor "Counter" 0) [GrinLitValue (GrinLitInt IntRep 3)])
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
          function "callsTwo" [] (GrinBind [boxed "value"] (call "usesCaf") (call "usesImported")),
          function "callsSameTwice" [] (GrinBind [boxed "value"] (call "usesCaf") (call "callsUsesCaf")),
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
    call name = GrinCall liftedResultRep (FunctionName name) []
    function name parameters = GrinFunction (FunctionName name) parameters liftedResultRep

boxed :: Text -> GrinVar
boxed name = GrinVar name 0 liftedGrinRep
