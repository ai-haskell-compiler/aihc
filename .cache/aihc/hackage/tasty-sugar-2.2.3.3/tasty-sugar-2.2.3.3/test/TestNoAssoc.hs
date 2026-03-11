{-# LANGUAGE ScopedTypeVariables #-}

module TestNoAssoc ( noAssocTests, mkNoAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Runners ( testsNames )
import           Test.Tasty.Sugar
import           TestUtils

import           Sample1 ( sample1 )

testInpPath = "tests/samples"

sugarCube = mkCUBE
            { rootName = "*.c"
            , expectedSuffix = "expected"
            , inputDirs = [ testInpPath ]
            , associatedNames = []
            , validParams = [ ("arch", Just ["x86", "ppc"])
                            , ("form", Just ["base", "refined"])
                            ]
            }


noAssocTests :: [TT.TestTree]
noAssocTests =
  let chkCandidate = checkCandidate sugarCube (\c -> sample1 c testInpPath)
                     testInpPath []
  in [

       -- This is simply the number of entries in sample1; if this
       -- fails in means that sample1 has been changed and the other
       -- tests here are likely to need updating.
       testCase "valid sample" $ 57 @=? length (sample1 sugarCube testInpPath)

     , TT.testGroup "candidates"
       [
         chkCandidate 0 "global-max-good.c" []
       , chkCandidate 2 "global-max-good.ppc.o" [ ("arch", Explicit "ppc") ]
       , chkCandidate 2 "global-max-good.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 2 "global-max-good.ppc.expected" [ ("arch", Explicit "ppc") ]
       , chkCandidate 2 "global-max-good.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 2 "global-max-good.x86.expected" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "jumpfar.c" []
       , chkCandidate 0 "jumpfar.h" []
       , chkCandidate 0 "jumpfar.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "jumpfar.ppc.o" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "jumpfar.ppc.expected" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "jumpfar.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "jumpfar.x86.expected" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "looping.c" []
       , chkCandidate 0 "looping.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "looping.ppc.expected" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "looping.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "looping.x86.expected" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "looping-around.c" []
       , chkCandidate 1 "looping-around.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 1 "looping-around.ppc.expected" [ ("arch", Explicit "ppc") ]
       , chkCandidate 1 "looping-around.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "looping-around.expected" []
       , chkCandidate 0 "Makefile" []
       , chkCandidate 0 "README.org" []
       , chkCandidate 0 "switching" []
       , chkCandidate 0 "switching_stuff" []
       , chkCandidate 0 "switching.c" []
       , chkCandidate 0 "switching.h" []
       , chkCandidate 0 "switching.hh" []
       , chkCandidate 0 "switching_llvm.c" []
       , chkCandidate 0 "switching_llvm.h" []
       , chkCandidate 0 "switching_llvm.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "switching_many.c" []
       , chkCandidate 0 "switching_many_llvm.c" []
       , chkCandidate 0 "switching_many_llvm.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "switching_many.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "switching.ppc.base-expected" [ ("arch", Explicit "ppc")
                                                    , ("form", Explicit "base") ]
       , chkCandidate 0 "switching.ppc.o" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "switching.ppc.base.o" [ ("arch", Explicit "ppc")
                                             , ("form", Explicit "base") ]
       , chkCandidate 0 "switching.ppc.extra.o" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "switching.ppc.other" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "switching.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "switching.x86.base-expected" [ ("arch", Explicit "x86")
                                                    , ("form", Explicit "base") ]
       , chkCandidate 0 "switching.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "switching-refined.x86.o" [ ("arch", Explicit "x86")
                                                , ("form", Explicit "refined") ]
       , chkCandidate 0 "switching.x86.orig" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "switching.x86.refined-expected" [ ("arch", Explicit "x86")
                                                       , ("form", Explicit "refined") ]
       , chkCandidate 0 "switching.x86.refined-expected-orig" [ ("arch", Explicit "x86")
                                                            , ("form", Explicit "refined") ]
       , chkCandidate 0 "switching.x86.refined-last-actual" [ ("arch", Explicit "x86")
                                                          , ("form", Explicit "refined") ]
       , chkCandidate 0 "tailrecurse.c" []
       , chkCandidate 0 "tailrecurse.expected" []
       , chkCandidate 0 "tailrecurse.expected.expected" []
       , chkCandidate 0 "tailrecurse.food.expected" []
       , chkCandidate 0 "tailrecurse.ppc.exe" [ ("arch", Explicit "ppc") ]
       , chkCandidate 0 "tailrecurse.x86.exe" [ ("arch", Explicit "x86") ]
       , chkCandidate 0 "tailrecurse.x86.expected" [ ("arch", Explicit "x86") ]
       ]

     , sugarTestEq "correct found count" sugarCube
       (flip sample1 testInpPath) 6 length

     , testCase "results" $
       let p = (testInpPath </>)
           exp0 e a f =
             Expectation { expectedFile = p e
                         , expParamsMatch = [("arch", a), ("form", f)]
                         , associated = []
                         }
           e = Explicit
           a = Assumed
       in do
         (sugar1, _desc) <- findSugarIn sugarCube (sample1 sugarCube testInpPath)
         compareBags "results" sugar1
           [
             Sweets
             { rootMatchName = "global-max-good.c"
             , rootBaseName = "global-max-good"
             , rootFile = p "global-max-good.c"
             , cubeParams = validParams sugarCube
             , expected =
                 [
                   exp0 "global-max-good.ppc.expected" (e "ppc") (a "refined")
                 , exp0 "global-max-good.ppc.expected" (e "ppc") (a "base")
                 , exp0 "global-max-good.x86.expected" (e "x86") (a "refined")
                 , exp0 "global-max-good.x86.expected" (e "x86") (a "base")
                 ]
             }

           , Sweets
             { rootMatchName = "jumpfar.c"
             , rootBaseName = "jumpfar"
             , rootFile = p "jumpfar.c"
             , cubeParams = validParams sugarCube
             , expected =
                 [ exp0 "jumpfar.ppc.expected" (e "ppc") (a "refined")
                 , exp0 "jumpfar.ppc.expected" (e "ppc") (a "base")
                 , exp0 "jumpfar.x86.expected" (e "x86") (a "refined")
                 , exp0 "jumpfar.x86.expected" (e "x86") (a "base")
                 ]
             }
           , Sweets
             { rootMatchName = "looping.c"
             , rootBaseName = "looping"
             , rootFile = p "looping.c"
             , cubeParams = validParams sugarCube
             , expected =
                 [ exp0 "looping.ppc.expected" (e "ppc") (a "refined")
                 , exp0 "looping.ppc.expected" (e "ppc") (a "base")
                 , exp0 "looping.x86.expected" (e "x86") (a "refined")
                 , exp0 "looping.x86.expected" (e "x86") (a "base")
                 ]
             }
           , Sweets
             { rootMatchName = "looping-around.c"
             , rootBaseName = "looping-around"
             , rootFile = p "looping-around.c"
             , cubeParams = validParams sugarCube
             , expected =
                 [ exp0 "looping-around.expected"     (a "x86") (a "refined")
                 , exp0 "looping-around.expected"     (a "x86") (a "base")
                 , exp0 "looping-around.ppc.expected" (e "ppc") (a "refined")
                 , exp0 "looping-around.ppc.expected" (e "ppc") (a "base")
                 ]
             }
           , Sweets
             { rootMatchName = "switching.c"
             , rootBaseName = "switching"
             , rootFile = p "switching.c"
             , cubeParams = validParams sugarCube
             , expected =
                 [ exp0 "switching.ppc.base-expected"    (e "ppc") (e "base")
                 , exp0 "switching.x86.base-expected"    (e "x86") (e "base")
                 , exp0 "switching.x86.refined-expected" (e "x86") (e "refined")
                 ]
             }
           , Sweets
             { rootMatchName = "tailrecurse.c"
             , rootBaseName = "tailrecurse"
             , rootFile = p "tailrecurse.c"
             , cubeParams = validParams sugarCube
             , expected =
                 [ exp0 "tailrecurse.expected" (a "ppc") (a "refined")
                 , exp0 "tailrecurse.expected" (a "ppc") (a "base")
                 , exp0 "tailrecurse.x86.expected" (e "x86") (a "refined")
                 , exp0 "tailrecurse.x86.expected" (e "x86") (a "base")
                 ]
             }
           ]
     ]


mkNoAssocTests :: IO [TT.TestTree]
mkNoAssocTests = do
  (sugar1, _desc) <- findSugarIn sugarCube $ sample1 sugarCube testInpPath
  tt <- withSugarGroups sugar1 TT.testGroup $
          \sw idx exp ->
            -- Verify this will suppress the "refined" tests for
            -- "looping-around.c"
            if (rootMatchName sw == "looping-around.c" &&
                Just (Assumed "refined") == lookup "form" (expParamsMatch exp))
            then (return [])
            else return [ testCase (rootMatchName sw <> "." <> show idx) $
                          return ()
                        ]
  return $
    (testCase "generated count" $ 21 @=? length (concatMap (testsNames mempty) tt))
    : tt
