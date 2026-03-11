{-# LANGUAGE ScopedTypeVariables #-}

module TestSingleAssoc ( singleAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils

import           Sample1 ( sample1 )


testInpPath = "test/samples"

sugarCube = mkCUBE
            { rootName = "*.c"
            , expectedSuffix = "expected"
            , inputDirs = [ testInpPath ]
            , associatedNames = [ ("exe", "exe") ]
            , validParams = [ ("arch", Just ["x86", "ppc"])
                            , ("form", Just ["base", "refined"])
                            ]
            }

singleAssocTests :: [TT.TestTree]
singleAssocTests =
  [

    -- This is simply the number of entries in sample1; if this
    -- fails in means that sample1 has been changed and the other
    -- tests here are likely to need updating.
    testCase "valid sample" $ 57 @=? length (sample1 sugarCube testInpPath)

  , sugarTestEq "correct found count" sugarCube
    (flip sample1 testInpPath) 6 length

  , testCase "results" $ do
      (sugar1,_s1desc) <- findSugarIn sugarCube (sample1 sugarCube testInpPath)
      compareBags "results" sugar1 $
        let p = (testInpPath </>)
            exp1 e a f x =
              Expectation { expectedFile = p e
                          , expParamsMatch = [("arch", a), ("form", f)]
                          , associated = [("exe", p x)]
                          }
            e = Explicit
            a = Assumed
        in
          [
            Sweets
            { rootMatchName = "global-max-good.c"
            , rootBaseName = "global-max-good"
            , rootFile = p "global-max-good.c"
            , cubeParams = validParams sugarCube
            , expected =
                [
                  exp1 "global-max-good.ppc.expected" (e "ppc") (a "refined")
                  "global-max-good.ppc.exe"

                , exp1 "global-max-good.ppc.expected" (e "ppc") (a "base")
                  "global-max-good.ppc.exe"

                , exp1 "global-max-good.x86.expected" (e "x86") (a "refined")
                  "global-max-good.x86.exe"

                , exp1 "global-max-good.x86.expected" (e "x86") (a "base")
                  "global-max-good.x86.exe"
                ]
            }

          , Sweets
            { rootMatchName = "jumpfar.c"
            , rootBaseName = "jumpfar"
            , rootFile = p "jumpfar.c"
            , cubeParams = validParams sugarCube
            , expected =
                [ exp1 "jumpfar.ppc.expected" (e "ppc") (a "refined")
                  "jumpfar.ppc.exe"

                , exp1 "jumpfar.ppc.expected" (e "ppc") (a "base")
                  "jumpfar.ppc.exe"

                , exp1 "jumpfar.x86.expected" (e "x86") (a "refined")
                  "jumpfar.x86.exe"

                , exp1 "jumpfar.x86.expected" (e "x86") (a "base")
                  "jumpfar.x86.exe"
                ]
            }
          , Sweets
            { rootMatchName = "looping.c"
            , rootBaseName = "looping"
            , rootFile = p "looping.c"
            , cubeParams = validParams sugarCube
            , expected =
                [ exp1 "looping.ppc.expected" (e "ppc") (a "refined")
                  "looping.ppc.exe"

                , exp1 "looping.ppc.expected" (e "ppc") (a "base")
                  "looping.ppc.exe"

                , exp1 "looping.x86.expected" (e "x86") (a "refined")
                  "looping.x86.exe"

                , exp1 "looping.x86.expected" (e "x86") (a "base")
                  "looping.x86.exe"
                ]
            }
          , Sweets
            { rootMatchName = "looping-around.c"
            , rootBaseName = "looping-around"
            , rootFile = p "looping-around.c"
            , cubeParams = validParams sugarCube
            , expected =
                [ exp1 "looping-around.expected"     (a "x86") (a "refined")
                  "looping-around.x86.exe"

                , exp1 "looping-around.expected"     (a "x86") (a "base")
                  "looping-around.x86.exe"

                , exp1 "looping-around.ppc.expected" (e "ppc") (a "refined")
                  "looping-around.ppc.exe"

                , exp1 "looping-around.ppc.expected" (e "ppc") (a "base")
                  "looping-around.ppc.exe"
                ]
            }
          , Sweets
            { rootMatchName = "switching.c"
            , rootBaseName = "switching"
            , rootFile = p "switching.c"
            , cubeParams = validParams sugarCube
            , expected =
                [ exp1 "switching.ppc.base-expected"    (e "ppc") (e "base")
                  "switching.ppc.exe"

                , exp1 "switching.x86.base-expected"    (e "x86") (e "base")
                  "switching.x86.exe"

                , exp1 "switching.x86.refined-expected" (e "x86") (e "refined")
                  "switching.x86.exe"
                ]
            }
          , Sweets
            { rootMatchName = "tailrecurse.c"
            , rootBaseName = "tailrecurse"
            , rootFile = p "tailrecurse.c"
            , cubeParams = validParams sugarCube
            , expected =
                [ exp1 "tailrecurse.expected" (a "ppc") (a "refined")
                  "tailrecurse.ppc.exe"

                , exp1 "tailrecurse.expected" (a "ppc") (a "base")
                  "tailrecurse.ppc.exe"

                , exp1 "tailrecurse.x86.expected" (e "x86") (a "refined")
                  "tailrecurse.x86.exe"

                , exp1 "tailrecurse.x86.expected" (e "x86") (a "base")
                  "tailrecurse.x86.exe"
                ]
            }
          ]
  ]
