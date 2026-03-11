{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestMultiAssoc ( multiAssocTests ) where

import qualified Data.Text as T
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils

import           Sample1 ( sample1 )


testInpPath :: FilePath
testInpPath = "tests/samples"

sugarCube :: CUBE
sugarCube = mkCUBE
              { rootName = "*.c"
              , expectedSuffix = "expected"
              , inputDirs = [ testInpPath ]
              , associatedNames = [ ("exe", "exe")
                                  , ("obj", "o")
                                  , ("include", "h")
                                  , ("c++-include", "hh")
                                  , ("plain", "")
                                  ]
              , validParams = [ ("arch", Just ["x86", "ppc"])
                              , ("form", Just ["base", "refined"])
                              ]
              }


multiAssocTests :: [TT.TestTree]
multiAssocTests =
  [
    -- This is simply the number of entries in sample1; if this
    -- fails in means that sample1 has been changed and the other
    -- tests here are likely to need updating.
    testCase "valid sample" $ 57 @=? length (sample1 sugarCube testInpPath)

    -- KWQ: disabled 28 June 2022: encounters a pathological case in kvitable
    -- rendering that causes this test to run ... forever (?)

    -- , testCase "sweets rendering" $
    --   let actual = sweetsTextTable [sugarCube] sugar1
    --   in do putStrLn "Table" -- try to start table on its own line
    --         putStrLn $ T.unpack actual
    --         T.length actual > 0 @? "change this to see the table"

  , sugarTestEq "correct found count" sugarCube
    (flip sample1 testInpPath) 6 length

  , testCase "results" $ do
      (sugar1, _s1desc) <- findSugarIn sugarCube (sample1 sugarCube testInpPath)
      compareBags "results" sugar1 $
        let p = (testInpPath </>)
            exp0 e a f s =
              Expectation { expectedFile = p e
                          , expParamsMatch = [("arch", a), ("form", f)]
                          , associated = s
                          }
            exp1 e a f x =
              let e1 = exp0 e a f []
              in e1 { associated = ("exe", p x) : associated e1 }
            exp2 e a f x o =
              let e1 = exp1 e a f x
              in e1 { associated = ("obj", p o) : associated e1 }
            exp2i e a f x i =
              let e1 = exp1 e a f x
              in e1 { associated = ("include", p i) : associated e1 }
            exp3 e a f x o i =
              let e1 = exp2 e a f x o
              in e1 { associated = ("include", p i) : associated e1 }
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
                  exp2 "global-max-good.ppc.expected" (e "ppc") (a "refined")
                  "global-max-good.ppc.exe"
                  "global-max-good.ppc.o"

                , exp2 "global-max-good.ppc.expected" (e "ppc") (a "base")
                  "global-max-good.ppc.exe"
                  "global-max-good.ppc.o"

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
                [ exp3 "jumpfar.ppc.expected" (e "ppc") (a "refined")
                  "jumpfar.ppc.exe"
                  "jumpfar.ppc.o"
                  "jumpfar.h"

                , exp3 "jumpfar.ppc.expected" (e "ppc") (a "base")
                  "jumpfar.ppc.exe"
                  "jumpfar.ppc.o"
                  "jumpfar.h"

                , exp2i "jumpfar.x86.expected" (e "x86") (a "refined")
                  "jumpfar.x86.exe"
                  "jumpfar.h"

                , exp2i "jumpfar.x86.expected" (e "x86") (a "base")
                  "jumpfar.x86.exe"
                  "jumpfar.h"
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
                [ exp1 "looping-around.expected" (a "x86") (a "refined")
                  "looping-around.x86.exe"

                , exp1 "looping-around.expected" (a "x86") (a "base")
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
                [ exp0 "switching.ppc.base-expected" (e "ppc") (e "base")
                  [ ("exe",         p "switching.ppc.exe")
                    -- Note: uses switching.ppc.base.o and not
                    -- switching.ppc.o--or both--because the former is a more
                    -- explicit match against the expParamsMatch.
                  , ("obj",         p "switching.ppc.base.o")
                  , ("include",     p "switching.h")
                  , ("c++-include", p "switching.hh")
                  , ("plain",       p "switching")
                  ]

                , exp0 "switching.x86.base-expected" (e "x86") (e "base")
                  [ ("exe",         p "switching.x86.exe")
                  , ("include",     p "switching.h")
                  , ("c++-include", p "switching.hh")
                  , ("plain",       p "switching")
                  ]

                , exp0 "switching.x86.refined-expected" (e "x86") (e "refined")
                  [ ("exe",         p "switching.x86.exe")
                  , ("obj",         p "switching-refined.x86.o")
                  , ("include",     p "switching.h")
                  , ("c++-include", p "switching.hh")
                  , ("plain",       p "switching")
                  ]

                ]
            }
          , Sweets
            { rootMatchName = "tailrecurse.c"
            , rootBaseName = "tailrecurse"
            , rootFile = p "tailrecurse.c"
            , cubeParams = validParams sugarCube
            , expected =
                [ exp1 "tailrecurse.expected"     (a "ppc") (a "refined")
                  "tailrecurse.ppc.exe"

                , exp1 "tailrecurse.expected"     (a "ppc") (a "base")
                  "tailrecurse.ppc.exe"

                , exp1 "tailrecurse.x86.expected" (e "x86") (a "refined")
                  "tailrecurse.x86.exe"

                , exp1 "tailrecurse.x86.expected" (e "x86") (a "base")
                  "tailrecurse.x86.exe"
                ]
            }
          ]
  ]
