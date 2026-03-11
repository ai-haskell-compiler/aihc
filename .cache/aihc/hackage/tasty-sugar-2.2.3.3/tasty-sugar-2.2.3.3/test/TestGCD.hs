{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestGCD ( gcdSampleTests ) where

import qualified Data.Text as T
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ

import           Prelude hiding ( exp )


testInpPath = "test-data/samples"

testParams = [ ("solver", Just ["z3", "yices", "boolector", "cvc4"])
             , ("loop-merging", Just ["loopmerge", "loop"])
             ]

sugarCube = mkCUBE
            { rootName = "*.c"
            , expectedSuffix = "good"
            , inputDirs = [ testInpPath ]
            , associatedNames = [ ("config", "config")
                                , ("stdio", "print")
                                , ("haskell", "hs")
                                ]
            , validParams = testParams
            }

gcdSampleTests :: [TT.TestTree]
gcdSampleTests =
  [ testCase "valid sample" $ 19 @=? length (gcdSamples sugarCube)
  , sugarTestEq "correct found count" sugarCube gcdSamples 1 length

  , testCase "sweets rendering" $ do
      (sugar,_sdesc) <- findSugarIn sugarCube (gcdSamples sugarCube)
      let actual = sweetsTextTable [sugarCube] sugar
      -- putStrLn "Table" -- try to start table on its own line
      -- putStrLn $ T.unpack actual
      T.length actual > 0 @? "change this to see the table"

  , testCaseSteps "sweets info" $ \step -> do
      (sugar,_sdesc) <- findSugarIn sugarCube (gcdSamples sugarCube)
      step "rootMatchName"
      (rootMatchName <$> sugar) @?= ["gcd-test.c"]
      step "rootBaseName"
      (rootBaseName <$> sugar) @?= ["gcd-test"]
      step "rootFile"
      (rootFile <$> sugar) @?= [ testInpPath </> "gcd-test.c" ]
      step "cubeParams"
      (cubeParams <$> sugar) @?= [ validParams sugarCube ]

  , testCase "Expectations" $ do
      (sugar,_sdesc) <- findSugarIn sugarCube (gcdSamples sugarCube)
      compareBags "expected" (expected $ head sugar) $
        let p = (testInpPath </>)
            exp e s l c o =
              Expectation
              { expectedFile = p e
              , expParamsMatch = [("solver", s), ("loop-merging", l)]
              , associated = [ ("config", p c), ("stdio", p o)]
              }
            e = Explicit
            a = Assumed
        in
          [
            exp "gcd-test.boolector.good" (e "boolector") (a "loopmerge")
                "gcd-test.loopmerge.config"
                "gcd-test.boolector.print"

          , exp "gcd-test.boolector.good" (e "boolector") (a "loop")
                "gcd-test.config"
                "gcd-test.boolector.print"

          , exp "gcd-test.good"           (a "cvc4")      (a "loopmerge")
                "gcd-test.loopmerge.config"
                "gcd-test.print"

          , exp "gcd-test.good"           (a "cvc4")      (a "loop")
                "gcd-test.config"
                "gcd-test.print"

          , exp "gcd-test.good"           (a "yices")     (a "loopmerge")
                "gcd-test.loopmerge.config"
                "gcd-test.print"

          , exp "gcd-test.good"           (a "yices")     (a "loop")
                "gcd-test.config"
                "gcd-test.print"

          , exp "gcd-test.good"           (a "z3")        (a "loopmerge")
                "gcd-test.loopmerge.config"
                "gcd-test.print"

          , exp "gcd-test.good"           (a "z3")        (a "loop")
                "gcd-test.config"
                "gcd-test.print"

          ]
  ]

gcdSamples cube = fmap (makeCandidate cube testInpPath [])
                  $ filter (not . null)
                  $ lines [r|
gcd-test.boolector.boolector.out
gcd-test.boolector.boolector.print.out
gcd-test.boolector.good
gcd-test.boolector.print
gcd-test.c
gcd-test.config
gcd-test.cvc4.out
gcd-test.cvc4.print.out
gcd-test.good
gcd-test.loopmerge.config
gcd-test.print
gcd-test.stp.good
gcd-test.stp.print
gcd-test.stp.stp.out
gcd-test.stp.stp.print.out
gcd-test.yices.out
gcd-test.yices.print.out
gcd-test.z3.out
gcd-test.z3.print.out
|]
