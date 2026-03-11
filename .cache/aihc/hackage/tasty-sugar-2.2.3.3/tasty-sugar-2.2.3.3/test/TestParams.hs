module TestParams ( paramTests ) where

import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils


testInpPath = "/test/data"


-- | These tests show the simple progression of parameter matching in expected
-- files, starting with a base where no parameters are matched and so everything
-- is assumed, through intermediate cases where some of the parameters are
-- matched (and therefore explicit) through to the end case where every parameter
-- match is present and explicit.

paramTests :: [TT.TestTree]
paramTests =
  let cube = mkCUBE { inputDirs = [ testInpPath ]
                      , rootName = "*.inp"
                      , separators = "-."
                      , expectedSuffix = "exp"
                      , validParams = [ ("p1", Just [ "one", "two", "three" ]) ]
                      }
      chkExp res swNum expNum expF p1Match =
        safeElem expNum .expected <$> safeElem swNum res @?=
        (Just $ Just $ Expectation { expectedFile = expF
                                   , expParamsMatch = [ ("p1", p1Match) ]
                                   , associated = []
                                   })
      expbase = testInpPath <> "/first.exp"
      expone = testInpPath <> "/first-one.exp"
      exptwo = testInpPath <> "/first-two.exp"
      expthree = testInpPath <> "/first-three.exp"

      checkTheStandardThings op =
        [
          testCase "valid # results" $ do
            (res, _) <- op
            length res @?= 1

        , testCase "rootMatchName" $ do
            (res, _) <- op
            rootMatchName <$> (safeElem 0 res) @?= Just "first.inp"

        , testCase "rootBaseName" $ do
            (res, _) <- op
            rootBaseName <$> (safeElem 0 res) @?= Just "first"

        , testCase "rootFile" $ do
            (res, _) <- op
            rootFile <$> (safeElem 0 res) @?= Just "/test/data/first.inp"

        , testCase "cubeParams" $ do
            (res, _) <- op
            cubeParams <$> (safeElem 0 res) @?=
              Just [ ("p1", Just [ "one", "two", "three" ]) ]

        , testCase "num expecteds" $ do
            (res, _) <- op
            length . expected <$> safeElem 0 res @?= Just 3
        ]

  in [ TT.testGroup "no matching params" $

       let sample = (makeCandidate cube testInpPath []) <$>
                    [ "first.inp"
                    , "first.exp"
                    ]
       in do
         checkTheStandardThings (findSugarIn cube sample)
          -- No parameters match, all expectations are against the base file and
          -- all parameter values are assumed.
          <> [ testCase "expected 2" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 2 expbase (Assumed "one")
             , testCase "expected 0" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 0 expbase (Assumed "two")
             , testCase "expected 1" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 1 expbase (Assumed "three")
             ]

       ----------------------------------------------------------------------

     , TT.testGroup "one matching param" $

       let sample = (makeCandidate cube testInpPath []) <$>
                    [ "first.inp"
                    , "first.exp"
                    , "first-one.exp"
                    ]
       in do
         checkTheStandardThings (findSugarIn cube sample)
          -- One parameter matches a specific file which is therefore explicit,
          -- all other expectations are against the base file and their parameter
          -- values are assumed.
          <> [ testCase "expected 0" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 0 expone (Explicit "one")
             , testCase "expected 1" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 1 expbase (Assumed "two")
             , testCase "expected 2" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 2 expbase (Assumed "three")
             ]

       ----------------------------------------------------------------------

     , TT.testGroup "all matching params" $

       let sample = (makeCandidate cube testInpPath []) <$>
                    [ "first.inp"
                    , "first.exp"
                    , "first-one.exp"
                    , "first-two.exp"
                    , "first-three.exp"
                    ]
       in do
         checkTheStandardThings (findSugarIn cube sample)
          -- All parameters match a specific expected file and are explicit.  The
          -- base expected file is never matched because the explicit matches are
          -- more precise.
          <> [ testCase "expected 0" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 0 expone (Explicit "one")
             , testCase "expected 2" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 2 exptwo (Explicit "two")
             , testCase "expected 1" $ do
                 ( sugar, _desc ) <- findSugarIn cube sample
                 chkExp sugar 0 1 expthree (Explicit "three")
             ]

     ]
