{-# LANGUAGE LambdaCase #-}
module TestLLVMRange ( llvmRangeTests ) where

import           Control.Applicative
import           Control.Monad.Trans.Writer
import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe
import           System.FilePath ( (</>), takeFileName )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.Read ( readMaybe )


testInpPath :: FilePath
testInpPath = "test/data/llvm1"

files :: CUBE -> [CandidateFile]
files cube = makeCandidate cube testInpPath []
             <$> [ "T847-fail2.c"
                 , "T847-fail2.cvc5.good"
                 , "T847-fail2.cvc5.pre-clang13.good"
                 , "T847-fail2.ll"
                 , "T847-fail2.pre-clang12.z3.good"
                 , "T847-fail2.z3.good"
                 , "T972-fail.c"
                 , "T972-fail.pre-clang12.z3.good"
                 , "T972-fail.pre-clang14.z3.good"
                 , "T972-fail.z3.good"
                 , "abd-test-file-32.c"
                 , "abd-test-file-32.config"
                 , "abd-test-file-32.cvc5.good"
                 , "abd-test-file-32.pre-clang13.cvc5.good"
                 , "freeze.c"
                 , "freeze.ll"
                 , "freeze.pre-clang12.z3.good"
                 , "freeze.z3.good"
                 , "shrink.c"
                 , "shrink.config"
                 , "shrink.ll"
                 , "shrink.z3.good"
                 ]

files2 :: CUBE -> [CandidateFile]
files2 cube = makeCandidate cube testInpPath []
              <$> [ "T847-fail2.c"
                  , "T847-fail2.cvc5.good"
                  , "T847-fail2.cvc5.clang13+.good"
                  , "T847-fail2.ll"
                  , "T847-fail2.clang12+.z3.good"
                  , "T847-fail2.z3.good"
                  , "T972-fail.c"
                  , "T972-fail.clang12+.clang14+.z3.good"
                  , "T972-fail.z3.good"
                  , "abd-test-file-32.c"
                  , "abd-test-file-32.config"
                  , "abd-test-file-32.cvc5.good"
                  , "abd-test-file-32.clang13+.cvc5.good"
                  , "freeze.c"
                  , "freeze.ll"
                  , "freeze.clang12+.z3.good"
                  , "freeze.z3.good"
                  , "shrink.c"
                  , "shrink.config"
                  , "shrink.ll"
                  , "shrink.z3.good"
                  ]


llvmRangeTests :: IO [TT.TestTree]
llvmRangeTests = do tsts1 <- sequence [ llvmRange1 ("direct", Nothing)
                                      , llvmRange1 ("ranged", Nothing)
                                      , llvmRange1 ("ranged", Just 9)
                                      , llvmRange1 ("ranged", Just 11)
                                      , llvmRange1 ("ranged", Just 12)
                                      , llvmRange1 ("ranged", Just 14)
                                      , llvmRange1 ("ranged", Just 16)
                                      ]
                    tsts2 <- sequence [ llvmRange2 ("direct", Nothing)
                                      , llvmRange2 ("ranged", Just 9)
                                      , llvmRange2 ("ranged", Just 11)
                                      , llvmRange2 ("ranged", Just 12)
                                      , llvmRange2 ("ranged", Just 14)
                                      , llvmRange2 ("ranged", Just 16)
                                      ]
                    tsts3 <- sequence [ llvmRange3 ("direct", Nothing)
                                      , llvmRange3 ("ranged", Just 9)
                                      , llvmRange3 ("ranged", Just 11)
                                      , llvmRange3 ("ranged", Just 12)
                                      , llvmRange3 ("ranged", Just 13)
                                      , llvmRange3 ("ranged", Just 14)
                                      , llvmRange3 ("ranged", Just 16)
                                      ]
                    return [ TT.testGroup "LLVM Range 1" $ concat tsts1
                           , TT.testGroup "LLVM Range 2" $ concat tsts2
                           , TT.testGroup "LLVM Range 3" $ concat tsts3
                           ]

llvmRange1 :: (String, Maybe Int) -> IO [TT.TestTree]
llvmRange1 (mode, matchClang) = do
  let cube =
        let c = mkCUBE { inputDirs = [ testInpPath ]
                       , rootName = "*.c"
                       , expectedSuffix = "good"
                       , separators = "."
                       , validParams = [ ("solver", Just [ "z3", "cvc5" ])
                                       , ("clang-range", Just [ "recent-clang"
                                                              , "pre-clang11"
                                                              , "pre-clang12"
                                                              , "pre-clang13"
                                                              , "pre-clang14"
                                                              ])
                                       ]
                       }
                in case mode of
                     "direct" -> c
                     "ranged" ->
                       let extract pval =
                             if "pre-clang" `L.isPrefixOf` pval
                             then readMaybe (drop (length "pre-clang") pval)
                             else if "recent-clang" == pval
                                  then Nothing :: Maybe Int
                                  else error $ "Unknown parameter value for range testing: " <> pval
                       in c { sweetAdjuster =
                                rangedParamAdjuster "clang-range" extract
                                (<) matchClang
                            }
  (sweets, _) <- findSugarIn cube (files cube)
  -- putStrLn $ show sweets

  (tests, calls) <-
    runWriterT
    $ withSugarGroups sweets TT.testGroup $ \sweet instnum exp ->
    do tell [1]
       let exps = mkTest1 mode matchClang sweet exp
       return
         [ testCase ("instnum valid: " <> show instnum)
                      (instnum `elem` [1] @?
                       ("unexpected instnum for exp " <> show exp))
         , testCase "# expectations" (length (expected sweet) @?= length exps)
         , testCase "passed exp" (exp `elem` exps @? "Unexpected: " <> show exp)
         ]


  return
    [ TT.testGroup (mode <> " clang " <> maybe "not-specified" show matchClang)
      $ testCase "correct # of sweets" (length sweets @?= 5)
      : testCase "correct # processed via withSugarGroups"
        -- without ranged parameter handling, will call for every Expectation,
        -- but if ranged, only calls with the appropriate Expectation for that
        -- value.
        (sum calls @?=
          let expNothingCalls = if mode == "direct" then 5*6 else 22
          in maybe expNothingCalls (const 6) matchClang)
      : tests
    ]


-- llvmRange2 is like llvmRange1 except there is no "default" match of
-- "recent-clang" in the validParams of the CUBE, so there will be no
-- corresponding default expectations.

llvmRange2 :: (String, Maybe Int) -> IO [TT.TestTree]
llvmRange2 (mode, matchClang) = do
  let cube = mkCUBE { inputDirs = [ testInpPath ]
                    , rootName = "*.c"
                    , expectedSuffix = "good"
                    , separators = "."
                    , validParams = [ ("solver", Just [ "z3", "cvc5" ])
                                    , ("clang-range", Just [ "pre-clang11"
                                                           , "pre-clang12"
                                                           , "pre-clang13"
                                                           , "pre-clang14"
                                                           ])
                                    ]
                    , sweetAdjuster =
                        if mode == "ranged"
                        then
                          let extract paramVal =
                                if "pre-clang" `L.isPrefixOf` paramVal
                                then readMaybe (drop (length "pre-clang") paramVal)
                                else if "recent-clang" == paramVal
                                     then Nothing :: Maybe Int
                                     else error $ "Unknown parameter value for range testing: " <> paramVal
                          in rangedParamAdjuster "clang-range" extract (<) matchClang
                        else const return
                    }
  (sweets,_) <- findSugarIn cube (files cube)
  -- putStrLn $ show sweets

  (tests, calls) <-
    runWriterT
    $ withSugarGroups sweets TT.testGroup $ \sweet instnum exp ->
    do tell [1]
       let exps =
             let baseExps = mkTest1 mode matchClang sweet exp
             in case mode of
                  "direct" ->
                    -- Remove the first of the expected matches from the base,
                    -- which (by convention) is the default match.  Also, for
                    -- T847-fail2.c there are two defaults, the second is the 5th
                    -- original entry.
                    tail $ if rootMatchName sweet == "T847-fail2.c"
                           then take 5 baseExps <> drop 6 baseExps
                           else baseExps
                  "ranged" ->
                    -- Since there's no default, there are no expectations
                    -- generated if the local clang version is 14 or above
                    -- (pre-clang14 is the highest valid parameter value)
                    case matchClang of
                      Just 14 -> []
                      Just 16 -> []
                      _ -> baseExps

       return
         [ testCase ("instnum valid: " <> show instnum)
                      (instnum `elem` [1] @? "unexpected instnum")
         , testCase "# expectations" (length (expected sweet) @?= length exps)
         , testCase "passed exp" (exp `elem` exps @? "Unexpected: " <> show exp)
         ]

  return
    [ TT.testGroup (mode <> " clang " <> maybe "not-specified" show matchClang)
      $ testCase "correct # of sweets" (length sweets @?= 5)
      : testCase "correct # processed via withSugarGroups"
        -- without ranged parameter handling, will call for every Expectation,
        -- but if ranged, only calls with the appropriate Expectation for that
        -- value.
        (sum calls @?=
          maybe (4*6)
          (const $ case matchClang of
                     Just 14 -> 0
                     Just 16 -> 0
                     _ -> 6)
          matchClang)
      : tests
    ]


-- The llvmRange3 tests are similar to the llvmRange1 tests, except that it
-- inverts the range limits: llvmRange1 specified exclusive upper bounds
-- (e.g. pre-clang11) whereas llvmRange3 flips to inclusive lower bounds
-- (e.g. clang11+).

llvmRange3 :: (String, Maybe Int) -> IO [TT.TestTree]
llvmRange3 (mode, matchClang) = do
  let cube0 = mkCUBE { inputDirs = [ testInpPath ]
                    , rootName = "*.c"
                    , expectedSuffix = "good"
                    , separators = "."
                    , validParams = [ ("solver", Just [ "z3", "cvc5" ])
                                    , ("clang-range", Just [ "older-clang"
                                                           , "clang11+"
                                                           , "clang12+"
                                                           , "clang13+"
                                                           , "clang14+"
                                                           ])
                                    ]
                    }
  let cube = if mode == "ranged"
             then let e = readMaybe . drop (length "clang") . init
                  in cube0
                     {
                       sweetAdjuster =
                         rangedParamAdjuster "clang-range" e (>=) matchClang
                     }
             else cube0
  (sweets,_) <- findSugarIn cube (files2 cube)
  -- putStrLn $ show sweets

  (tests, calls) <-
    runWriterT
    $ withSugarGroups sweets TT.testGroup $ \sweet instnum exp ->
    do tell [1]
       let exps = mkTest2 mode matchClang sweet exp
       return
         [ testCase ("instnum valid: " <> show instnum)
                      (instnum `elem` [1] @? "unexpected instnum")
         , testCase "# expectations" (length (expected sweet) @?= length exps)
         , testCase "passed exp" (exp `elem` exps @? "Unexpected: " <> show exp)
         ]


  return
    [ TT.testGroup (mode <> " clang " <> maybe "not-specified" show matchClang)
      $ testCase "correct # of sweets" (length sweets @?= 5)
      : testCase "correct # processed via withSugarGroups"
        -- without ranged parameter handling, will call for every Expectation,
        -- but if ranged, only calls with the appropriate Expectation for that
        -- value.
        (sum calls @?= maybe (5*6) (const 6) matchClang)
      : tests
    ]


mkTest1 mode matchClang sweet exp =
  let exp0 f s c = Expectation
                 { expectedFile = testInpPath </> f
                 , expParamsMatch = [ ("solver", s)
                                    , ("clang-range", c)
                                    ]
                 , associated = []
                 }
      expA f s l = exp0 f (Explicit s) (Assumed l)
      expE f s l = exp0 f (Explicit s) (Explicit l)

      exps =
          case rootMatchName sweet of
            "shrink.c" ->
              let def = [ expA "shrink.z3.good" "z3" "recent-clang"
                        , expA "shrink.z3.good" "z3" "pre-clang14"
                        , expA "shrink.z3.good" "z3" "pre-clang13"
                        , expA "shrink.z3.good" "z3" "pre-clang12"
                        , expA "shrink.z3.good" "z3" "pre-clang11"
                        ]
              in case mode of
                   "direct" -> def
                   "ranged" ->
                     -- Only run the expectation that fits the matching version
                     -- of clang that is present.  There are no Explicit matches,
                     -- so this should use the best fallback (Assumed) match.
                     case matchClang of
                       Just 9 ->
                         [ expA "shrink.z3.good" "z3" "pre-clang11"
                         ]
                       Just 11 ->
                         [ expA "shrink.z3.good" "z3" "pre-clang12"
                         ]
                       Just 12 ->
                         [ expA "shrink.z3.good" "z3" "pre-clang13"
                         ]
                       Just 14 ->
                         [ expA "shrink.z3.good" "z3" "recent-clang"
                         ]
                       Just 16 ->
                         [ expA "shrink.z3.good" "z3" "recent-clang"
                         ]
                       Nothing ->
                         let e = expA "shrink.z3.good" "z3" in
                         [ e "recent-clang" -- presumed clang=15, clang14, nothing
                         , e "pre-clang14" -- presumed clang=13
                         , e "pre-clang13" -- presumed clang=12
                         , e "pre-clang12" -- presumed clang=11
                         , e "pre-clang11" -- presumed clang=10
                         ]

            "freeze.c" ->
              case mode of
                "direct" ->
                  [ expA "freeze.z3.good" "z3" "recent-clang"
                  , expA "freeze.z3.good" "z3" "pre-clang14"
                  , expA "freeze.z3.good" "z3" "pre-clang13"
                  , expE "freeze.pre-clang12.z3.good" "z3" "pre-clang12"
                  , expA "freeze.z3.good" "z3" "pre-clang11"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expE "freeze.pre-clang12.z3.good" "z3" "pre-clang12" ]
                    Just 11 ->
                      [ expE "freeze.pre-clang12.z3.good" "z3" "pre-clang12" ]
                    Just 12 ->
                      [ expA "freeze.z3.good" "z3" "pre-clang13" ]
                    Just 14 ->
                      [ expA "freeze.z3.good" "z3" "recent-clang" ]
                    Just 16 ->
                      [ expA "freeze.z3.good" "z3" "recent-clang" ]
                    Nothing ->
                      let e = expA "freeze.z3.good" "z3" in
                      [ e "recent-clang" -- presumed clang=15, clang=14, nothing
                      , e "pre-clang14" -- presumed clang=13
                      , e "pre-clang13" -- presumed clang=12
                      , expE "freeze.pre-clang12.z3.good" "z3" "pre-clang12"
                        -- presumed clang=10, clang=11
                      ]

            "T847-fail2.c" ->
              case mode of
                "direct" ->
                  [ expA "T847-fail2.z3.good" "z3" "recent-clang"
                  , expA "T847-fail2.z3.good" "z3" "pre-clang14"
                  , expA "T847-fail2.z3.good" "z3" "pre-clang13"
                  , expE "T847-fail2.pre-clang12.z3.good" "z3" "pre-clang12"
                  , expA "T847-fail2.z3.good" "z3" "pre-clang11"

                  , expA "T847-fail2.cvc5.good" "cvc5" "recent-clang"
                  , expA "T847-fail2.cvc5.good" "cvc5" "pre-clang14"
                  , expE "T847-fail2.cvc5.pre-clang13.good" "cvc5" "pre-clang13"
                  , expA "T847-fail2.cvc5.good" "cvc5" "pre-clang12"
                  , expA "T847-fail2.cvc5.good" "cvc5" "pre-clang11"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expE "T847-fail2.pre-clang12.z3.good" "z3" "pre-clang12"
                      , expE "T847-fail2.cvc5.pre-clang13.good" "cvc5" "pre-clang13"
                      ]
                    Just 11 ->
                      [ expE "T847-fail2.pre-clang12.z3.good" "z3" "pre-clang12"
                      , expE "T847-fail2.cvc5.pre-clang13.good" "cvc5" "pre-clang13"
                      ]
                    Just 12 ->
                      [ expA "T847-fail2.z3.good" "z3" "pre-clang13"
                      , expE "T847-fail2.cvc5.pre-clang13.good" "cvc5" "pre-clang13"
                      ]
                    Just 14 ->
                      [ expA "T847-fail2.z3.good" "z3" "recent-clang"
                      , expA "T847-fail2.cvc5.good" "cvc5" "recent-clang"
                      ]
                    Just 16 ->
                      [ expA "T847-fail2.z3.good" "z3" "recent-clang"
                      , expA "T847-fail2.cvc5.good" "cvc5" "recent-clang"
                      ]
                    Nothing ->
                      let e1 = expA "T847-fail2.z3.good" "z3"
                          e2 = expA "T847-fail2.cvc5.good" "cvc5"
                      in
                        [ e1 "recent-clang" -- presumed clang=15, clang=14, nothing
                        , e1 "pre-clang14" -- presumed clang=13
                        , e1 "pre-clang13" -- presumed clang=12
                        , expE "T847-fail2.pre-clang12.z3.good" "z3" "pre-clang12"
                          -- presumed clang=11, clang=10

                        , e2 "recent-clang" -- presumed clang=15, clang=14, nothing
                        , e2 "pre-clang14" -- presumed clang=13
                        , expE "T847-fail2.cvc5.pre-clang13.good" "cvc5" "pre-clang13"
                          -- presumed clang=12, clang=11, clang=10
                        ]

            "T972-fail.c" ->
              case mode of
                "direct" ->
                  [ expA "T972-fail.z3.good" "z3" "recent-clang"
                  , expE "T972-fail.pre-clang14.z3.good" "z3" "pre-clang14"
                  , expA "T972-fail.z3.good" "z3" "pre-clang13"
                  , expE "T972-fail.pre-clang12.z3.good" "z3" "pre-clang12"
                  , expA "T972-fail.z3.good" "z3" "pre-clang11"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expE "T972-fail.pre-clang12.z3.good" "z3" "pre-clang12"
                      ]
                    Just 11 ->
                      [ expE "T972-fail.pre-clang12.z3.good" "z3" "pre-clang12"
                      ]
                    Just 12 ->
                      [ expE "T972-fail.pre-clang14.z3.good" "z3" "pre-clang14"
                      ]
                    Just 14 ->
                      [ expA "T972-fail.z3.good" "z3" "recent-clang"
                      ]
                    Just 16 ->
                      [ expA "T972-fail.z3.good" "z3" "recent-clang"
                      ]
                    Nothing ->
                      let e = expA "T972-fail.z3.good" "z3" in
                        [ e "recent-clang" -- presumed clang=15, clang=14, nothing
                        , expE "T972-fail.pre-clang14.z3.good" "z3" "pre-clang14"
                          -- presumed clang=13, clang=12
                        , expE "T972-fail.pre-clang12.z3.good" "z3" "pre-clang12"
                          -- presumed clang=11, clang=10
                        ]

            "abd-test-file-32.c" ->
              case mode of
                "direct" ->
                  [ expA "abd-test-file-32.cvc5.good" "cvc5" "recent-clang"
                  , expA "abd-test-file-32.cvc5.good" "cvc5" "pre-clang14"
                  , expE "abd-test-file-32.pre-clang13.cvc5.good" "cvc5" "pre-clang13"
                  , expA "abd-test-file-32.cvc5.good" "cvc5" "pre-clang12"
                  , expA "abd-test-file-32.cvc5.good" "cvc5" "pre-clang11"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expE "abd-test-file-32.pre-clang13.cvc5.good" "cvc5" "pre-clang13"
                      ]
                    Just 11 ->
                      [ expE "abd-test-file-32.pre-clang13.cvc5.good" "cvc5" "pre-clang13"
                      ]
                    Just 12 ->
                      [ expE "abd-test-file-32.pre-clang13.cvc5.good" "cvc5" "pre-clang13"
                      ]
                    Just 14 ->
                      [ expA "abd-test-file-32.cvc5.good" "cvc5" "recent-clang"
                      ]
                    Just 16 ->
                      [ expA "abd-test-file-32.cvc5.good" "cvc5" "recent-clang"
                      ]
                    Nothing ->
                      let e = expA "abd-test-file-32.cvc5.good" "cvc5" in
                        [ e "recent-clang" -- presumed clang=15, clang=14, nothing
                        , e "pre-clang14" -- presumed clang=13
                        , expE "abd-test-file-32.pre-clang13.cvc5.good" "cvc5" "pre-clang13"
                          -- presumed clang=12, clang=11, clang=10
                        ]

            _ -> error $ "Unexpected root sweet: " <> rootMatchName sweet

  in exps


mkTest2 mode matchClang sweet exp =
  let exp0 f s c = Expectation
                 { expectedFile = testInpPath </> f
                 , expParamsMatch = [ ("solver", s)
                                    , ("clang-range", c)
                                    ]
                 , associated = []
                 }
      expA f s l = exp0 f (Explicit s) (Assumed l)
      expE f s l = exp0 f (Explicit s) (Explicit l)

      exps =
          case rootMatchName sweet of
            "shrink.c" ->
              case mode of
                "direct" ->
                  [ expA "shrink.z3.good" "z3" "older-clang"
                  , expA "shrink.z3.good" "z3" "clang14+"
                  , expA "shrink.z3.good" "z3" "clang13+"
                  , expA "shrink.z3.good" "z3" "clang12+"
                  , expA "shrink.z3.good" "z3" "clang11+"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present.  There are no Explicit matches, so
                  -- this should use the best fallback (Assumed) match.
                  case matchClang of
                    Just 9 ->
                      [ expA "shrink.z3.good" "z3" "older-clang"
                      ]
                    Just 11 ->
                      [ expA "shrink.z3.good" "z3" "clang11+"
                      ]
                    Just 12 ->
                      [ expA "shrink.z3.good" "z3" "clang12+"
                      ]
                    Just 13 ->
                      [ expA "shrink.z3.good" "z3" "clang13+"
                      ]
                    Just 14 ->
                      [ expA "shrink.z3.good" "z3" "clang14+"
                      ]
                    Just 16 ->
                      [ expA "shrink.z3.good" "z3" "clang14+"
                      ]
                    Nothing -> []

            "freeze.c" ->
              case mode of
                "direct" ->
                  [ expA "freeze.z3.good" "z3" "older-clang"
                  , expA "freeze.z3.good" "z3" "clang14+"
                  , expA "freeze.z3.good" "z3" "clang13+"
                  , expE "freeze.clang12+.z3.good" "z3" "clang12+"
                  , expA "freeze.z3.good" "z3" "clang11+"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expA "freeze.z3.good" "z3" "older-clang"
                      ]
                    Just 11 ->
                      [ expA "freeze.z3.good" "z3" "clang11+"
                      ]
                    Just 12 ->
                      [ expE "freeze.clang12+.z3.good" "z3" "clang12+"
                      ]
                    Just 13 ->
                      [ expE "freeze.clang12+.z3.good" "z3" "clang12+"
                      ]
                    Just 14 ->
                      [ expE "freeze.clang12+.z3.good" "z3" "clang12+"
                      ]
                    Just 16 ->
                      [ expE "freeze.clang12+.z3.good" "z3" "clang12+"
                      ]
                    Nothing -> []

            "T847-fail2.c" ->
              case mode of
                "direct" ->
                  [ expA "T847-fail2.z3.good" "z3" "older-clang"
                  , expA "T847-fail2.z3.good" "z3" "clang14+"
                  , expA "T847-fail2.z3.good" "z3" "clang13+"
                  , expE "T847-fail2.clang12+.z3.good" "z3" "clang12+"
                  , expA "T847-fail2.z3.good" "z3" "clang11+"

                  , expA "T847-fail2.cvc5.good" "cvc5" "older-clang"
                  , expA "T847-fail2.cvc5.good" "cvc5" "clang14+"
                  , expE "T847-fail2.cvc5.clang13+.good" "cvc5" "clang13+"
                  , expA "T847-fail2.cvc5.good" "cvc5" "clang12+"
                  , expA "T847-fail2.cvc5.good" "cvc5" "clang11+"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expA "T847-fail2.z3.good" "z3" "older-clang"
                      , expA "T847-fail2.cvc5.good" "cvc5" "older-clang"
                      ]
                    Just 11 ->
                      [ expA "T847-fail2.z3.good" "z3" "clang11+"
                      , expA "T847-fail2.cvc5.good" "cvc5" "clang11+"
                      ]
                    Just 12 ->
                      [ expE "T847-fail2.clang12+.z3.good" "z3" "clang12+"
                      , expA "T847-fail2.cvc5.good" "cvc5" "clang12+"
                      ]
                    Just 13 ->
                      [ expE "T847-fail2.clang12+.z3.good" "z3" "clang12+"
                      , expE "T847-fail2.cvc5.clang13+.good" "cvc5" "clang13+"
                      ]
                    Just 14 ->
                      [ expE "T847-fail2.clang12+.z3.good" "z3" "clang12+"
                      , expE "T847-fail2.cvc5.clang13+.good" "cvc5" "clang13+"
                      ]
                    Just 16 ->
                      [ expE "T847-fail2.clang12+.z3.good" "z3" "clang12+"
                      , expE "T847-fail2.cvc5.clang13+.good" "cvc5" "clang13+"
                      ]
                    Nothing -> []

            "T972-fail.c" ->
              case mode of
                "direct" ->
                  [ expA "T972-fail.z3.good" "z3" "older-clang"
                  , expE "T972-fail.clang12+.clang14+.z3.good" "z3" "clang14+"
                  , expA "T972-fail.z3.good" "z3" "clang13+"
                  , expE "T972-fail.clang12+.clang14+.z3.good" "z3" "clang12+"
                  , expA "T972-fail.z3.good" "z3" "clang11+"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expA "T972-fail.z3.good" "z3" "older-clang"
                      ]
                    Just 11 ->
                      [ expA "T972-fail.z3.good" "z3" "clang11+"
                      ]
                    Just 12 ->
                      [ expE "T972-fail.clang12+.clang14+.z3.good" "z3" "clang12+"
                      ]
                    Just 13 ->
                      [ expE "T972-fail.clang12+.clang14+.z3.good" "z3" "clang12+"
                      ]
                    Just 14 ->
                      [ expE "T972-fail.clang12+.clang14+.z3.good" "z3" "clang14+"
                      ]
                    Just 16 ->
                      [ expE "T972-fail.clang12+.clang14+.z3.good" "z3" "clang14+"
                      ]
                    Nothing -> []

            "abd-test-file-32.c" ->
              case mode of
                "direct" ->
                  [ expA "abd-test-file-32.cvc5.good" "cvc5" "older-clang"
                  , expA "abd-test-file-32.cvc5.good" "cvc5" "clang14+"
                  , expE "abd-test-file-32.clang13+.cvc5.good" "cvc5" "clang13+"
                  , expA "abd-test-file-32.cvc5.good" "cvc5" "clang12+"
                  , expA "abd-test-file-32.cvc5.good" "cvc5" "clang11+"
                  ]
                "ranged" ->
                  -- Only run the expectation that fits the matching version of
                  -- clang that is present
                  case matchClang of
                    Just 9 ->
                      [ expA "abd-test-file-32.cvc5.good" "cvc5" "older-clang"
                      ]
                    Just 11 ->
                      [ expA "abd-test-file-32.cvc5.good" "cvc5" "clang11+"
                      ]
                    Just 12 ->
                      [ expA "abd-test-file-32.cvc5.good" "cvc5" "clang12+"
                      ]
                    Just 13 ->
                      [ expE "abd-test-file-32.clang13+.cvc5.good" "cvc5" "clang13+"
                      ]
                    Just 14 ->
                      [ expE "abd-test-file-32.clang13+.cvc5.good" "cvc5" "clang13+"
                      ]
                    Just 16 ->
                      [ expE "abd-test-file-32.clang13+.cvc5.good" "cvc5" "clang13+"
                      ]
                    Nothing -> []

            _ -> error $ "Unexpected root sweet: " <> rootMatchName sweet

  in exps
