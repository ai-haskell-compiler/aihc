{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| This test verifies behavior when the mkCUBE rootName is a generic
  wildcard ("*") which will match everything in the target directory.
  Proper tasty-sugar behavior will check for the expected and
  associated files and identify those as associated test files *not*
  include those as input rootFiles.
-}

module TestWildcard ( wildcardAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils
import           Text.RawString.QQ


testInpPath = "some/path/to/test/samples"


sample1 cube = fmap (makeCandidate cube testInpPath [])
          $ filter (not . null)
          $ lines [r|
foo
foo.exp
foo.ex
foo.right.exp
bar.exp
bar.
bar-ex
cow.moo
cow.mooexp
cow.mooex
cow.mooexe
readme.txt
dog.bark
dog.bark-exp
|]


wildcardAssocTests :: [TT.TestTree]
wildcardAssocTests =
  [ let sugarCube = mkCUBE
                    { rootName = "*"
                    , expectedSuffix = "exp"
                    , inputDirs = [ testInpPath ]
                    , associatedNames = [ ("extern", "ex") ]
                    }
        p = (testInpPath </>)
    in testCase "valid sample" $ 14 @=? length (sample1 sugarCube)

  -- The first CUBE uses the default set of separators, and the expected
  -- suffix does not limit which separators can preceed the suffix.
  , TT.testGroup "with default seps" $
    let sugarCube = mkCUBE
                    { rootName = "*"
                    , expectedSuffix = "exp"
                    , inputDirs = [ testInpPath ]
                    , associatedNames = [ ("extern", "ex") ]
                    }
        p = (testInpPath </>)
        exp e = Expectation { expectedFile = p e
                            , associated = []
                            , expParamsMatch = []
                            }
        expE e a = (exp e) { associated = [("extern", p a)] }
        sw m b f e = Sweets { rootBaseName = b
                            , rootMatchName = m
                            , rootFile = p f
                            , cubeParams = []
                            , expected = e
                            }
    in [ sugarTestEq "correct found count" sugarCube sample1 9 length

         -- foo.ex is an associated name for foo, but removing its
         -- extension makes it a sibling for the expected file and
         -- therefore a valid root as well.
       , sugarTestEq "foo is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootMatchName x == "foo" ]
       , sugarTestEq "foo.ex is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootMatchName x == "foo.ex" ]

       -- Similarly, bar.ex is an associated name, but also
       -- matches the expected Suffix when its .ex suffix is
       -- removed.
       , sugarTestEq "bar. is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootMatchName x == "bar." ]
       , sugarTestEq "bar-ex is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootMatchName x == "bar-ex" ]

       , sugarTestEq "dog.bark is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootFile x == p "dog.bark" ]
       -- n.b. cow.moo is not matched because there is no separator in cow.mooexp
       , testCase "full results" $ do
           (sweets, _desc) <- findSugarIn sugarCube (sample1 sugarCube)
           compareBags "default result" sweets
             [ sw "foo"      "foo"      "foo"      [ expE "foo.exp" "foo.ex" ]
             , sw "foo.ex"   "foo"      "foo.ex"   [ exp "foo.exp" ]
             , sw "bar."     "bar"      "bar."     [ expE "bar.exp" "bar-ex" ]
             , sw "bar-ex"   "bar"      "bar-ex"   [ exp "bar.exp" ]
             , sw "dog.bark" "dog.bark" "dog.bark" [ exp "dog.bark-exp" ]
               -- rootName is a wildcard, so the expected can match the root:
             , sw "bar.exp"  "bar"      "bar.exp"  [ expE "bar.exp" "bar-ex" ]
             , let r = "dog.bark-exp" in sw r "dog.bark" r [ exp r ]
             , sw "foo.exp"  "foo"      "foo.exp"  [ expE "foo.exp" "foo.ex" ]
             , let r = "foo.right.exp" in sw r "foo.right" r [ exp r ]
             ]

       , testCase "full distinct results" $ do
           (sweets, _desc) <- findSugarIn (sugarCube
                                           { sweetAdjuster = const (return . distinctResults) })
                              (sample1 sugarCube)
           compareBags "default result" sweets
             [ sw "foo"      "foo"      "foo"      [ expE "foo.exp" "foo.ex" ]
             , sw "foo.ex"   "foo"      "foo.ex"   [ exp "foo.exp" ]
             , sw "bar."     "bar"      "bar."     [ expE "bar.exp" "bar-ex" ]
             , sw "bar-ex"   "bar"      "bar-ex"   [ exp "bar.exp" ]
             , sw "dog.bark" "dog.bark" "dog.bark" [ exp "dog.bark-exp" ]
             ]
       ]

  -- The second CUBE specifies no separators: the expected suffix
  -- immediately follows the source name with no separator.
  , TT.testGroup "no seps" $
    let sugarCube = mkCUBE
                    { rootName = "*"
                    , expectedSuffix = "exp"
                    , separators = ""
                    , inputDirs = [ testInpPath ]
                    , associatedNames = [ ("extern", "ex") ]
                    }
        p = (testInpPath </>)
        exp e = Expectation { expectedFile = p e
                            , associated = []
                            , expParamsMatch = []
                            }
        expE e a = (exp e) { associated = [("extern", p a)] }
        sw m b f e = Sweets { rootBaseName = b
                            , rootMatchName = m
                            , rootFile = p f
                            , cubeParams = []
                            , expected = e
                            }
    in [ sugarTestEq "correct found count" sugarCube sample1 2 length
       , sugarTestEq "bar is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootFile x == p "bar." ]
       , sugarTestEq "cow.moo is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootFile x == p "cow.moo" ]
         -- n.b. neither foo nor dog.bark is matched because they
         -- both have a character between the name and the
         -- expectedSuffix that is not a known separator
       , testCase "full results" $ do
           (sweets, _desc) <- findSugarIn sugarCube (sample1 sugarCube)
           compareBags "default result" sweets
             [ sw "bar."    "bar."    "bar."    [ exp "bar.exp" ]
             , sw "cow.moo" "cow.moo" "cow.moo" [ expE "cow.mooexp" "cow.mooex" ]
             ]
       ]


  -- The third CUBE specifies one of the separators as part of the
  -- suffix, so only that separator is matched.
  , TT.testGroup "seps with suffix sep" $
    let sugarCube = mkCUBE
                    { rootName = "*"
                    , expectedSuffix = ".exp"
                    , inputDirs = [ testInpPath ]
                    , associatedNames = [ ("extern", "ex") ]
                    }
        p = (testInpPath </>)
        exp e = Expectation { expectedFile = p e
                            , associated = []
                            , expParamsMatch = []
                            }
        expE e a = (exp e) { associated = [("extern", p a)] }
        sw m b f e = Sweets { rootBaseName = b
                            , rootMatchName = m
                            , rootFile = p f
                            , cubeParams = []
                            , expected = e
                            }
    in  [ sugarTestEq "correct found count" sugarCube sample1 7 length

          -- see notes for default seps tests above

        , sugarTestEq "foo is a case" sugarCube sample1 1 $
          \sugar -> length [ x | x <- sugar, rootMatchName x == "foo" ]
        , sugarTestEq "foo.ex is a case" sugarCube sample1 1 $
          \sugar -> length [ x | x <- sugar, rootMatchName x == "foo.ex" ]

          -- n.b. dog.bark is not matched because the separator in
          -- dog.bark-exp is not the separator specified for the
          -- expected suffix.

        , sugarTestEq "bar. is a case" sugarCube sample1 1 $
          \sugar -> length [ x | x <- sugar, rootMatchName x == "bar." ]
        , sugarTestEq "bar-ex is a case" sugarCube sample1 1 $
          \sugar -> length [ x | x <- sugar, rootMatchName x == "bar-ex" ]

          -- n.b. cow.moo is not matched because there is no separator in cow.mooexp
        , testCase "full results" $ do
            (sweets, _desc) <- findSugarIn sugarCube (sample1 sugarCube)
            compareBags "default result" sweets
              [ sw "foo"    "foo" "foo"    [ expE "foo.exp" "foo.ex" ]
              , sw "foo.ex" "foo" "foo.ex" [ exp "foo.exp" ]
              , sw "bar."   "bar" "bar."   [ expE "bar.exp" "bar-ex" ]
              , sw "bar-ex" "bar" "bar-ex" [ exp "bar.exp" ]

                -- rootName is a wildcard, so the expected can match the root:
              , sw "bar.exp" "bar" "bar.exp" [ expE "bar.exp" "bar-ex" ]
              , sw "foo.exp" "foo" "foo.exp" [ expE "foo.exp" "foo.ex" ]
              , let r = "foo.right.exp" in sw r "foo.right" r [ exp r ]
              ]
        ]

  -- The fourth CUBE specifies one of the separators as part of the suffix, so
  -- only that separator is matched, and further specifies a non-wildcard
  -- suffix for the match (which occludes the associated name).
  , TT.testGroup "seps with suffix sep and root suffix" $
    let sugarCube = mkCUBE
                    { rootName = "*.ex"
                    , expectedSuffix = ".exp"
                    , inputDirs = [ testInpPath ]
                    , associatedNames = [ ("extern", "ex") ]
                    }
        p = (testInpPath </>)
        exp e = Expectation { expectedFile = p e
                            , associated = []
                            , expParamsMatch = []
                            }
        sw m b f e = Sweets { rootBaseName = b
                            , rootMatchName = m
                            , rootFile = p f
                            , cubeParams = []
                            , expected = e
                            }
    in [ sugarTestEq "correct found count" sugarCube sample1 1 length
       , sugarTestEq "foo.ex is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootMatchName x == "foo.ex" ]
       , testCase "full results" $ do
           (sweets, _desc) <- findSugarIn sugarCube (sample1 sugarCube)
           compareBags "default result" sweets
             [ sw "foo.ex" "foo" "foo.ex" [ exp "foo.exp" ]
             ]
       ]

  -- The fifth CUBE is like the fourth CUBE but it additionally matches
  -- parameters.  The fifth CUBE (like the fourth) specifies one of the
  -- separators as part of the suffix, so only that separator is matched, and
  -- further specifies a non-wildcard suffix for the match (which occludes the
  -- associated name).
  , TT.testGroup "seps with suffix sep and root suffix and params" $
    let sugarCube = mkCUBE
                    { rootName = "*.ex"
                    , expectedSuffix = ".exp"
                    , inputDirs = [ testInpPath ]
                    , validParams = [ ("dir", Just [ "right", "left" ]) ]
                    , associatedNames = [ ("extern", "ex") ]
                    }
        p = (testInpPath </>)
        exp e d = Expectation { expectedFile = p e
                              , associated = []
                              , expParamsMatch = [ ("dir", d) ]
                              }
        sw m b f e = Sweets { rootBaseName = b
                            , rootMatchName = m
                            , rootFile = p f
                            , cubeParams = [("dir", Just ["right", "left"])]
                            , expected = e
                            }
    in [ sugarTestEq "correct found count" sugarCube sample1 1 length
       , sugarTestEq "foo.ex is a case" sugarCube sample1 1 $
         \sugar -> length [ x | x <- sugar, rootMatchName x == "foo.ex" ]
       , testCase "full results" $ do
           (sweets, _desc) <- findSugarIn sugarCube (sample1 sugarCube)
           compareBags "default result" sweets
             [ sw "foo.ex" "foo" "foo.ex"
               [ exp "foo.exp"       (Assumed "left")
               , exp "foo.right.exp" (Explicit "right")
               ]
             ]
       ]

  ]
