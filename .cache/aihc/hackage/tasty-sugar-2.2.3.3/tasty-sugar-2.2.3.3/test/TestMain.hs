{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception ( SomeException, try )
import           Control.Monad
import           Data.Bifunctor ( bimap )
import qualified Hedgehog as HH
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import           Test.Tasty.Sugar

#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter
#else
import           Data.Text.Prettyprint.Doc
#endif

import           TestFileSys
import           TestGCD
import           TestIssue3
import           TestLLVMRange
import           TestMultiAssoc
import           TestNoAssoc
import           TestParams
import           TestParamsAssoc
import           TestSingleAssoc
import           TestStrlen2
import           TestUtils
import           TestWildcard


main :: IO ()
main =
  let namedGenGroup groupName tests =
        return $ testGroup (groupName <> " generated") tests
  in
  do generatedTests <- namedGenGroup "no association" <$> mkNoAssocTests
     fsTests <- fileSysTests
     issueTests <- issue3Tests
     llvmTests <- llvmRangeTests
     defaultMain $
       testGroup "tasty-sweet tests" $
       [
#if MIN_VERSION_tasty_hedgehog(1,2,0)
         testPropertyNamed "empty file list" "emptyfile" $
#else
         testProperty "empty file list" $
#endif
         HH.withTests 10000 $ HH.property $ do
           cube <- HH.forAll $ genCube
           (sweets, _desc) <- findSugarIn cube []
           HH.assert $ null sweets

       , testGroup "invalid separators" $
         [
           testCase "duplicate separators" $
           (Left "Duplicate separator characters" @=?) =<<
           runTestOrErr (mkCUBE { inputDirs = [ "." ]
                                , rootName = "*.foo"
                                , expectedSuffix = "e"
                                , separators = ".-."
                                , associatedNames = []
                                , validParams = []
                                })

         , testCase "many duplicate separators" $
           (Left "Duplicate separator characters" @=?) =<<
           runTestOrErr (mkCUBE { inputDirs = [ "." ]
                                , rootName = "*.foo"
                                , expectedSuffix = "e"
                                , separators = ".---....---"
                                , associatedNames = []
                                , validParams = []
                                })

         ]

       , testGroup "invalid parameters" $
         let c1 = mkCUBE { inputDirs = [ "." ]
                         , rootName = "*.foo"
                         , expectedSuffix = "e"
                         , separators = "."
                         , associatedNames = []
                         , validParams = [("a", Nothing)
                                         ,("b", Nothing)
                                         ]
                         }
             msg1 = "Only one parameter can have unconstrained values (i.e. Nothing)"
             c2 = c1 { validParams = [("a", Just [])] }
             msg2 = "Blank validParams values are not allowed (a)"
             c3 = c1 { validParams = [("a", Just ["hi", ""])
                                     ,("b", Just ["one", "two"])
                                     ,("c", Just [""])]
                     }
             msg3 = "Parameter values cannot be blank (a, c)"

             c4 = c1 { validParams = [("a", Just ["hi", "two"])
                                     ,("b", Just ["one", "two"])
                                     ,("c", Just ["end"])]
                     }
             msg4 = "Parameter values cannot be duplicated " <>
                    show [(("a","b"), "two")]

             c5 = c1 { validParams = [("a", Just ["two", "two"])
                                     ,("b", Just ["one", "hi"])
                                     ,("c", Just ["one"])]
                     }
             msg5 = "Parameter values cannot be duplicated " <>
                    show [ (("a","a"), "two")
                         , (("b", "c"), "one")
                         ]

             c6 = c1 { separators = ".-o"
                     , validParams = [("a", Just ["two", "t"])
                                     ,("b", Just [".1", "one", "hi.u"])
                                     ,("c", Just ["o"])]
                     }
             msg6 = "Parameter values cannot contain separators " <>
                    show ["a", "b", "b", "b", "c"]
         in [

           testCase "too many ambiguous parameters" $
             (Left msg1 @=?) =<< runTestOrErr c1

           , testCase "empty parameter value list" $
             (Left msg2 @=?) =<< runTestOrErr c2

           , testCase "blank parameter values" $
             (Left msg3 @=?) =<< runTestOrErr c3

           , testCase "inter-duplicated parameter values" $
             (Left msg4 @=?) =<< runTestOrErr c4

           , testCase "intra-duplicated parameter values" $
             (Left msg5 @=?) =<< runTestOrErr c5

           , testCase "parameter values containing separators" $
             (Left msg6 @=?) =<< runTestOrErr c6

           ]

       , testGroup "no associated file" $ noAssocTests
       , testGroup "single associated file" $ singleAssocTests
       , testGroup "multiple associated files" $ multiAssocTests
       , testGroup "params association" $ paramsAssocTests
       , testGroup "params" $ paramTests
       , testGroup "wildcard tests" $ wildcardAssocTests
       , testGroup "gcd sample tests" $ gcdSampleTests
       , testGroup "strlen2 sample tests" $ strlen2SampleTests
       ]
       <> generatedTests
       <> fsTests
       <> issueTests
       <> llvmTests


runTestOrErr :: CUBE -> IO (Either String String)
runTestOrErr c = bimap (head . lines . show) show <$>
                 (try (do !r <- findSugarIn c []
                          -- the following is to force the evaluation of
                          -- findSugarIn within the try context.  If this isn't
                          -- done, the tests will fail with "error" calls.
                          if null (fst r)
                            then error "never reached"
                            else return r
                      )
                   :: IO (Either SomeException ([Sweets], Doc ann)))
