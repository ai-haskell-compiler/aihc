{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main (main) where

import           Data.Rg
import           Fmt
import           Test.Tasty
import           Test.Tasty.HUnit


main :: IO ()
main = defaultMain $
    testGroup "rg" $
    [ testGroup "Range"
      [ testCase "sizeRg"         $ 10        @=? sizeRg tst
      , testCase "allListRg"      $ [10,9..1] @=? map extractRange (allListRg tst)
      , testCase "predRg"         $ Nothing   @=? (extractRange <$> predRg tst)
      , testCase "predRg . maxRg" $ Just 2    @=? (extractRange <$> predRg (maxRg tst))
      , testCase "succRg"         $ Just 9    @=? (extractRange <$> succRg tst)
      , testCase "extractRange"   $ 10        @=? extractRange tst
      , testCase "minRg"          $ 10        @=? extractRange (minRg tst)
      , testCase "maxRg"          $ 1         @=? extractRange (maxRg tst)
      , testCase "extractRange"   $ 1         @=? extractRange (maxRg tst)
      , testCase "fromRg"         $ 0         @=? fromRg tst
      , testCase "minRg"          $ 0         @=? fromRg (minRg tst)
      , testCase "maxRg"          $ 9         @=? fromRg (maxRg tst)
      , testCase "toRg"           $ Just 5    @=? (fromRg <$> toRg tst 5)
      ]
    , testGroup "BE"
      [ testCase "sizeRg True"    $ 2                  @=? sizeRg    (BE True )
      , testCase "sizeRg False"   $ 2                  @=? sizeRg    (BE False)
      , testCase "toRg 0"         $ Just (BE False)    @=? toRg      (BE False) 0
      , testCase "toRg 1"         $ Just (BE True )    @=? toRg      (BE False) 1
      , testCase "toRg 3"         $ Nothing            @=? toRg      (BE False) 2
      , testCase "allListRg"      $ [BE False,BE True] @=? allListRg (BE False)
      , testCase "fromRg False"   $ 0                  @=? fromRg    (BE False)
      , testCase "fromRg True"    $ 1                  @=? fromRg    (BE True )
      ]
    , testGroup "rgCoreMethodsBE"
      [ testCase "sizeRg True"    $ 2                  @=? sizeRg    True
      , testCase "sizeRg False"   $ 2                  @=? sizeRg    False
      , testCase "toRg 0"         $ Just False         @=? toRg      False 0
      , testCase "toRg 1"         $ Just True          @=? toRg      False 1
      , testCase "toRg 3"         $ Nothing            @=? toRg      False 2
      , testCase "allListRg"      $ [False,True]       @=? allListRg False
      , testCase "fromRg False"   $ 0                  @=? fromRg    False
      , testCase "fromRg True"    $ 1                  @=? fromRg    True
      ]
    , testGroup "RgText"
      [ testCase "TRUE"    $ Right (BE True ) @=? parseRgText (BE False) "TRUE"
      , testCase "FALSE"   $ Right (BE False) @=? parseRgText (BE True ) "FALSE"
      , testCase "true"    $ Left msg         @=? parseRgText (BE False) "false"
      ]
    ]
  where
    tst :: Range Int
    tst = newStartOfRangeFromList [10,9..1]

    msg :: String
    msg = "parseRgText: enumeration not recognised: \"false\""

instance RgText (BE Bool)

instance Rg Bool where rgCoreMethods = rgCoreMethodsBE

instance Buildable (BE Bool) where
  build (BE True)  = "TRUE"
  build (BE False) = "FALSE"
