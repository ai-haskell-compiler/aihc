{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE RankNTypes, FlexibleContexts #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  test/Integral
-- Copyright   :  Copyright (c) 2010--2021 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@cpan.org
-- Stability   :  test framework
-- Portability :  FlexibleContexts + RankNTypes
--
-- Correctness testing for "Data.ByteString.Lex.Integral".
----------------------------------------------------------------
module Integral (main, tests) where

import qualified Test.Tasty                   as Tasty
import qualified Test.Tasty.SmallCheck        as SC
import qualified Test.Tasty.QuickCheck        as QC
import           Data.Int                     (Int32, Int64)
import           Control.Monad                ((<=<))
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import           Data.ByteString.Lex.Integral

----------------------------------------------------------------
----------------------------------------------------------------
----- QuickCheck\/SmallCheck properties

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal' returns the original
-- number.
prop_readDecimal_show :: (Show a, Integral a) => a -> Bool
prop_readDecimal_show x =
    let px = abs x
    in  Just (px, BS.empty) == (readDecimal . BS8.pack . show) px


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readDecimal'@ returns the original
-- number.
prop_readSignedDecimal_show :: (Show a, Integral a) => a -> Bool
prop_readSignedDecimal_show x =
    Just (x, BS.empty) == (readSigned readDecimal . BS8.pack . show) x


-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal_' returns the original
-- number.
prop_readDecimalzu_show :: (Show a, Integral a) => a -> Bool
prop_readDecimalzu_show x =
    let px = abs x
    in  px == (readDecimal_ . BS8.pack . show) px


-- | Converting a non-negative number to a bytestring using
-- 'packDecimal' and then reading it back using 'read' returns the
-- original number.
prop_read_packDecimal :: (Read a, Integral a) => a -> Bool
prop_read_packDecimal x =
    let px = abs x
    in  px == (read . maybe "" BS8.unpack . packDecimal) px


-- | Converting a non-negative number to a string using 'packDecimal'
-- and then reading it back using 'readDecimal' returns the original
-- number.
prop_readDecimal_packDecimal :: (Show a, Integral a) => a -> Bool
prop_readDecimal_packDecimal x =
    let px = abs x
    in  Just (px, BS.empty) == (readDecimal <=< packDecimal) px
-- TODO: how can we check the other composition with QC/SC?

----------------------------------------------------------------
-- | Converting a non-negative number to a string using 'packHexadecimal'
-- and then reading it back using 'readHexadecimal' returns the
-- original number.
prop_readHexadecimal_packHexadecimal :: (Show a, Integral a) => a -> Bool
prop_readHexadecimal_packHexadecimal x =
    let px = abs x
    in  Just (px, BS.empty) == (readHexadecimal <=< packHexadecimal) px

-- TODO: how can we check the other composition with QC/SC?

----------------------------------------------------------------
-- | Converting a non-negative number to a string using 'packOctal'
-- and then reading it back using 'readOctal' returns the original
-- number.
prop_readOctal_packOctal :: (Show a, Integral a) => a -> Bool
prop_readOctal_packOctal x =
    let px = abs x
    in  Just (px, BS.empty) == (readOctal <=< packOctal) px

-- TODO: how can we check the other composition with QC/SC?

----------------------------------------------------------------
{-
-- | A more obviously correct but much slower implementation than
-- the public one.
packDecimal :: (Integral a) => a -> Maybe ByteString
packDecimal = start
    where
    start n0
        | n0 < 0    = Nothing
        | otherwise = Just $ loop n0 BS.empty

    loop !n !xs
        | n <= 9    = BS.cons (0x30 + fromIntegral n) xs
        | otherwise =
            let (q,r) = n `quotRem` 10
            in loop q (BS.cons (0x30 + fromIntegral r) xs)
-}

----------------------------------------------------------------
----------------------------------------------------------------

atInt :: (Int -> a) -> Int -> a
atInt = id

atInt32 :: (Int32 -> a) -> Int32 -> a
atInt32 = id

atInt64 :: (Int64 -> a) -> Int64 -> a
atInt64 = id

atInteger :: (Integer -> a) -> Integer -> a
atInteger = id

-- | Test 'Integers' around the 'Int' boundary. This combinator is
-- for smallcheck.
intBoundary :: (Integer -> a) -> Integer -> a
intBoundary f x = f (x + fromIntegral (maxBound - 8 :: Int))


qc_testGroup
    :: QC.Testable b
    => String
    -> (forall a. (Integral a, Read a, Show a) => a -> b)
    -> Tasty.TestTree
qc_testGroup n f =
    Tasty.testGroup n
        [ QC.testProperty "Int"     $ atInt     f
        , QC.testProperty "Int32"   $ atInt32   f
        , QC.testProperty "Int64"   $ atInt64   f
        , QC.testProperty "Integer" $ atInteger f
        ]

sc_testGroup
    :: SC.Testable IO b
    => String
    -> (forall a. (Integral a, Read a, Show a) => a -> b)
    -> Tasty.TestTree
sc_testGroup n f =
    Tasty.testGroup n
        [ SC.testProperty "Int"         $ atInt       f
        , SC.testProperty "IntBoundary" $ intBoundary f
        ]

----------------------------------------------------------------
main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Integral Tests"
    [Tasty.testGroup "Properties"
        [ quickcheckTests
        , smallcheckTests
        ]
    -- TODO: add some HUnit tests
    ]

quickcheckTests :: Tasty.TestTree
quickcheckTests = Tasty.testGroup "(checked by QuickCheck)"
    [ qc_testGroup
        "prop_readDecimal_show"
         prop_readDecimal_show
    , qc_testGroup
        "prop_readDecimalzu_show"
         prop_readDecimalzu_show
    , qc_testGroup
        "prop_readSignedDecimal_show"
         prop_readSignedDecimal_show
    , qc_testGroup
        "prop_read_packDecimal"
         prop_read_packDecimal
    , qc_testGroup
        "prop_readDecimal_packDecimal"
         prop_readDecimal_packDecimal
    , qc_testGroup
        "prop_readHexadecimal_packHexadecimal"
         prop_readHexadecimal_packHexadecimal
    , qc_testGroup
        "prop_readOctal_packOctal"
         prop_readOctal_packOctal
    ]


-- TODO: how can we set our default 'SmallCheckDepth' to 2^8 while still allowing @--smallcheck-depth@ to override that default?
smallcheckTests :: Tasty.TestTree
smallcheckTests =
    Tasty.localOption (SC.SmallCheckDepth (2 ^ (8 :: Int))) $
    Tasty.testGroup "(checked by SmallCheck)"
    [ sc_testGroup
        "prop_readDecimal_show"
         prop_readDecimal_show
    , sc_testGroup
        "prop_readDecimalzu_show"
         prop_readDecimalzu_show
    , sc_testGroup
        "prop_readSignedDecimal_show"
         prop_readSignedDecimal_show
    , sc_testGroup
        "prop_read_packDecimal"
         prop_read_packDecimal
    , sc_testGroup
        "prop_readDecimal_packDecimal"
         prop_readDecimal_packDecimal
    , sc_testGroup
        "prop_readHexadecimal_packHexadecimal"
         prop_readHexadecimal_packHexadecimal
    , sc_testGroup
        "prop_readOctal_packOctal"
         prop_readOctal_packOctal
    ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
