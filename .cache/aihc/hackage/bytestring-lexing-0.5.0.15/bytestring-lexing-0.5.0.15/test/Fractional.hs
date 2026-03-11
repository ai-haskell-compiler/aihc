{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
----------------------------------------------------------------
--                                                    2021.10.17
-- |
-- Module      :  test/Fractional
-- Copyright   :  Copyright (c) 2015--2021 wren gayle romano
-- License     :  BSD2
-- Maintainer  :  wren@cpan.org
-- Stability   :  test framework
-- Portability :  ScopedTypeVariables + RankNTypes
--
-- Correctness testing for "Data.ByteString.Lex.Fractional".
----------------------------------------------------------------
module Fractional (main, tests) where

import qualified Test.Tasty                   as Tasty
--import qualified Test.Tasty.SmallCheck        as SC
import qualified Test.Tasty.QuickCheck        as QC
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Char8        as BS8
import           Data.ByteString.Lex.Fractional
--import           Control.Monad                ((<=<))

----------------------------------------------------------------
----------------------------------------------------------------
-- We reimplement Data.Proxy to avoid build errors on older systems

data Proxy a = Proxy

asProxyTypeOf :: a -> Proxy a -> a
asProxyTypeOf a _ = a

----------------------------------------------------------------
-- | Fuzzy equality checking for floating-point numbers.
(=~=) :: (Fractional a, Ord a) => a -> a -> Bool
(=~=) a b = a == b || abs (a - b) <= max (abs a) (abs b) * 1e20


----------------------------------------------------------------
----- QuickCheck\/SmallCheck properties
-- N.B., these properties do not hold of 'Rational', since those
-- are shown as @numerator % denominator@.


-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimal' returns the original
-- number.
prop_readDecimal_show
    :: (Show a, Ord a, Fractional a) => Proxy a -> Integer -> Bool
prop_readDecimal_show proxy x =
    let px = abs x in
    case (readDecimal . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) ->
        BS.null rest && py =~= (fromInteger px `asProxyTypeOf` proxy)


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readDecimal'@ returns the original
-- number.
prop_readSignedDecimal_show
    :: (Show a, Ord a, Fractional a) => Proxy a -> Integer -> Bool
prop_readSignedDecimal_show proxy x =
    case (readSigned readDecimal . BS8.pack . show) x of
    Nothing        -> False
    Just (y, rest) ->
        BS.null rest && y =~= (fromInteger x `asProxyTypeOf` proxy)

----------------------------------------------------------------
-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readExponential' returns the original
-- number.
prop_readExponential_show :: (Show a, Ord a, Fractional a) => a -> Bool
prop_readExponential_show x =
    let px = abs x in
    case (readExponential . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) -> BS.null rest && px =~= py


-- | Converting a number to a string using 'show' and then reading
-- it back using @'readSigned' 'readExponential'@ returns the
-- original number.
prop_readSignedExponential_show
    :: (Show a, Ord a, Fractional a) => a -> Bool
prop_readSignedExponential_show x =
    case (readSigned readExponential . BS8.pack . show) x of
    Nothing        -> False
    Just (y, rest) -> BS.null rest && x =~= y

----------------------------------------------------------------

-- | Use \"infinity\" as the precision-limit for a reader.
atInfinity
    :: (Int -> ByteString -> Maybe (a,ByteString))
    -> ByteString -> Maybe (a,ByteString)
atInfinity f = (\xs -> f (1 + BS.length xs) xs)

-- | Use a 'RealFloat' type's inherent limit as the precision-limit
-- for a reader.
atInherent
    :: forall a. RealFloat a
    => (Int -> ByteString -> Maybe (a,ByteString))
    -> ByteString -> Maybe (a,ByteString)
atInherent f = f (decimalPrecision (Proxy::Proxy a))


-- BUG: at Double, fails on 5.0e-324
--
-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimalLimited' with an \"infinite\"
-- precision limit returns the original number.
prop_readDecimalLimitedInfinity_show
    :: (Show a, Ord a, Fractional a) => Proxy a -> Integer -> Bool
prop_readDecimalLimitedInfinity_show proxy x =
    let px = abs x in
    case (atInfinity readDecimalLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) ->
        BS.null rest && py =~= (fromInteger px `asProxyTypeOf` proxy)

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readExponentialLimited' with an
-- \"infinite\" precision limit returns the original number.
prop_readExponentialLimitedInfinity_show
    :: (Show a, Ord a, Fractional a) => a -> Bool
prop_readExponentialLimitedInfinity_show x =
    let px = abs x in
    case (atInfinity readExponentialLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) -> BS.null rest && px =~= py


-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readDecimalLimited' with the type's
-- inherent precision limit returns the original number.
prop_readDecimalLimitedInherent_show
    :: (Show a, Ord a, RealFloat a) => Proxy a -> Integer -> Bool
prop_readDecimalLimitedInherent_show proxy x =
    let px = abs x in
    case (atInherent readDecimalLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) ->
        BS.null rest && py =~= (fromInteger px `asProxyTypeOf` proxy)

-- | Converting a non-negative number to a string using 'show' and
-- then reading it back using 'readExponentialLimited' with the
-- type's inherent precision limit returns the original number.
prop_readExponentialLimitedInherent_show
    :: (Show a, Ord a, RealFloat a) => a -> Bool
prop_readExponentialLimitedInherent_show x =
    let px = abs x in
    case (atInherent readExponentialLimited . BS8.pack . show) px of
    Nothing         -> False
    Just (py, rest) -> BS.null rest && px =~= py

----------------------------------------------------------------
----------------------------------------------------------------
floatProxy :: Proxy Float
floatProxy = Proxy

doubleProxy :: Proxy Double
doubleProxy = Proxy

atFloat :: (Float -> a) -> Float -> a
atFloat = id

atDouble :: (Double -> a) -> Double -> a
atDouble = id

qc_testGroup_Proxy
    :: QC.Testable b
    => String
    -> (forall a. (RealFloat a, Ord a, Show a) => Proxy a -> b)
    -> Tasty.TestTree
qc_testGroup_Proxy n f =
    Tasty.testGroup n
        [ QC.testProperty "Float"  $ f floatProxy
        , QC.testProperty "Double" $ f doubleProxy
        ]

qc_testGroup_At
    :: QC.Testable b
    => String
    -> (forall a. (RealFloat a, Ord a, Show a) => a -> b)
    -> Tasty.TestTree
qc_testGroup_At n f =
    Tasty.testGroup n
        [ QC.testProperty "Float"  $ atFloat  f
        , QC.testProperty "Double" $ atDouble f
        ]

----------------------------------------------------------------
main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Fractional Tests"
    [Tasty.testGroup "Properties"
        [ quickcheckTests
        , smallcheckTests
        ]
    -- TODO: add some HUnit tests
    ]


quickcheckTests :: Tasty.TestTree
quickcheckTests = Tasty.testGroup "(checked by QuickCheck)"
    [ qc_testGroup_Proxy
        "prop_readDecimal_show"
         prop_readDecimal_show
    , qc_testGroup_Proxy
        "prop_readSignedDecimal_show"
         prop_readSignedDecimal_show
    , qc_testGroup_At
        "prop_readExponential_show"
         prop_readExponential_show
    , qc_testGroup_At
        "prop_readSignedExponential_show"
         prop_readSignedExponential_show
    , qc_testGroup_Proxy
        "prop_readDecimalLimitedInfinity_show"
         prop_readDecimalLimitedInfinity_show
    , qc_testGroup_At
        "prop_readExponentialLimitedInfinity_show"
         prop_readExponentialLimitedInfinity_show
    , qc_testGroup_Proxy
        "prop_readDecimalLimitedInherent_show"
         prop_readDecimalLimitedInherent_show
    , qc_testGroup_At
        "prop_readExponentialLimitedInherent_show"
         prop_readExponentialLimitedInherent_show
    ]


-- TODO: how to properly utilize SmallCheck for this module?
-- TODO: how can we set a default 'SmallCheckDepth' while still allowing @--smallcheck-depth@ to override that default?
smallcheckTests :: Tasty.TestTree
smallcheckTests =
    -- Tasty.localOption (SC.SmallCheckDepth (2 ^ (8 :: Int))) $
    Tasty.testGroup "(checked by SmallCheck)"
        [
        ]

----------------------------------------------------------------
----------------------------------------------------------- fin.
