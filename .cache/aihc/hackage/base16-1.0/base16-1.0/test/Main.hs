{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2020-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the test implementation for the `base16` package
--
module Main
( main
, tests
) where


import Data.Base16.Types
import Data.Bifunctor (second)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import "base16" Data.ByteString.Base16 as B16
import qualified "base16-bytestring" Data.ByteString.Base16 as Bos
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base16.Error (Base16Error(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Short as TS
import Data.Word

import Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base16 Tests"
  [ mkTree b16
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree lb16
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree sb16
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree t16
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree T.decodeUtf8' b16
    ]
  , mkTree tl16
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree TL.decodeUtf8' lb16
    ]
  , mkTree ts16
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree
      (second TS.fromText . T.decodeUtf8' . SBS.fromShort) sb16
    ]
  ]

-- ---------------------------------------------------------------- --
-- Test tree generation

-- | Make a test tree for a given label
--
mkTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> [proxy a -> TestTree]
  -> TestTree
mkTree a = testGroup (label @a) . fmap ($ a)

-- | Make a test group with some name, lifting a test tree up to the correct
-- type information via some Harness
--
mkTests
  :: forall a b proxy
  . Harness a b
  => String
  -> [proxy a -> TestTree]
  -> proxy a
  -> TestTree
mkTests context ts = testGroup context . (<*>) ts . pure

-- | Make property tests for a given harness instance
--
mkPropTree :: forall a b proxy. Harness a b => proxy a -> TestTree
mkPropTree = mkTests "Property Tests"
  [ prop_roundtrip
  , prop_untyped_correctness
  , const prop_bos_coherence
  ]

-- | Make unit tests for a given harness instance
--
mkUnitTree
  :: forall a b proxy
  . Harness a b
  => proxy a
  -> TestTree
mkUnitTree = mkTests "Unit tests"
  [ rfcVectors
  , lenientTests
  ]

-- | Make unit tests for textual 'decode*With' functions
--
mkDecodeTree
  :: forall t a b c e proxy
  . ( TextHarness a b c
    , Harness t c
    , Show e
    )
  => (c -> Either e b)
  -> proxy t
  -> proxy a
  -> TestTree
mkDecodeTree utf8 t = mkTests "Decoding tests"
  [ decodeWithVectors utf8 t
  ]

-- ---------------------------------------------------------------- --
-- Property tests

prop_roundtrip :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_roundtrip _ = testGroup "prop_roundtrip"
  [ testProperty "prop_std_roundtrip_typed" $ \(bs :: b) ->
      bs == decodeTyped (encode bs)
  , testProperty "prop_std_roundtrip_untyped" $ \(bs :: b) ->
      Right bs
        == decode (extractBase16 $ encode bs)
  , testProperty "prop_std_lenient_roundtrip" $ \(bs :: b) ->
      bs == lenient (extractBase16 $ encode bs)
  ]

prop_untyped_correctness :: forall a b proxy. Harness a b => proxy a -> TestTree
prop_untyped_correctness _ = testGroup "prop_validity"
  [ testProperty "prop_std_valid" $ \(bs :: b) ->
    validate (extractBase16 $ encode bs)
  , testProperty "prop_std_correct" $ \(bs :: b) ->
    correct (extractBase16 $ encode bs)
  ]

-- | just a sanity check against `base16-bytestring`
--
prop_bos_coherence :: TestTree
prop_bos_coherence = testGroup "prop_bos_coherence"
  [ testProperty "prop_std_bos_coherence" $ \bs ->
      Right bs == B16.decodeBase16Untyped (extractBase16 $ B16.encodeBase16' bs)
      && Right bs == Bos.decode (Bos.encode bs)
      && bs == B16.decodeBase16 (B16.encodeBase16' bs)
  ]

-- ---------------------------------------------------------------- --
-- Unit tests

rfcVectors :: forall a b proxy. Harness a b => proxy a -> TestTree
rfcVectors _ = testGroup "RFC 4648 Test Vectors"
    [ testGroup "lower-case"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666f"
      , testCaseB16 "foo" "666f6f"
      , testCaseB16 "foob" "666f6f62"
      , testCaseB16 "fooba" "666f6f6261"
      , testCaseB16 "foobar" "666f6f626172"
      ]
    ,  testGroup "upper-case"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666F"
      , testCaseB16 "foo" "666F6F"
      , testCaseB16 "foob" "666F6F62"
      , testCaseB16 "fooba" "666F6F6261"
      , testCaseB16 "foobar" "666F6F626172"
      ]
    ,  testGroup "mixed-case"
      [ testCaseB16 "" ""
      , testCaseB16 "f" "66"
      , testCaseB16 "fo" "666F"
      , testCaseB16 "foo" "666F6f"
      , testCaseB16 "foob" "666F6f62"
      , testCaseB16 "fooba" "666F6f6261"
      , testCaseB16 "foobar" "666F6f626172"
      ]
    ]
  where
    testCaseB16 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do

        step "encode is sound"
        lower t @=? extractBase16 (encode @a s)

        step "decode is sound"
        s @=? decodeTyped (assertBase16 t)

        step "decodeUntyped is sound"
        Right s @=? decode (extractBase16 $ encode s)

-- | Unit test trees for the `decode*With` family of text-valued functions
--
decodeWithVectors
  :: forall t a b c e proxy
  . ( TextHarness a c b
    , Harness t b
    , Show e
    )
  => (b -> Either e c)
    -- ^ utf8
  -> proxy t
    -- ^ witness to the bytestring-ey dictionaries
  -> proxy a
    -- ^ witness to the text dictionaries
  -> TestTree
decodeWithVectors utf8 _ _ = testGroup "DecodeWith* unit tests"
  [ testGroup "decodeWith negative tests"
    [ testCase "decodeWith non-utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeWith valid utf8 inputs on decodeUtf8" $ do
      case decodeWith_ @a utf8 (extractBase16 $ encode @t "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    ]
  , testGroup "decodeWith positive tests"
    [ testCase "decodeWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decode @a "666f6f626172"
      b <- either (assertFailure . show) pure $ decodeWith_ @a utf8 "666f6f626172"
      a @=? b
    ]
  ]

lenientTests :: forall a b proxy. Harness a b => proxy a -> TestTree
lenientTests _ = testGroup "Lenient Tests"
    [ testCaseB16 "" ""
    , testCaseB16 "f" "6+6"
    , testCaseB16 "fo" "6$6+6|f"
    , testCaseB16 "foo" "==========6$$66()*F6f"
    , testCaseB16 "foob" "66^%$&^6f6F62"
    , testCaseB16 "fooba" "666f()*#@6F#)(@*)6()*)2()61"
    , testCaseB16 "foobar" "6@6@6@f@6@f@6@2@6@1@7@2++++++++++++++++++++++++"
    ]
  where
    testCaseB16 s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        let t0 = decode (extractBase16 $ encode @a s)
            t1 = lenient @a t

        step "compare decoding"
        t0 @=? Right t1
