{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module       : Main
-- Copyright    : (c) 2019-2023 Emily Pillmore
-- License      : BSD-style
--
-- Maintainer   : Emily Pillmore <emilypi@cohomolo.gy>
-- Stability    : Experimental
-- Portability  : portable
--
-- This module contains the test implementation for the `base32` package
--
module Main
( main
) where


import Data.Bifunctor (second)
import qualified "memory" Data.ByteArray.Encoding as Mem
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import "base32" Data.ByteString.Base32 as B32
import "base32" Data.ByteString.Base32.Hex as B32H
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Base32.Error (Base32Error(..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Short as TS
import Data.Word

import Internal

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)

import Test.QuickCheck hiding (label)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Base32 Tests"
  [ mkTree b32
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree lb32
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree sb32
    [ mkPropTree
    , mkUnitTree
    ]
  , mkTree t32
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree T.decodeUtf8' tt32 b32
    ]
  , mkTree tl32
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree TL.decodeUtf8' ttl32 lb32
    ]
  , mkTree ts32
    [ mkPropTree
    , mkUnitTree
    , mkDecodeTree
      (second TS.fromText . T.decodeUtf8' . SBS.fromShort) tts32 sb32
    ]
  ]

-- ---------------------------------------------------------------- --
-- Test tree generation

type C a = (Arbitrary a, IsString a, Eq a, Show a)

-- | Make a test tree for a given label
--
mkTree
  :: C a
  => Harness a
  -> [Harness a -> TestTree]
  -> TestTree
mkTree a = testGroup (label a) . fmap ($ a)

-- | Make a test group with some name, lifting a test tree up to the correct
-- type information via some Harness
--
mkTests
  :: C a
  => String
  -> [Harness a -> TestTree]
  -> Harness a
  -> TestTree
mkTests context ts = testGroup context . (<*>) ts . pure

-- | Make property tests for a given harness instance
--
mkPropTree :: C a => Harness a -> TestTree
mkPropTree = mkTests "Property Tests"
  [ prop_roundtrip
  , prop_correctness
  , prop_padding_invariants
  , const prop_mem_coherence
  ]

-- | Make unit tests for a given harness instance
--
mkUnitTree :: C a => Harness a -> TestTree
mkUnitTree = mkTests "Unit tests"
  [ rfcVectors
  , offsetVectors
  , validityTests
  ]

-- | Make unit tests for textual 'decode*With' functions
--
mkDecodeTree
  :: (C s, C t, Show e)
  => (s -> Either e t)
  -> TextHarness s t
  -> Harness s
  -> Harness t
  -> TestTree
mkDecodeTree utf8 s t = mkTests "Decoding tests"
  [ decodeWithVectors utf8 s t
  ]

-- ---------------------------------------------------------------- --
-- Property tests

prop_roundtrip :: C a => Harness a -> TestTree
prop_roundtrip Harness{..} = testGroup "prop_roundtrip"
  [ testProperty "prop_std_roundtrip" $ \(bs :: b) ->
      Right (encode bs) == decode (encode (encode bs))
  , testProperty "prop_hex_roundtrip" $ \(bs :: b) ->
      Right (encodeHex bs) == decodeHex (encodeHex (encodeHex bs))
  , testProperty "prop_hex_roundtrip_nopad" $ \(bs :: b) ->
      Right (encodeHexNopad bs)
        == decodeHexNopad (encodeHexNopad (encodeHexNopad bs))
  ]

prop_correctness :: C a => Harness a -> TestTree
prop_correctness Harness{..} = testGroup "prop_validity"
  [ testProperty "prop_std_valid" $ \(bs :: b) ->
    validate (encode bs)
  , testProperty "prop_hex_valid" $ \(bs :: b) ->
    validateHex (encodeHex bs)
  , testProperty "prop_std_correct" $ \(bs :: b) ->
    correct (encode bs)
  , testProperty "prop_hex_correct" $ \(bs :: b) ->
    correctHex (encodeHex bs)
  ]

prop_padding_invariants :: C a => Harness a -> TestTree
prop_padding_invariants Harness{..} = testGroup "prop_padding_invariants"
  [ testProperty "prop_hex_nopad_roundtrip" $ \(bs :: b) ->
      Right bs
        == decodeHexNopad (encodeHexNopad bs)

  , testProperty "prop_hex_pad_roundtrip" $ \(bs :: b) ->
      Right bs == decodeHexPad (encodeHex bs)

  , testProperty "prop_hex_decode_invariant" $ \(bs :: b) ->
      ( decodeHexNopad (encodeHexNopad bs)
      == decodeHex (encodeHex bs)
      ) ||
      ( decodeHexPad (encodeHex bs)
      == decodeHex (encodeHex bs)
      )

  , testProperty "prop_std_decode_invariant" $ \(bs :: b) ->
      ( decodeNopad (encodeNopad bs)
      == decode (encode bs)
      ) ||
      ( decodePad (encode bs)
      == decode (encode bs)
      )

  , testProperty "prop_hex_padding_coherence" $ \(bs :: b) ->
      Right bs == decodeHex (encodeHex bs)
      && Right bs == decodeHexPad (encodeHex bs)

  , testProperty "prop_hex_nopadding_coherence" $ \(bs :: b) ->
      Right bs == decodeHexNopad (encodeHexNopad bs)
      && Right bs == decodeHex (encodeHexNopad bs)
  ]

-- | just a sanity check against `memory`
--
prop_mem_coherence :: TestTree
prop_mem_coherence = testGroup "prop_mem_coherence"
  [ testProperty "prop_std_mem_coherence" $ \bs ->
      Right bs == B32.decodeBase32 (B32.encodeBase32' bs)
      && Right bs == Mem.convertFromBase Mem.Base32 (Mem.convertToBase @BS.ByteString @BS.ByteString Mem.Base32 bs)
  ]

-- ---------------------------------------------------------------- --
-- Unit tests

-- | RFC 4328 test vectors
--
rfcVectors :: C a => Harness a -> TestTree
rfcVectors Harness{..} = testGroup "RFC 4648 Test Vectors"
    [ testGroup "std alphabet"
      [ testCaseStd "" ""
      , testCaseStd "f" "MY======"
      , testCaseStd "fo" "MZXQ===="
      , testCaseStd "foo" "MZXW6==="
      , testCaseStd "foob" "MZXW6YQ="
      , testCaseStd "fooba" "MZXW6YTB"
      , testCaseStd "foobar" "MZXW6YTBOI======"
      ]
    , testGroup "hex alphabet"
      [ testCaseHex "" ""
      , testCaseHex "f" "CO======"
      , testCaseHex "fo" "CPNG===="
      , testCaseHex "foo" "CPNMU==="
      , testCaseHex "foob" "CPNMUOG="
      , testCaseHex "fooba" "CPNMUOJ1"
      , testCaseHex "foobar" "CPNMUOJ1E8======"
      ]
    ]
  where
    testCaseStd s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encode s

        step "decode is sound"
        Right s @=? decode (encode s)

    testCaseHex s t =
      testCaseSteps (show $ if s == "" then "empty" else s) $ \step -> do
        step "encode is sound"
        t @=? encodeHex s

        step "decode is sound"
        Right s @=? decodeHexPad t

-- | Unit test trees for the `decode*With` family of text-valued functions
--
decodeWithVectors
  :: (C s, C t, Show e)
  => (s -> Either e t)
    -- ^ utf8
  -> TextHarness s t
  -> Harness s
    -- ^ witness to the bytestring-ey dictionaries
  -> Harness t
    -- ^ witness to the text dictionaries
  -> TestTree
decodeWithVectors utf8 TextHarness{..} s t = testGroup "DecodeWith* unit tests"
  [ testGroup "decodeWith negative tests"
    [ testCase "decodeWith non-utf8 inputs on decodeUtf8" $ do
      case decodeWith_  utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeWith valid utf8 inputs on decodeUtf8" $ do
      case decodeWith_  utf8 (encode s "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeHexWith non-utf8 inputs on decodeUtf8" $ do
      case decodeHexWith_  utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodePaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodePaddedWith_ utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodePaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodePaddedWith_ utf8 (encode s "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeUnpaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeUnpaddedWith_ utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeUnpaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeUnpaddedWith_ utf8 (encodeNopad s "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeHexWith valid utf8 inputs on decodeUtf8" $ do
      case decodeHexWith_ utf8 (encodeHex s "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeHexPaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeHexPaddedWith_  utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeHexPaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeHexPaddedWith_  utf8 (encodeHex s "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    , testCase "decodeHexUnpaddedWith non-utf8 inputs on decodeUtf8" $ do
      case decodeHexUnpaddedWith_  utf8 "\1079743" of
        Left (DecodeError _) -> return ()
        _ -> assertFailure "decoding phase"
    , testCase "decodeHexUnpaddedWith valid utf8 inputs on decodeUtf8" $ do
      case decodeHexUnpaddedWith_ utf8 (encodeHexNopad s "\1079743") of
        Left (ConversionError _) -> return ()
        _ -> assertFailure "conversion phase"
    ]
  , testGroup "decodeWith positive tests"
    [ testCase "decodeWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decode t "MZXW6YTBOI======"
      b <- either (assertFailure . show) pure $ decodeWith_ utf8 "MZXW6YTBOI======"
      a @=? b
    , testCase "decodePaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodePad t "MZXW6YTBOI======"
      b <- either (assertFailure . show) pure $ decodePaddedWith_ utf8 "MZXW6YTBOI======"
      a @=? b
    , testCase "decodeUnpaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeNopad t "MZXW6YTBOI"
      b <- either (assertFailure . show) pure $ decodeUnpaddedWith_ utf8 "MZXW6YTBOI"
      a @=? b
    , testCase "decodeHexWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeHex t "CPNMUOJ1E8======"
      b <- either (assertFailure . show) pure $ decodeHexWith_ utf8 "CPNMUOJ1E8======"
      a @=? b
    , testCase "decodeHexPaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeHexPad t "CPNMUOJ1E8======"
      b <- either (assertFailure . show) pure $ decodeHexPaddedWith_ utf8 "CPNMUOJ1E8======"
      a @=? b
    , testCase "decodeHexUnpaddedWith utf8 inputs on decodeUtf8" $ do
      a <- either (assertFailure . show) pure $ decodeHexNopad t "CPNMUOJ1"
      b <- either (assertFailure . show) pure $ decodeHexUnpaddedWith_ utf8 "CPNMUOJ1"
      a @=? b
    ]
  ]

-- | Validity unit tests for the URL workflow
--
validityTests :: C a => Harness a -> TestTree
validityTests Harness{..} = testGroup "Validity and correctness unit tests"
  [ testGroup "Validity unit tests"
    [ testCase "Hex padding tests" $ do
      not (validateHex "C") @? "C"
      validateHex "CO" @? "CO"
      validateHex "CPNG" @? "CPNG"
      validateHex "CPNMU" @? "CPNMU"
      validateHex "CPNMUOG" @? "CPNMUOG"
      validateHex "CPNMUOJ1" @? "CPNMUOJ1"
      validateHex "CPNMUOJ1E8" @? "CPNMUOJ1E8"
      validateHex "CO======" @? "CO======"
      validateHex "CPNG====" @? "CPNG===="
      validateHex "CPNMU===" @? "CPNMU==="
      validateHex "CPNMUOG=" @? "CPNMUOG="
      validateHex "CPNMUOJ1" @? "CPNMUOJ1"
      validateHex "CPNMUOJ1E8======" @? "CPNMUOJ1E8======"
    , testCase "Std padding tests" $ do
      not (validate "M") @? "M"
      validate "MY" @? "MY"
      validate "MZXQ" @? "MZXQ"
      validate "MZXW6" @? "MZXW6"
      validate "MZXW6YQ" @? "MZXW6YQ"
      validate "MZXW6YTB" @? "MZXW6YTB"
      validate "MZXW6YTBOI" @? "MZXW6YTBOI"
      validate "MY======" @? "MY======"
      validate "MZXQ====" @? "MZXQ===="
      validate "MZXW6===" @? "MZXW6==="
      validate "MZXW6YQ=" @? "MZXW6YQ="
      validate "MZXW6YTB" @? "MZXW6YTB"
      validate "MZXW6YTBOI======" @? "MZXW6YTBOI======"
    ]
  , testGroup "Correctness unit tests"
    [ testCase "Hex tests" $ do
      not (correctHex "C") @? "C"
      correctHex "CO" @? "CO"
      correctHex "CPNG" @? "CPNG"
      correctHex "CPNMU" @? "CPNMU"
      correctHex "CPNMUOG" @? "CPNMUOG"
      correctHex "CPNMUOJ1" @? "CPNMUOJ1"
      correctHex "CPNMUOJ1E8" @? "CPNMUOJ1E8"
      correctHex "CO======" @? "CO======"
      correctHex "CPNG====" @? "CPNG===="
      correctHex "CPNMU===" @? "CPNMU==="
      correctHex "CPNMUOG=" @? "CPNMUOG="
      correctHex "CPNMUOJ1" @? "CPNMUOJ1"
      correctHex "CPNMUOJ1E8======" @? "CPNMUOJ1E8======"
    , testCase "Std tests" $ do
      not (correct "M") @? "M"
      correct "MY" @? "MY"
      correct "MZXQ" @? "MZXQ"
      correct "MZXW6" @? "MZXW6"
      correct "MZXW6YQ" @? "MZXW6YQ"
      correct "MZXW6YTB" @? "MZXW6YTB"
      correct "MZXW6YTBOI" @? "MZXW6YTBOI"
      correct "MY======" @? "MY======"
      correct "MZXQ====" @? "MZXQ===="
      correct "MZXW6===" @? "MZXW6==="
      correct "MZXW6YQ=" @? "MZXW6YQ="
      correct "MZXW6YTB" @? "MZXW6YTB"
      correct "MZXW6YTBOI======" @? "MZXW6YTBOI======"
    ]
  ]

-- | Offset test vectors. This stresses the invalid char + incorrect padding
-- offset error messages
--
offsetVectors :: C a => Harness a -> TestTree
offsetVectors Harness{..} = testGroup "Offset tests"
  [ testGroup "Hex - Invalid padding"
    [ testCase "Invalid staggered padding" $ do
      decodeHex "=PNMUOJ1E8======" @?= Left "invalid padding at offset: 0"
      decodeHex "C=NMUOJ1E8======" @?= Left "invalid padding at offset: 1"
      decodeHex "CP=MUOJ1E8======" @?= Left "invalid padding at offset: 2"
      decodeHex "CPN=UOJ1E8======" @?= Left "invalid padding at offset: 3"
      decodeHex "CPNM=OJ1E8======" @?= Left "invalid padding at offset: 4"
      decodeHex "CPNMU=J1E8======" @?= Left "invalid padding at offset: 5"
      decodeHex "CPNMUO=1E8======" @?= Left "invalid padding at offset: 6"
      decodeHex "CPNMUOJ=E8======" @?= Left "invalid padding at offset: 7"
    , testCase "Invalid character coverage" $ do
      decodeHex "%PNMUOJ1E8======" @?= Left "invalid character at offset: 0"
      decodeHex "C%NMUOJ1E8======" @?= Left "invalid character at offset: 1"
      decodeHex "CP%MUOJ1E8======" @?= Left "invalid character at offset: 2"
      decodeHex "CPN%UOJ1E8======" @?= Left "invalid character at offset: 3"
      decodeHex "CPNM%OJ1E8======" @?= Left "invalid character at offset: 4"
      decodeHex "CPNMU%J1E8======" @?= Left "invalid character at offset: 5"
      decodeHex "CPNMUO%1E8======" @?= Left "invalid character at offset: 6"
      decodeHex "CPNMUOJ%E8======" @?= Left "invalid character at offset: 7"
    ]
  , testGroup "Std - Invalid padding"
    [ testCase "Invalid staggered padding" $ do
      decode "=ZXW6YTBOI======" @?= Left "invalid padding at offset: 0"
      decode "M=XW6YTBOI======" @?= Left "invalid padding at offset: 1"
      decode "MZ=W6YTBOI======" @?= Left "invalid padding at offset: 2"
      decode "MZX=6YTBOI======" @?= Left "invalid padding at offset: 3"
      decode "MZXW=YTBOI======" @?= Left "invalid padding at offset: 4"
      decode "MZXW6=TBOI======" @?= Left "invalid padding at offset: 5"
      decode "MZXW6Y=BOI======" @?= Left "invalid padding at offset: 6"
      decode "MZXW6YT=OI======" @?= Left "invalid padding at offset: 7"
    , testCase "Invalid character coverage" $ do
      decode "%ZXW6YTBOI======" @?= Left "invalid character at offset: 0"
      decode "M%XW6YTBOI======" @?= Left "invalid character at offset: 1"
      decode "MZ%W6YTBOI======" @?= Left "invalid character at offset: 2"
      decode "MZX%6YTBOI======" @?= Left "invalid character at offset: 3"
      decode "MZXW%YTBOI======" @?= Left "invalid character at offset: 4"
      decode "MZXW6%TBOI======" @?= Left "invalid character at offset: 5"
      decode "MZXW6Y%BOI======" @?= Left "invalid character at offset: 6"
      decode "MZXW6YT%OI======" @?= Left "invalid character at offset: 7"
    ]
  ]
