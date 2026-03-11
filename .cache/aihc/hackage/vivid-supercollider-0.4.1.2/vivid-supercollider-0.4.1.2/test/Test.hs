{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Data.Binary (decode, encode)
-- import Data.Binary.Get
-- import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BSL
import Data.Int
-- import Data.Monoid
import Data.Serialize hiding (decode, encode)
-- import Data.Serialize.Get
-- import Data.Serialize.Put
-- import Data.Serialize.IEEE754
import Data.Word


import Test.Microspec
-- import Test.QuickCheck ()
import Test.QuickCheck.Arbitrary ()

import Vivid.OSC.Old.Util
import Vivid.SC.SynthDef.Literally
import Vivid.SC.SynthDef.Types

main :: IO ()
main = microspec $ do
   describe "synthdef literal representation" $ do
      describe "the whole enchilada" $ do
         it "correctly does (encode . decode)" $ do
            \origSDLiteral_wVariants ->
               let origSDLiteral = origSDLiteral_wVariants { _synthDefVariants = [] }
                   encoded :: ByteString
                   encoded = encodeLiteralSynthDef origSDLiteral
                   (decoded, "") = decodeLiteralSynthDef encoded
               in decoded === origSDLiteral
         it "new format does (get . put)" $ \sdl ->
            runGet getLiteralSynthDef' (runPut (putLiteralSynthDef' sdl)) === Right sdl
         it "oldEncode == newEncode" $ \sdl_wVariants ->
            let sdl = sdl_wVariants { _synthDefVariants = [] }
            in (encodeLiteralSynthDef sdl)
                  === runPut (putLiteralSynthDef' sdl)
      describe "components" $ do
         describe "pascal strings" $ do
            it "put->get" $ do
               \ws ->
                   let bs = BS.pack $ take (fromEnum (maxBound::Word8)) ws
                   in runGet getPString' (runPut (putPString' bs)) === Right bs
            it "get->put" $ pending
            describe "compared to old stuff" $ do
               it "putPString' == encodePString" pending
               it "getPString' == getPString" pending
         describe "UGenSpecs" $ do
            it "get . put" $ \us ->
               runGet getUGenSpec' (runPut (putUGenSpec' us)) == Right us
            it "encodeUGenSpec == putUGenSpec" $ \us ->
               runPut (putUGenSpec' us)
                  === encodeUGenSpec us
            it "(oldGet . (old)put) == (newGet . (old)put)" $ \us ->
               runGet getUGenSpec' (encodeUGenSpec us)
                  == Right (fst (getUGenSpec (encodeUGenSpec us)))
         describe "input specs" $ do
            it "get . put" $ \is ->
               runGet getInputSpec' (runPut (putInputSpec' is)) === Right is
            it "encodeInputSpec == putInputSpec'" $ \is ->
               runPut (putInputSpec' is)
                  === encodeInputSpec is
            it "(oldGet . put) == (newGet . put)" $ \is ->
               runGet getInputSpec' (runPut (putInputSpec' is))
                  === (Right $ fst $ getInputSpec $ runPut $ putInputSpec' is)
         describe "output specs" pending
         describe "param names" $ do
            -- It feels like this is enough to prove sameness, but
            -- could actually think about it more when i'm not so
            -- tired:
            it "get . put" $ \pn ->
               runGet getParamName' (runPut (putParamName' pn)) === Right pn
            it "encodeParamName == putParamName'" $ \pn ->
               runPut (putParamName' pn)
                  === encodeParamName pn
            it "(oldDecode . encode) == (newGet . newPut)" $ \pn ->
               runGet getParamName' (runPut (putParamName' pn))
                  === (Right $ fst $ getParamName $ runPut $ putParamName' pn)
         describe "calc rate" $ do
            it "get . put" $ \cr ->
               runGet getCalcRate' (runPut (putCalcRate' cr)) == Right cr
         describe "variant specs" $ do
            it "get . put" $ \vs ->
               let numParams = toEnum $ (length::[a]->Int) $ _variantSpec_initialParamVals vs
               in runGet (getVariantSpec' numParams) (runPut (putVariantSpec' vs)) === Right vs
         describe "uopToSpecialI roundtrips" pending
         describe "biopToSpecialI roundtrips" pending
   -- A lot of this can be removed now that we're using 'cereal'
   describe "utilities" $ do
      describe "wordToFloat<->floatToWord" $ do
         it "word->float->word" $ do
            \w -> floatToWord (wordToFloat w) === w
         it "float->word->float" $ do
            \f -> wordToFloat (floatToWord f) === f
         describe "with put/get" $ pending
            -- undefined $ runPut (putFloat (wordToFloat 2139095041))
         describe "with encode/decode" $ do
            it "word->float->word" $ do
               \w -> w == (floatToWord
                  $ decode
                  $ BSL.fromStrict
                  $ BSL.toStrict
                  $ encode
                  $ wordToFloat
                  $ w)
            it "float->word->float" $ do
               \f -> f == (wordToFloat
                  $ decode
                  $ BSL.fromStrict
                  $ BSL.toStrict
                  $ encode
                  $ floatToWord
                  $ f)
   describe "commands" $ do
      describe "/d_load" $ do
         it "unit test, with beyonc`e" pending



-- Note these might NOT be valid usens, only in the valid format
-- (e.g. you could refer to parameters that don't exist, etc)
instance Arbitrary LiteralSynthDef where
   arbitrary = do
      name <- (UTF8.fromString . getNonEmpty) <$> arbitrary
      constants <- arbitrary
      numParameters <- choose (0,100)
      parameters <- replicateM numParameters arbitrary :: Gen [Float]
      paramNames <- replicateM ((length::[a]->Int) parameters) (arbitrary :: Gen ParamName)
      ugens <- arbitrary
      numVariantSpecs <- choose (0, 10)
      variantSpecs <- replicateM numVariantSpecs $
          -- Needs to be same as number of params:
         arbitraryVariantSpecWithNParams (toEnum numParameters)
      pure $ LiteralSynthDef {
           _synthDefName = name
         , _synthDefConstants = constants
         , _synthDefParameters = parameters
         , _synthDefParamNames = paramNames
         , _synthDefUGens = ugens
         , _synthDefVariants = variantSpecs
         }

-- Again, these might not be sensible, just syntactically valid
instance Arbitrary ParamName where
   arbitrary =
      ParamName
         <$> ((UTF8.fromString . getNonEmpty) <$> arbitrary)
         <*> arbitrary

-- todo: generate this with generics
instance Arbitrary InputSpec where
   arbitrary = oneof [
        InputSpec_Constant <$> arbitrary
      , InputSpec_UGen
           -- TODO: do we ever want to test it with an invalid number?:
           <$> (getNonNegative <$> arbitrary)
           <*> arbitrary
      ]

instance Arbitrary OutputSpec where
   arbitrary = OutputSpec <$> arbitrary

-- todo: generics:
instance Arbitrary CalculationRate where
   arbitrary = elements [IR, KR, AR, DR]

instance Arbitrary UGenSpec where
   arbitrary = UGenSpec
      <$> arbitraryPString
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

-- | Defined *instead* of an arbitrary instance because all
--   VariantSpecs come in a list, and each spec in that list
--   must have the same number of parameters

-- TODO: no, not instead of: just also
-- TODO: i think should be 'word32' not 'int32':
arbitraryVariantSpecWithNParams :: Int32 -> Gen VariantSpec
arbitraryVariantSpecWithNParams n =
   VariantSpec
      <$> arbitraryPString
      <*> (replicateM (fromEnum n) arbitrary)

-- Might not want to keep this one (for the reason above) - todo:
instance Arbitrary VariantSpec where
   arbitrary = do
      n <- mod 100 <$> arbitrary
      arbitraryVariantSpecWithNParams n

-- The length has to fit into one word8!:
arbitraryPString :: Gen ByteString
arbitraryPString = do
   numChars <- choose (0, maxBound :: Word8)
   BS.pack <$> replicateM (fromEnum numChars) arbitrary
