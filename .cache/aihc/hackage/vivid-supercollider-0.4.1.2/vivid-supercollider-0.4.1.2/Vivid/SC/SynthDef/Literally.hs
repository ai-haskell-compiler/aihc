
-- |  __You probably don't need to use this directly__ -- use "Vivid.SynthDef" instead
-- 
--   This is a representation of how SynthDefs are sent over the wire, as described in the
--   < http://doc.sccode.org/Reference/Synth-Definition-File-Format.html Synth Definition File Format >
--   helpfile.

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE
     NoRebindableSyntax
   , ScopedTypeVariables
   , LambdaCase
   , OverloadedStrings

   , NoIncoherentInstances
   , NoMonomorphismRestriction
   , NoUndecidableInstances
   #-}
  -- TODO: remove ScopedTypeVariables?

module Vivid.SC.SynthDef.Literally (
     LiteralSynthDef(..)
   , encodeSynthDefFile
   , decodeSynthDefFile
   , encodeLiteralSynthDef
   , decodeLiteralSynthDef
   , getLiteralSynthDef'
   , putLiteralSynthDef'

   , UGenSpec(..)
   , InputSpec(..)
   , ParamName(..)
   , SynthDefFile(..)
   , OutputSpec(..)
   , VariantSpec(..)

   , uOpToSpecialI
   , biOpToSpecialI
   , specialIToUOp
   , specialIToBiOp

   , sdLitPretty

   -- For testing: -- todo: maybe 'Internal' module
   , putPString'
   , getPString'
   , encodePString
   , getPString

   , getParamName
   , getParamName'
   , putParamName'
   , encodeParamName

   , getInputSpec'
   , putInputSpec'
   , getInputSpec
   , encodeInputSpec

   , getOutputSpec'
   , putOutputSpec'
   , getOutputSpec
   , encodeOutputSpec

   , getCalcRate'
   , putCalcRate'

   , getUGenSpec
   , getUGenSpec'
   , encodeUGenSpec
   , putUGenSpec'

   , getVariantSpec
   , getVariantSpec'
   , putVariantSpec'
   , encodeVariantSpec
   ) where

import Vivid.SC.SynthDef.Types (BinaryOp(..), UnaryOp(..), CalculationRate(..))
-- import Vivid.OSC.Old.Util (floatToWord, wordToFloat)

-- For GHC 7.8 and below:
-- (Eventually remove)
import Control.Applicative (pure, (<$>))

import Control.Arrow (first)
import Control.Monad
import Data.Binary (decode, encode)
-- import Data.Binary.Get
-- import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (cons, splitAt, drop, head, length)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL (fromStrict, toStrict)
import Data.Int
import Data.List.Split (chunksOf)
import Data.Monoid
-- cereal. From the sc docs: "all data is stored big endian"
import Data.Serialize hiding (decode, encode)
-- import Data.Serialize.Get
-- import Data.Serialize.Put
-- import Data.Serialize.IEEE754


-- TODO: temporary:
import Data.Bits ((.&.), complement, Bits)
import qualified Foreign as F
import System.IO.Unsafe (unsafePerformIO)
data LiteralSynthDef
   = LiteralSynthDef {
    _synthDefName :: ByteString -- pstring: "a pascal format string: a byte giving the length followed by that many bytes"
   ,_synthDefConstants :: [Float]
   ,_synthDefParameters :: [Float] -- Initial values
   ,_synthDefParamNames :: [ParamName]
   ,_synthDefUGens :: [UGenSpec]
   ,_synthDefVariants :: [VariantSpec]
   }
 deriving (Show, Eq)

data SynthDefFile = SynthDefFile [LiteralSynthDef]
 deriving (Show)

decodeSynthDefFile :: ByteString -> Either String SynthDefFile
decodeSynthDefFile blob =
   let (top, rest) = BS.splitAt 4 blob
       (fileVersion :: Int32, rest2) =
         first (decode . BSL.fromStrict) $ BS.splitAt 4 rest
   in if (top /= "SCgf" || fileVersion /= 2)
      then Left $ "screwed up synthdef file " <> show top <> show fileVersion
      else let (numberOfSynthDefs :: Int16, rest3) =
                  first (decode . BSL.fromStrict) $ BS.splitAt 2 rest2
               (synthDefs, rest4) =
                  getNWith numberOfSynthDefs decodeLiteralSynthDef rest3
           in if rest4 /= ""
              then Left $ "leftover data: " <> show rest4
              else Right $ SynthDefFile synthDefs

{-
a synth-definition-file is :
int32 - four byte file type id containing the ASCII characters: "SCgf"
int32 - file version, currently 2.
int16 - number of synth definitions in this file (D).
[ synth-definition ] * D
-}

encodeSynthDefFile :: SynthDefFile -> ByteString
encodeSynthDefFile (SynthDefFile synthDefs) = mconcat [
     "SCgf"
   , BSL.toStrict $ encode (2 :: Int32)
   , BSL.toStrict $ encode (toEnum ((length::[a]->Int) synthDefs) :: Int16)
   , mconcat $ map encodeLiteralSynthDef synthDefs
   ]

-- Yes, the 'restN's are ugly, yes i could have used a state monad. Don't judge!
decodeLiteralSynthDef :: ByteString -> (LiteralSynthDef, {- rest: -} ByteString)
decodeLiteralSynthDef blob =
   let (name :: ByteString, rest) = getPString blob
       (numConstants :: Int32, rest2) = getInt32 rest
       (constants :: [Float], rest3) =
          first (map (wordToFloat . decode . BSL.fromStrict)) $ getN4ByteBlocks numConstants rest2
       (numParams :: Int32, rest4) = getInt32 rest3
       (params :: [Float], rest5) =
          first (map (wordToFloat . decode . BSL.fromStrict)) $ getN4ByteBlocks numParams rest4
       (numParamNames :: Int32, rest6) = getInt32 rest5
       (paramNames :: [ParamName], rest7) = getNWith numParamNames getParamName rest6
       (numUGens :: Int32, rest8) = getInt32 rest7
       (uGens, rest9) = getNWith numUGens getUGenSpec rest8
       (numVariants, rest10) = getInt16 rest9
       (variantSpecs, rest11) = getNWith numVariants (getVariantSpec numParams) rest10
   in (LiteralSynthDef name constants params paramNames uGens variantSpecs, rest11)

getLiteralSynthDef' :: Get LiteralSynthDef
getLiteralSynthDef' = do
   name <- getPString'
   -- Can dedupe these 5 (and others like it):
   numConstants <- getInt32be
   constants <- replicateM (fromEnum numConstants) getFloat32be

   numParams <- getInt32be
   params <- replicateM (fromEnum numParams) getFloat32be

   numParamNames <- getInt32be
   paramNames <- replicateM (fromEnum numParamNames) getParamName'

   numUGens <- getInt32be
   ugens <- replicateM (fromEnum numUGens) getUGenSpec'

   -- Note this is 16, not 32:
   numVariants <- getInt16be
   variants <- replicateM (fromEnum numVariants) (getVariantSpec' numParams)

   pure $ LiteralSynthDef name constants params paramNames ugens variants

{-
a synth-definition is :
pstring - the name of the synth definition
int32 - number of constants (K)
[float32] * K - constant values
int32 - number of parameters (P)
[float32] * P - initial parameter values
int32 - number of parameter names (N)
[ param-name ] * N
int32 - number of unit generators (U)
[ ugen-spec ] * U
int16 - number of variants (V)
[ variant-spec ] * V
-}

encodeLiteralSynthDef :: LiteralSynthDef -> ByteString
encodeLiteralSynthDef (LiteralSynthDef name constants params paramNames uGenSpecs variants) = mconcat [
     encodePString name
   , BSL.toStrict $ encode (toEnum ((length::[a]->Int) constants) :: Int32)
   , mconcat $ map (BSL.toStrict . encode . floatToWord) constants
   , BSL.toStrict $ encode (toEnum ((length::[a]->Int) params) :: Int32)
   , mconcat $ map (BSL.toStrict . encode . floatToWord) params
   , BSL.toStrict $ encode (toEnum ((length::[a]->Int) paramNames) :: Int32)
   , mconcat $ map encodeParamName paramNames
   , BSL.toStrict $ encode (toEnum ((length::[a]->Int) uGenSpecs) :: Int32)
   , mconcat $ map encodeUGenSpec uGenSpecs
   , BSL.toStrict $ encode (toEnum ((length::[a]->Int) variants) :: Int16)
   , mconcat $ map encodeVariantSpec variants
   ]


-- TODO: temporary:
-- from hosc:
align :: (Num i,Bits i) => i -> i
{-# INLINE align #-}
align n = ((n + 3) .&. complement 3) - n

-- from data-binary-ieee754:
floatToWord :: Float -> F.Word32
floatToWord = coercionThing

wordToFloat :: F.Word32 -> Float
wordToFloat = coercionThing

doubleToWord :: Double -> F.Word64
doubleToWord = coercionThing

wordToDouble :: F.Word64 -> Double
wordToDouble = coercionThing


coercionThing :: (F.Storable a, F.Storable b) => a -> b
coercionThing x = unsafePerformIO $ F.alloca $ \buf -> do
   F.poke (F.castPtr buf) x
   F.peek buf


putLiteralSynthDef' :: LiteralSynthDef -> Put
putLiteralSynthDef' (LiteralSynthDef name constants params paramNames ugenSpecs variants) = do
   putPString' name
   putListWLenPrefix32 putFloat32be constants
   putListWLenPrefix32 putFloat32be params
   putListWLenPrefix32 putParamName' paramNames
   putListWLenPrefix32 putUGenSpec' ugenSpecs
   putListWLenPrefix16 putVariantSpec' variants

putListWLenPrefix32 :: (a -> Put) -> [a] -> Put
putListWLenPrefix32 putF l = do
   -- TODO: maybe this should be 'Int32' to be compliant:
   -- (and maybe others, for the read?)
   putWord32be $ toEnum $ (length::[a]->Int) l
   mapM_ putF l

putListWLenPrefix16 :: (a -> Put) -> [a] -> Put
putListWLenPrefix16 putF l = do
   -- TODO: maybe this should be 'Int32' to be compliant:
   -- (and maybe others, for the read?)
   putWord16be $ toEnum $ (length::[a]->Int) l
   mapM_ putF l

data ParamName = ParamName {
    _paramName_name :: ByteString
   ,_paramName_indexInParamArray :: Int32
   }
 deriving (Show, Eq)

{-
a param-name is :
pstring - the name of the parameter
int32 - its index in the parameter array
-}

getParamName :: ByteString -> (ParamName, ByteString)
getParamName blob =
   let (name, rest) = getPString blob
       (index, rest2) = getInt32 rest
   in (ParamName name index, rest2)

getParamName' :: Get ParamName
getParamName' = do
   name <- getPString'
   index <- getInt32be
   pure $ ParamName name index

encodeParamName :: ParamName -> ByteString
encodeParamName (ParamName name index) =
   encodePString name <> BSL.toStrict (encode index)

putParamName' :: ParamName -> Put
putParamName' (ParamName name index) = do
   putPString' name
   putInt32be index


data UGenSpec = UGenSpec {
    _uGenSpec_name :: ByteString
   ,_uGenSpec_calcRate :: CalculationRate
   ,_uGenSpec_inputs :: [InputSpec]
   ,_uGenSpec_outputs :: [OutputSpec]
   ,_uGenSpec_specialIndex :: Int16
   }
 deriving (Show, Eq)

{-
a ugen-spec is :
pstring - the name of the SC unit generator class
int8 - calculation rate
int32 - number of inputs (I)
int32 - number of outputs (O)
int16 - special index
[ input-spec ] * I
[ output-spec ] * O
-}

getUGenSpec :: ByteString -> (UGenSpec, {- rest: -} ByteString)
getUGenSpec blob =
   let (name, rest) = getPString blob
       (calcRate :: CalculationRate, rest2) =
          first ((toEnum) . (fromEnum :: Int8 -> Int) . decode . BSL.fromStrict) $
             BS.splitAt 1 rest
       (numInputs :: Int32, rest3) = getInt32 rest2
       (numOutputs :: Int32, rest4) = getInt32 rest3

       (specialIndex, rest5) = getInt16 rest4
       (inputSpecs, rest6) = getNWith numInputs getInputSpec rest5
       (outputSpecs, rest7) = getNWith numOutputs getOutputSpec rest6

   in (UGenSpec name calcRate inputSpecs outputSpecs specialIndex, rest7)

getUGenSpec' :: Get UGenSpec
getUGenSpec' = do
   name <- getPString'
   calcRate <- getCalcRate'
   numInputs <- fromEnum <$> getInt32be
   numOutputs <- fromEnum <$> getInt32be
   specialIndex <- getInt16be
   inputSpecs <- replicateM numInputs getInputSpec'
   outputSpecs <- replicateM numOutputs getOutputSpec'
   pure $ UGenSpec name calcRate inputSpecs outputSpecs specialIndex

encodeUGenSpec :: UGenSpec -> ByteString
encodeUGenSpec (UGenSpec name calcRate inputSpecs outputSpecs specialIndex) = mconcat [
    encodePString name
   ,BSL.toStrict $ encode $ (toEnum (fromEnum calcRate) :: Int8)
   ,BSL.toStrict $ encode $ (toEnum ((length::[a]->Int) inputSpecs) :: Int32)
   ,BSL.toStrict $ encode $ (toEnum ((length::[a]->Int) outputSpecs) :: Int32)
   ,BSL.toStrict $ encode specialIndex
   ,mconcat $ map encodeInputSpec inputSpecs
   ,mconcat $ map encodeOutputSpec outputSpecs
   ]

putUGenSpec' :: UGenSpec -> Put
putUGenSpec' (UGenSpec name calcRate inputSpecs outputSpecs specialIndex) = do
   putPString' name
   putCalcRate' calcRate
   putInt32be $ toEnum $ (length::[a]->Int) inputSpecs
   putInt32be $ toEnum $ (length::[a]->Int) outputSpecs
   putInt16be specialIndex
   mapM_ putInputSpec' inputSpecs
   mapM_ putOutputSpec' outputSpecs

-- TODO:
-- | These record fields are deprecated as it's a variant -- don't use!
data InputSpec
   = InputSpec_UGen {
    _inputSpec_uGen_index :: Int32
   ,_inputSpec_uGen_outputIndex :: Int32
   }
   | InputSpec_Constant {
    _inputSpec_constant_index :: Int32
   }
 deriving (Show, Read, Eq)

{-
an input-spec is :
int32 - index of unit generator or -1 for a constant
if (unit generator index == -1) :
int32 - index of constant
else :
int32 - index of unit generator output
-}

getInputSpec :: ByteString -> (InputSpec, ByteString)
getInputSpec blob =
   let (stuffForThis, rest) = BS.splitAt 8 blob
       (one :: Int32, two :: Int32) = (\(a,b) -> ((decode . BSL.fromStrict) a, (decode . BSL.fromStrict) b)) $ BS.splitAt 4 stuffForThis
       spec = case one of
          -1 -> InputSpec_Constant two
          n | n > -1 -> InputSpec_UGen one two
          _ -> error "bad number"
   in (spec, rest)

getInputSpec' :: Get InputSpec
getInputSpec' = do
   index <- getInt32be
   outputOrConstantIndex <- getInt32be
   pure $ case index of
      -1 -> InputSpec_Constant outputOrConstantIndex
      n | n > -1 -> InputSpec_UGen index outputOrConstantIndex
      _ -> error "bad number" -- TODO

encodeInputSpec :: InputSpec -> ByteString
encodeInputSpec inputSpec = mconcat $ map (BSL.toStrict . encode) $ encodeInputSpec' inputSpec
 where
   encodeInputSpec' :: InputSpec -> [Int32]
   encodeInputSpec' (InputSpec_Constant i) = [ (-1), i ]
   encodeInputSpec' (InputSpec_UGen i oI) = [ i, oI ]

putInputSpec' :: InputSpec -> Put
putInputSpec' = \case
   InputSpec_Constant constant -> do
      putInt32be (-1)
      putInt32be constant
   InputSpec_UGen index constant -> do
      putInt32be index
      putInt32be constant

data OutputSpec = OutputSpec { _outputSpec_calcRate :: CalculationRate }
 deriving (Show, Read, Eq)

{-
an output-spec is :
int8 - calculation rate
-}

getOutputSpec :: ByteString -> (OutputSpec, ByteString)
getOutputSpec blob =
      first (OutputSpec . toEnum . (fromEnum :: Int8 -> Int) . decode . BSL.fromStrict) $
         BS.splitAt 1 blob

getOutputSpec' :: Get OutputSpec
getOutputSpec' = do
   OutputSpec <$> getCalcRate'

encodeOutputSpec :: OutputSpec -> ByteString
encodeOutputSpec (OutputSpec calcRate) =
   BSL.toStrict $ encode $ (toEnum (fromEnum calcRate) :: Int8)

putOutputSpec' :: OutputSpec -> Put
putOutputSpec' (OutputSpec calcRate) =
   putCalcRate' calcRate

getCalcRate' :: Get CalculationRate
getCalcRate' =
   (toEnum . fromEnum) <$> getInt8

putCalcRate' :: CalculationRate -> Put
putCalcRate' calcRate = 
   putWord8 $ toEnum $ fromEnum calcRate

data VariantSpec
   = VariantSpec {
    _variantSpec_name :: ByteString
   ,_variantSpec_initialParamVals :: [Float] -- float32
   }
 deriving (Show, Eq)

{-
a variant-spec is :
pstring - the name of the variant
[float32] * P - variant initial parameter values
-}

getVariantSpec :: Int32 -> ByteString -> (VariantSpec, ByteString)
getVariantSpec numParams blob =
   let (name, rest) = getPString blob
       (initialParamVals :: [Float], rest2) = first (map (decode . BSL.fromStrict)) $ getN4ByteBlocks numParams rest
   in (VariantSpec name initialParamVals, rest2)

getVariantSpec' :: Int32 -> Get VariantSpec
getVariantSpec' numParams = do
   name <- getPString'
   -- numVariants <- getInt16be -- NOT numVariants
   floats <- replicateM (fromEnum numParams) $ do
      -- make sure this is right!:
      -- wordToFloat <$> getWord32be
      getFloat32be
   pure $ VariantSpec name floats


encodeVariantSpec :: VariantSpec -> ByteString
encodeVariantSpec (VariantSpec name initialParamVals) =
   encodePString name <> mconcat (map (BSL.toStrict . encode . floatToWord) initialParamVals)

putVariantSpec' :: VariantSpec -> Put
putVariantSpec' (VariantSpec name initialParamVals) = do
   putPString' name
   mapM_ putFloat32be initialParamVals

--- helpers:
getPString :: ByteString -> (ByteString, {- rest: -} ByteString)
getPString blob = first (BS.drop 1) $
   BS.splitAt (fromEnum (BS.head blob) + 1) blob

-- | A \"pascal format string\"
getPString' :: Get ByteString
getPString' = do
   byteLen <- getWord8 -- why's this not 'getWord8be'? - todo
   getByteString $ fromEnum byteLen

encodePString :: ByteString -> ByteString
encodePString s = toEnum (BS.length s) `BS.cons` s

putPString' :: ByteString -> Put
putPString' bs = do
   putWord8 $ toEnum (BS.length bs)
   putByteString bs

-- Pretty sure this is exactly replicateM:
getNWith :: (Integral i) => i -> (ByteString -> (a, ByteString)) -> ByteString -> ([a], ByteString)
getNWith 0 _ rest = ([], rest)
getNWith n f rest =
   let (head1, rest2) = f rest
       (head2, rest3) = getNWith (n - 1) f rest2
   in (head1 : head2, rest3)
   
getInt32 :: ByteString -> (Int32, ByteString)
getInt32 blob = first (decode . BSL.fromStrict) $ BS.splitAt 4 blob

getInt16 :: ByteString -> (Int16, ByteString)
getInt16 blob = first (decode . BSL.fromStrict) $ BS.splitAt 2 blob

getN4ByteBlocks :: Int32 -> ByteString -> ([ByteString], ByteString)
getN4ByteBlocks numBlocks blob =
   first (map BS8.pack . chunksOf 4 . BS8.unpack) $
             BS.splitAt (4 * fromEnum numBlocks) blob



uOpToSpecialI :: UnaryOp -> Int16
uOpToSpecialI uop = toEnum . fromEnum $ uop

specialIToUOp :: Int16 -> UnaryOp
specialIToUOp specialI = toEnum . fromEnum $ specialI

biOpToSpecialI :: BinaryOp -> Int16
biOpToSpecialI theBiOp = toEnum . fromEnum $ theBiOp

specialIToBiOp :: Int16 -> BinaryOp
specialIToBiOp theBiOp = toEnum . fromEnum $ theBiOp


sdLitPretty :: LiteralSynthDef -> String
sdLitPretty synthDef = mconcat [
    "Constants: ", show $ _synthDefConstants synthDef
  , "\n"
--  , show $ zip (_synthDefParameters synthDef) (_synthDefParamNames synthDef)
  , show $ map (\(ParamName a i)->(a, _synthDefParameters synthDef !! fromIntegral i)) (_synthDefParamNames synthDef)
  , "\n"
  , mconcat$
      (flip map) (zip [0::Int ..] (_synthDefUGens synthDef)) $ \(i,ug) -> mconcat [
                show i <> " " <> show (_uGenSpec_name ug) <> " - " <> show (_uGenSpec_calcRate ug) <> " (" <> show ((length::[a]->Int) $ _uGenSpec_outputs ug) <> " outputs)"
               ,"\n"
               ,mconcat $ map ((<>"\n") . ("  "<>) . showInputSpec) $ _uGenSpec_inputs ug
               ,case UTF8.toString (_uGenSpec_name ug) of
                   "UnaryOpUGen" -> mconcat [ "  "
                      , show ( specialIToUOp (_uGenSpec_specialIndex ug))
                      , "\n" ]
                   "BinaryOpUGen" ->
                      "  " <> show (specialIToBiOp (_uGenSpec_specialIndex ug)) <> "\n"

                   _ -> ""
               ]
  ]
 where
   showInputSpec :: InputSpec -> String
   showInputSpec (InputSpec_Constant constantIndex) = mconcat [
       "Constant: "
      ,show $ (_synthDefConstants synthDef) !! fromEnum constantIndex
      ," (index ", show constantIndex, ")"
      ]
   showInputSpec (InputSpec_UGen ugNum ugOut) =
      "UGOut: "<>show (ugNum, ugOut)
--   showInputSpec x = show x
