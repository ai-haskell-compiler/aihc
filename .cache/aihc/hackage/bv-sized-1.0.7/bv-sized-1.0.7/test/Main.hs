{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- Testing modules
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

-- Modules under test
import qualified Data.BitVector.Sized as BV
import qualified Data.BitVector.Sized.Unsigned as BV
import qualified Data.BitVector.Sized.Signed as BV

-- Auxiliary modules
import Control.Monad.Random
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Maybe (isJust, fromJust)
import Data.Parameterized.NatRepr
import Data.Parameterized.Some
import Data.Parameterized.Pair
import Data.String ( fromString )
import Data.Word
import Numeric.Natural

-- | This is a wrapper around 'testPropertyNamed' that somewhat addresses a
-- deprecation warning.  Newer versions of the test library deprecated
-- 'testProperty' in favor of 'testPropertyNamed', which is intended to provide
-- better human-readable instructions to reproduce test failures.
--
-- However, doing that requires all properties to have a top-level definition
-- corresponding to them, which we do not here. We just re-use the string
-- description as the test name, which will produce incorrect instructions to
-- reproduce failures, but will provide sufficient context clues for a developer
-- to figure out which test failed.
--
-- The alternative is to refactor all of the tests into top-level properties to
-- use the API as intended.
testPropertyString :: String -> Property -> TestTree
testPropertyString desc = testPropertyNamed desc (fromString desc)

----------------------------------------
-- Utilities
forcePos :: (1 <= w => NatRepr w -> a)
         -> NatRepr w -> a
forcePos f w = case isZeroOrGT1 w of
  Left Refl -> error "Main.forcePos: encountered 0 nat"
  Right LeqProof -> f w

----------------------------------------
-- Homomorphisms
un :: Show a
   => Gen (Some NatRepr)
   -- ^ generator for width
   -> (forall w . NatRepr w -> a -> BV.BV w)
   -- ^ morphism
   -> (forall w . NatRepr w -> Gen a)
   -- ^ generator for arg
   -> (forall w . NatRepr w -> a -> a)
   -- ^ unary operator on domain
   -> (forall w . NatRepr w -> BV.BV w -> BV.BV w)
   -- ^ unary operator on codomain
   -> Property
un genW p gen aOp bOp = property $ do
  Some w <- forAll genW
  a <- forAll (gen w)

  p w (aOp w a) === bOp w (p w a)

bin :: Show a
    => Gen (Some NatRepr)
    -- ^ generator for width
    -> (forall w. NatRepr w -> a -> BV.BV w)
    -- ^ morphism on domains
    -> (forall w. NatRepr w -> Gen a)
    -- ^ generator for first arg
    -> (forall w. NatRepr w -> Gen a)
    -- ^ generator for second arg
    -> (forall w. NatRepr w -> a -> a -> a)
    -- ^ binary operator on domain
    -> (forall w. NatRepr w -> BV.BV w -> BV.BV w -> BV.BV w)
    -- ^ binary operator on codomain
    -> Property
bin genW p gen1 gen2 aOp bOp = property $ do
  Some w <- forAll genW
  a1 <- forAll (gen1 w)
  a2 <- forAll (gen2 w)

  -- compute f (a1 `aOp` a2)
  let a1_a2  = aOp w a1 a2
  let pa1_a2 = p w a1_a2

  -- compute f (a1) `bOp` f (a2)
  let pa1     = p w a1
  let pa2     = p w a2
  let pa1_pa2 = bOp w pa1 pa2

  pa1_a2 === pa1_pa2

binPred :: Show a
        => Gen (Some NatRepr)
        -- ^ generator for width
        -> (forall w. NatRepr w -> a -> BV.BV w)
        -- ^ morphism on domains
        -> (forall w . NatRepr w -> Gen a)
        -- ^ generator for first arg
        -> (forall w . NatRepr w -> Gen a)
        -- ^ generator for second arg
        -> (forall w . NatRepr w -> a -> a -> Bool)
        -- ^ binary predicate on domain
        -> (forall w . NatRepr w -> BV.BV w -> BV.BV w -> Bool)
        -- ^ binary predicate on codomain
        -> Property
binPred genW p gen1 gen2 aPred bPred = property $ do
  Some w <- forAll genW
  a1 <- forAll (gen1 w)
  a2 <- forAll (gen2 w)

  let a1_a2  = aPred w a1 a2

  let pa1     = p w a1
  let pa2     = p w a2
  let pa1_pa2 = bPred w pa1 pa2

  a1_a2 === pa1_pa2

----------------------------------------
-- Ranges

anyWidth :: Gen (Some NatRepr)
anyWidth = mkNatRepr <$> (Gen.integral $ Range.linear 0 128)

byteWidth :: Gen (Some NatRepr)
byteWidth = mkNatRepr <$> (8*) <$> (Gen.integral $ Range.linear 0 16)

anyPosWidth :: Gen (Some NatRepr)
anyPosWidth = mkNatRepr <$> (Gen.integral $ Range.linear 1 128)

anyWidthGT1 :: Gen (Some NatRepr)
anyWidthGT1 = mkNatRepr <$> (Gen.integral $ Range.linear 2 128)

smallPosWidth :: Gen (Some NatRepr)
smallPosWidth = mkNatRepr <$> (Gen.integral $ Range.linear 1 4)

data NatReprLte w where
  NatReprLte :: (i <= w) => NatRepr i -> NatReprLte w

deriving instance Show (NatReprLte w)

natReprLte :: NatRepr w -> Gen (NatReprLte w)
natReprLte w = do
  n <- Gen.integral $ Range.linear 0 (natValue w)
  Some i <- return $ mkNatRepr n
  Just LeqProof <- return $ i `testLeq` w
  return $ NatReprLte i

data NatReprLt w where
  NatReprLt :: (i+1 <= w) => NatRepr i -> NatReprLt w

deriving instance Show (NatReprLt w)

natReprLt :: NatRepr w -> Gen (NatReprLt w)
natReprLt w = do
  n <- Gen.integral $ Range.linear 0 (natValue w - 1)
  Some i <- return $ mkNatRepr n
  NatCaseLT LeqProof <- return $ i `testNatCases` w
  return $ NatReprLt i

data NatReprPosLt w where
  NatReprPosLt :: (1 <= i, i+1 <= w) => NatRepr i -> NatReprPosLt w

deriving instance Show (NatReprPosLt w)

natReprPosLt :: NatRepr w -> Gen (NatReprPosLt w)
natReprPosLt w = do
  n <- Gen.integral $ Range.linear 1 (natValue w - 1)
  Some i <- return $ mkNatRepr n
  NatCaseLT LeqProof <- return $ i `testNatCases` w
  Right LeqProof <- return $ isZeroOrGT1 i
  return $ NatReprPosLt i

bytes :: Gen [Word8]
bytes = Gen.list (Range.linear 0 16) $ Gen.word8 Range.linearBounded

bits :: Gen [Bool]
bits = Gen.list (Range.linear 0 128) $ Gen.bool

unsigned :: NatRepr w -> Gen Integer
unsigned w = Gen.integral $ Range.linear 0 (maxUnsigned w)

unsignedPos :: NatRepr w -> Gen Integer
unsignedPos w = Gen.integral $ Range.linear 1 (maxUnsigned w)

largeUnsigned :: NatRepr w -> Gen Integer
largeUnsigned w = Gen.integral $ Range.linear 0 (maxUnsigned w')
  where w' = incNat w

signed :: NatRepr w -> Gen Integer
signed w = case isZeroOrGT1 w of
  Left Refl -> error "Main.signed: w = 0"
  Right LeqProof -> Gen.integral $ Range.linearFrom 0 (minSigned w) (maxSigned w)

signedPos :: NatRepr w -> Gen Integer
signedPos w = case isZeroOrGT1 w of
  Left Refl -> error "Main.posBounded: w = 0"
  Right LeqProof -> Gen.integral $ Range.linear 1 (maxSigned w)

signedNeg :: NatRepr w -> Gen Integer
signedNeg w = case isZeroOrGT1 w of
  Left Refl -> error "Main.posBounded: w = 0"
  Right LeqProof -> Gen.integral $ Range.linearFrom (-1) (minSigned w) (-1)

largeSigned :: NatRepr w -> Gen Integer
largeSigned w = Gen.integral $ Range.linearFrom 0 (- maxUnsigned w') (maxUnsigned w')
  where w' = incNat w

genPair :: Gen a -> Gen a -> Gen (a, a)
genPair gen gen' = do
  a <- gen
  a' <- gen'
  return (a, a')

----------------------------------------
-- Tests

bitwiseHomTests :: TestTree
bitwiseHomTests = testGroup "bitwise homomorphisms tests"
  [ testPropertyString "and" $ bin anyWidth BV.mkBV
    largeUnsigned largeUnsigned
    (const (Bits..&.)) (const BV.and)
  , testPropertyString "or" $ bin anyWidth BV.mkBV
    largeUnsigned largeUnsigned
    (const (Bits..|.)) (const BV.or)
  , testPropertyString "xor" $ bin anyWidth BV.mkBV
    largeUnsigned largeUnsigned
    (const Bits.xor) (const BV.xor)
  , testPropertyString "complement" $ un anyWidth BV.mkBV
    largeUnsigned
    (const Bits.complement) BV.complement
  ]

arithHomTests :: TestTree
arithHomTests = testGroup "arithmetic homomorphisms tests"
  [ testPropertyString "add" $ bin anyWidth BV.mkBV
    largeSigned largeSigned
    (const (+)) BV.add
  , testPropertyString "sub" $ bin anyWidth BV.mkBV
    largeSigned largeSigned
    (const (-)) BV.sub
  , testPropertyString "mul" $ bin anyWidth BV.mkBV
    largeSigned largeSigned
    (const (*)) BV.mul
  , testPropertyString "uquot" $ bin anyPosWidth BV.mkBV
    unsigned unsignedPos
    (const quot) (const BV.uquot)
  , testPropertyString "urem" $ bin anyPosWidth BV.mkBV
    unsigned unsignedPos
    (const rem) (const BV.urem)
  , testPropertyString "squot-pos-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedPos
    (const quot) (forcePos BV.squot)
  , testPropertyString "squot-neg-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedNeg
    (const quot) (forcePos BV.squot)
  , testPropertyString "srem-pos-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedPos
    (const rem) (forcePos BV.srem)
  , testPropertyString "srem-neg-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedNeg
    (const rem) (forcePos BV.srem)
  , testPropertyString "sdiv-pos-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedPos
    (const div) (forcePos BV.sdiv)
  , testPropertyString "sdiv-neg-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedNeg
    (const div) (forcePos BV.sdiv)
  , testPropertyString "smod-pos-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedPos
    (const mod) (forcePos BV.smod)
  , testPropertyString "smod-neg-denom" $ bin anyWidthGT1 BV.mkBV
    signed signedNeg
    (const mod) (forcePos BV.smod)
  , testPropertyString "abs" $ un anyPosWidth BV.mkBV
    signed
    (const abs) (forcePos BV.abs)
  , testPropertyString "negate" $ un anyPosWidth BV.mkBV
    largeSigned
    (const negate) BV.negate
  , testPropertyString "signBit" $ un anyPosWidth BV.mkBV
    signed
    (\_ a -> if a < 0 then 1 else 0) (forcePos BV.signBit)
  , testPropertyString "signum" $ un anyPosWidth BV.mkBV
    signed
    (\_ a -> signum a) (forcePos BV.signum)
  , testPropertyString "slt" $ binPred anyPosWidth BV.mkBV
    signed signed
    (const (<)) (forcePos BV.slt)
  , testPropertyString "sle" $ binPred anyPosWidth BV.mkBV
    signed signed
    (const (<=)) (forcePos BV.sle)
  , testPropertyString "ult" $ binPred anyWidth BV.mkBV
    unsigned unsigned
    (const (<)) (const BV.ult)
  , testPropertyString "ule" $ binPred anyWidth BV.mkBV
    unsigned unsigned
    (const (<=)) (const BV.ule)
  , testPropertyString "umin" $ bin anyWidth BV.mkBV
    unsigned unsigned
    (const min) (const BV.umin)
  , testPropertyString "umax" $ bin anyWidth BV.mkBV
    unsigned unsigned
    (const max) (const BV.umax)
  , testPropertyString "smin" $ bin anyPosWidth BV.mkBV
    signed signed
    (const min) (forcePos BV.smin)
  , testPropertyString "smax" $ bin anyPosWidth BV.mkBV
    signed signed
    (const max) (forcePos BV.smax)
  ]

serdeTest :: Gen (Some NatRepr)
          -> (forall w . NatRepr w -> BV.BV w -> Maybe a)
          -> (a -> Pair NatRepr BV.BV)
          -> Property
serdeTest wGen ser de = property $ do
  Some w <- forAll wGen
  i <- forAll (largeUnsigned w)
  let bv = BV.mkBV w i

  let a = ser w bv
  assert (isJust a)
  Pair w' bv' <- return $ de $ fromJust a

  assert (isJust (w' `testEquality` w))
  Just Refl <- return $ w' `testEquality` w
  bv' === bv

deserTest :: (Show a, Eq a)
          => Gen a
          -> (a -> Int)
          -> (a -> Pair NatRepr BV.BV)
          -> (forall w . NatRepr w -> BV.BV w -> Maybe a)
          -> Property
deserTest genA lenA de ser = property $ do
  a <- forAll genA
  Some w' <- return $ mkNatRepr (fromIntegral (lenA a))

  Pair w bv <- return $ de $ a

  assert (isJust (w' `testEquality` w))
  Just Refl <- return $ w' `testEquality` w

  ser w bv === Just a

serdeTests :: TestTree
serdeTests = testGroup "serialization/deseriallization tests"
  [ testPropertyString "bitsBE" $
    serdeTest anyWidth (\w bv -> Just (BV.asBitsBE w bv)) BV.bitsBE
  , testPropertyString "bitsLE" $
    serdeTest anyWidth (\w bv -> Just (BV.asBitsLE w bv)) BV.bitsLE
  , testPropertyString "bytesBE" $
    serdeTest byteWidth BV.asBytesBE BV.bytesBE
  , testPropertyString "bytesLE" $
    serdeTest byteWidth BV.asBytesLE BV.bytesLE
  , testPropertyString "bytestringBE" $
    serdeTest byteWidth BV.asBytestringBE BV.bytestringBE
  , testPropertyString "bytestringLE" $
    serdeTest byteWidth BV.asBytestringLE BV.bytestringLE
  ]

deserTests :: TestTree
deserTests = testGroup "deserialization/serialization tests"
  [ testPropertyString "asBitsBE" $
    deserTest bits length BV.bitsBE (\w bv -> Just (BV.asBitsBE w bv))
  , testPropertyString "asBitsLE" $
    deserTest bits length BV.bitsLE (\w bv -> Just (BV.asBitsLE w bv))
  , testPropertyString "asBytesBE" $
    deserTest bytes ((*8) . length) BV.bytesBE BV.asBytesBE
  , testPropertyString "asBytesLE" $
    deserTest bytes ((*8) . length) BV.bytesLE BV.asBytesLE
  , testPropertyString "asBytesBE" $
    deserTest (BS.pack <$> bytes) ((*8) . BS.length) BV.bytestringBE BV.asBytestringBE
  , testPropertyString "asBytesLE" $
    deserTest (BS.pack <$> bytes) ((*8) . BS.length) BV.bytestringLE BV.asBytestringLE
  ]

checkBounds :: MonadTest m => Integer -> NatRepr w -> m ()
checkBounds x w = do
  diff 0 (<=) x
  diff x (<=) (2 ^ natValue w)

wfCtor :: Gen (Some NatRepr)
       -- ^ generator for width
       -> (forall w . NatRepr w -> Integer -> Maybe (BV.BV w))
       -- ^ constructor
       -> Property
wfCtor genW ctor = property $ do
  Some w <- forAll genW
  x <- forAll (largeSigned w)

  case ctor w x of
    Just (BV.BV x') -> checkBounds x' w
    Nothing -> return ()

wfCtor' :: NatRepr w
        -- ^ fixed width of constructor
        -> (Integer -> BV.BV w)
        -- ^ embedding of integer into constructor arg
        -> Property
wfCtor' w ctor = property $ do
  x <- forAll (largeSigned w)

  let BV.BV x' = ctor x

  checkBounds x' w

wfUnary :: Gen (Some NatRepr)
        -- ^ generator for width
        -> (forall w . NatRepr w -> BV.BV w -> BV.BV w)
        -- ^ unary operator
        -> Property
wfUnary genW op = property $ do
  Some w <- forAll genW
  bv <- BV.mkBV w <$> forAll (unsigned w)

  let BV.BV x' = op w bv
  checkBounds x' w

wfUnaryMaybe :: Gen (Some NatRepr)
             -- ^ generator for width
             -> (forall w . NatRepr w -> BV.BV w -> Maybe (BV.BV w))
             -- ^ unary operator
             -> Property
wfUnaryMaybe genW op = property $ do
  Some w <- forAll genW
  bv <- BV.mkBV w <$> forAll (unsigned w)

  case op w bv of
    Just (BV.BV x') -> checkBounds x' w
    Nothing -> return ()

wfBinary :: Gen (Some NatRepr)
         -- ^ generator for width
         -> (forall w . NatRepr w -> BV.BV w -> BV.BV w -> BV.BV w)
         -- ^ binary operator
         -> Property
wfBinary genW op = property $ do
  Some w <- forAll genW
  bv1 <- BV.mkBV w <$> forAll (unsigned w)
  bv2 <- BV.mkBV w <$> forAll (unsigned w)

  let BV.BV x' = op w bv1 bv2
  checkBounds x' w

wfBinaryDiv :: Gen (Some NatRepr)
            -- ^ generator for width
            -> (forall w . NatRepr w -> BV.BV w -> BV.BV w -> BV.BV w)
            -- ^ binary division-like operator
            -> Property
wfBinaryDiv genW op = property $ do
  Some w <- forAll genW
  bv1 <- BV.mkBV w <$> forAll (unsigned w)
  bv2 <- BV.mkBV w <$> forAll (unsignedPos w)

  let BV.BV x' = op w bv1 bv2
  checkBounds x' w

wfBinaryN :: Gen (Some NatRepr)
          -- ^ generator for width
          -> (forall w . NatRepr w -> BV.BV w -> Natural -> BV.BV w)
          -- ^ binary operator with Natural arg
          -> Property
wfBinaryN genW op = property $ do
  Some w <- forAll genW
  bv <- BV.mkBV w <$> forAll (unsigned w)
  n <- fromInteger <$> forAll (largeUnsigned w)

  let BV.BV x' = op w bv n
  checkBounds x' w

wfBit :: Gen (Some NatRepr)
      -- ^ generator for width
      -> (forall w ix . ix+1 <= w => NatRepr w -> NatRepr ix -> BV.BV w -> BV.BV w)
      -- ^ bit twiddling function
      -> Property
wfBit genW f = property $ do
  Some w <- forAll genW
  bv <- BV.mkBV w <$> forAll (unsigned w)
  NatReprLt ix <- forAll (natReprLt w)

  let BV.BV x = f w ix bv
  checkBounds x w

wfBitN :: Gen (Some NatRepr)
       -- ^ generator for width
       -> (forall w . NatRepr w -> Natural -> BV.BV w -> BV.BV w)
       -- ^ bit twiddling function
       -> Property
wfBitN genW f = property $ do
  Some w <- forAll genW
  bv <- BV.mkBV w <$> forAll (unsigned w)
  n <- fromInteger <$> forAll (largeUnsigned w)

  let BV.BV x = f w n bv
  checkBounds x w

wellFormedTests :: TestTree
wellFormedTests = testGroup "well-formedness tests"
  [ testPropertyString "mkBV" $ wfCtor anyWidth (fmap Just . BV.mkBV)
  , testPropertyString "mkBVUnsigned" $ wfCtor anyWidth BV.mkBVUnsigned
  , testPropertyString "mkBVSigned" $ wfCtor anyPosWidth (forcePos BV.mkBVSigned)
  , testPropertyString "signedClamp" $ wfCtor anyPosWidth (fmap Just . forcePos BV.signedClamp)
  , testPropertyString "minUnsigned" $ wfCtor anyWidth (\w _ -> Just (BV.minUnsigned w))
  , testPropertyString "maxUnsigned" $ wfCtor anyWidth (\w _ -> Just (BV.maxUnsigned w))
  , testPropertyString "minSigned" $ wfCtor anyPosWidth (\w _ -> Just (forcePos BV.minSigned w))
  , testPropertyString "maxSigned" $ wfCtor anyPosWidth (\w _ -> Just (forcePos BV.maxSigned w))
  , testPropertyString "bool" $ wfCtor' knownNat (BV.bool . odd)
  , testPropertyString "word8" $ wfCtor' knownNat (BV.word8 . fromInteger)
  , testPropertyString "word16" $ wfCtor' knownNat (BV.word16 . fromInteger)
  , testPropertyString "word32" $ wfCtor' knownNat (BV.word32 . fromInteger)
  , testPropertyString "word64" $ wfCtor' knownNat (BV.word64 . fromInteger)
  , testPropertyString "int8" $ wfCtor' knownNat (BV.int8 . fromInteger)
  , testPropertyString "int16" $ wfCtor' knownNat (BV.int16 . fromInteger)
  , testPropertyString "int32" $ wfCtor' knownNat (BV.int32 . fromInteger)
  , testPropertyString "int64" $ wfCtor' knownNat (BV.int64 . fromInteger)
  , testPropertyString "and" $ wfBinary anyWidth (const BV.and)
  , testPropertyString "or" $ wfBinary anyWidth (const BV.or)
  , testPropertyString "xor" $ wfBinary anyWidth (const BV.xor)
  , testPropertyString "complement" $ wfUnary anyWidth BV.complement
  , testPropertyString "shl" $ wfBinaryN anyWidth BV.shl
  , testPropertyString "ashr" $ wfBinaryN anyPosWidth (forcePos BV.ashr)
  , testPropertyString "lshr" $ wfBinaryN anyWidth BV.lshr
  , testPropertyString "rotateL" $ wfBinaryN anyWidth BV.rotateL
  , testPropertyString "rotateR" $ wfBinaryN anyWidth BV.rotateR
  , testPropertyString "bit" $ property $ do
      Some w <- forAll anyPosWidth
      NatReprLt i <- forAll (natReprLt w)

      let BV.BV x = BV.bit w i
      checkBounds x w
  , testPropertyString "bit'" $ property $ do
      Some w <- forAll anyPosWidth
      n <- forAll $ Gen.integral $ Range.linear 0 (2 * natValue w)

      let BV.BV x = BV.bit' w n
      checkBounds x w
  , testPropertyString "setBit" $ wfBit anyPosWidth (const BV.setBit)
  , testPropertyString "setBit'" $ wfBitN anyPosWidth BV.setBit'
  , testPropertyString "clearBit" $ wfBit anyPosWidth BV.clearBit
  , testPropertyString "clearBit'" $ wfBitN anyPosWidth BV.clearBit'
  , testPropertyString "complementBit" $ wfBit anyPosWidth (const BV.complementBit)
  , testPropertyString "complementBit'" $ wfBitN anyPosWidth BV.complementBit'
  , testPropertyString "popCount" $ wfUnary anyWidth (const BV.popCount)
  , testPropertyString "ctz" $ wfUnary anyWidth BV.ctz
  , testPropertyString "clz" $ wfUnary anyWidth BV.clz
  , testPropertyString "truncBits" $ property $ do
      Some w <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)
      n <- forAll $ Gen.integral $ Range.linear 0 (2 * natValue w)

      let BV.BV x = BV.truncBits n bv
      checkBounds x w
  , testPropertyString "add" $ wfBinary anyWidth BV.add
  , testPropertyString "sub" $ wfBinary anyWidth BV.sub
  , testPropertyString "mul" $ wfBinary anyWidth BV.mul
  , testPropertyString "uquot" $ wfBinaryDiv anyPosWidth (const BV.uquot)
  , testPropertyString "urem" $ wfBinaryDiv anyPosWidth (const BV.urem)
  , testPropertyString "squot" $ wfBinaryDiv anyPosWidth (forcePos BV.squot)
  , testPropertyString "srem" $ wfBinaryDiv anyPosWidth (forcePos BV.srem)
  , testPropertyString "sdiv" $ wfBinaryDiv anyPosWidth (forcePos BV.sdiv)
  , testPropertyString "smod" $ wfBinaryDiv anyPosWidth (forcePos BV.smod)
  , testPropertyString "abs" $ wfUnary anyPosWidth (forcePos BV.abs)
  , testPropertyString "negate" $ wfUnary anyWidth BV.negate
  , testPropertyString "signBit" $ wfUnary anyPosWidth (forcePos BV.signBit)
  , testPropertyString "signum" $ wfUnary anyPosWidth (forcePos BV.signum)
  , testPropertyString "umin" $ wfBinary anyWidth (const BV.umin)
  , testPropertyString "umax" $ wfBinary anyWidth (const BV.umax)
  , testPropertyString "smin" $ wfBinary anyPosWidth (forcePos BV.smin)
  , testPropertyString "smax" $ wfBinary anyPosWidth (forcePos BV.smax)
  , testPropertyString "concat" $ property $ do
      Some w <- forAll anyWidth
      Some w' <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)
      bv' <- BV.mkBV w' <$> forAll (unsigned w')

      let BV.BV x = BV.concat w w' bv bv'
      checkBounds x (w `addNat` w')
  , testPropertyString "select" $ property $ do
      Some w <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)
      NatReprLte ix <- forAll (natReprLte w)
      Just LeqProof <- return $ ix `testLeq` w
      NatReprLte w' <- forAll (natReprLte (w `subNat` ix))
      Just LeqProof <- return $ (ix `addNat` w') `testLeq` w

      let BV.BV x = BV.select ix w' bv
      checkBounds x w'
  , testPropertyString "select'" $ property $ do
      Some w <- forAll anyWidth
      Some w' <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)
      n <- forAll $ Gen.integral $ Range.linear 0 (2 * natValue w)

      let BV.BV x = BV.select' n w' bv
      checkBounds x w'
  , testPropertyString "zext" $ property $ do
      Some w' <- forAll anyPosWidth
      NatReprLt w <- forAll (natReprLt w')
      bv <- BV.mkBV w <$> forAll (unsigned w)

      let BV.BV x = BV.zext w' bv
      checkBounds x w'
  , testPropertyString "sext" $ property $ do
      Some w' <- forAll anyWidthGT1
      NatReprPosLt w <- forAll (natReprPosLt w')
      bv <- BV.mkBV w <$> forAll (unsigned w)

      let BV.BV x = BV.sext w w' bv
      checkBounds x w'
  , testPropertyString "trunc" $ property $ do
      Some w <- forAll anyPosWidth
      NatReprLt w' <- forAll (natReprLt w)
      bv <- BV.mkBV w <$> forAll (unsigned w)

      let BV.BV x = BV.trunc w' bv
      checkBounds x w'
  , testPropertyString "trunc'" $ property $ do
      Some w <- forAll anyWidth
      Some w' <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)

      let BV.BV x = BV.trunc' w' bv
      checkBounds x w'
  , testPropertyString "zresize" $ property $ do
      Some w <- forAll anyWidth
      Some w' <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)

      let BV.BV x = BV.zresize w' bv
      checkBounds x w'
  , testPropertyString "sresize" $ property $ do
      Some w <- forAll anyPosWidth
      Just LeqProof <- return $ isPosNat w
      Some w' <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)

      let BV.BV x = BV.sresize w w' bv
      checkBounds x w'
  , testPropertyString "mulWide" $ property $ do
      Some w <- forAll anyWidth
      Some w' <- forAll anyWidth
      bv <- BV.mkBV w <$> forAll (unsigned w)
      bv' <- BV.mkBV w' <$> forAll (unsigned w')

      let BV.BV x = BV.mulWide w w' bv bv'
      checkBounds x (w `addNat` w')
    , testPropertyString "succUnsigned" $ wfUnaryMaybe anyWidth BV.succUnsigned
    , testPropertyString "succSigned" $ wfUnaryMaybe anyPosWidth (forcePos BV.succUnsigned)
    , testPropertyString "predUnsigned" $ wfUnaryMaybe anyWidth BV.predUnsigned
    , testPropertyString "predSigned" $ wfUnaryMaybe anyPosWidth (forcePos BV.predUnsigned)
    ]

testRandomR :: (Ord (f w), Random (f w), Show (f w), Show a)
            => NatRepr w
            -> (forall w' . NatRepr w' -> a -> f w')
            -> (NatRepr w -> Gen a)
            -> Property
testRandomR w mk gen = property $ do
  x <- mk w <$> forAll (gen w)
  y <- mk w <$> forAll (gen w)

  let l = min x y
      h = max x y

  rand  <- liftIO $ getRandomR (l, h)
  rand' <- liftIO $ getRandomR (h, l)

  diff l    (<=) rand
  diff rand (<=) h

  diff l     (<=) rand'
  diff rand' (<=) h

randomTests :: TestTree
randomTests = testGroup "tests for random generation"
  [ testPropertyString "random unsigned well-formed" $ property $ do
      BV.UnsignedBV (BV.BV x) :: BV.UnsignedBV 32 <- liftIO $ getRandom
      checkBounds x (knownNat @32)
  , testPropertyString "random signed well-formed" $ property $ do
      BV.SignedBV (BV.BV x) :: BV.SignedBV 32 <- liftIO $ getRandom
      checkBounds x (knownNat @32)
  , testPropertyString "randomR unsigned well-formed and in bounds" $
    testRandomR (knownNat @32) BV.mkUnsignedBV unsigned
  , testPropertyString "randomR signed well-formed and in bounds" $
    testRandomR (knownNat @32) BV.mkSignedBV unsigned
  ]

tests :: TestTree
tests = testGroup "bv-sized tests"
  [ arithHomTests
  , bitwiseHomTests
  , serdeTests
  , deserTests
  , wellFormedTests
  , randomTests
  ]

main :: IO ()
main = defaultMain tests
