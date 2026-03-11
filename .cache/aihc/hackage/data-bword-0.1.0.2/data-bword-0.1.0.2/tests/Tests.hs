{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.Tasty (TestName, TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck hiding ((.&.))

import Data.Bits (Bits(..))
#if MIN_VERSION_base(4,7,0)
import Data.Bits (FiniteBits(..))
#endif
import Data.Int
import Data.Word
import Data.BinaryWord

main ∷ IO ()
main = defaultMain
     $ localOption (QuickCheckTests 10000)
     $ testGroup "Tests"
         [ binWordGroup "Word8" (0 ∷ Word8)
         , binWordGroup "Word16" (0 ∷ Word16)
         , binWordGroup "Word32" (0 ∷ Word32)
         , binWordGroup "Word64" (0 ∷ Word64)
         , binWordGroup "Int8" (0 ∷ Int8)
         , binWordGroup "Int16" (0 ∷ Int16)
         , binWordGroup "Int32" (0 ∷ Int32)
         , binWordGroup "Int64" (0 ∷ Int64)
         ]

#if !MIN_VERSION_base(4,7,0)
finiteBitSize ∷ Bits α ⇒ α → Int
finiteBitSize = bitSize
#endif

sameType ∷ α → α → α
sameType = const id

binWordGroup ∷ (Show w, Arbitrary w, Integral w, Integral (UnsignedWord w),
                BinaryWord w) ⇒ TestName → w → TestTree
binWordGroup n w = testGroup n
  [ testProperty "allZeroes" $ popCount (sameType w allZeroes) == 0
  , testProperty "allOnes" $ popCount (sameType w allOnes) == finiteBitSize w
  , testProperty "msb" $
      let bs = finiteBitSize w in
        testBit (sameType w msb) (bs - 1) &&
        all (not . testBit (sameType w msb)) [0 .. bs - 2]
  , testProperty "lsb" $
      let bs = finiteBitSize w in
        testBit (sameType w lsb) 0 &&
        all (not . testBit (sameType w lsb)) [1 .. bs - 1]
  , testProperty "testMsb" $ \x →
      testMsb (sameType w x) == testBit x (finiteBitSize x - 1)
  , testProperty "testLsb" $ \x →
      testLsb (sameType w x) == testBit x 0
  , testProperty "leadingZeroes" $ \x →
      let lz = leadingZeroes (sameType w x)
          bs = finiteBitSize x in
        if lz == 0
        then testBit x (bs - 1)
        else if lz == bs
             then x == 0
             else shiftR x (bs - lz) == 0 && testBit x (bs - lz - 1)
  , testProperty "trailingZeroes" $ \x →
      let tz = trailingZeroes (sameType w x)
          bs = finiteBitSize x in
        if tz == 0
        then testBit x 0
        else if tz == bs
             then x == 0
             else shiftL x (bs - tz) == 0 && testBit x tz
  , testProperty "unwrappedAdd" $ \w₁ w₂ →
      let (h, l) = unwrappedAdd (sameType w w₁) w₂ in
        toInteger h * 2 ^ finiteBitSize w + toInteger l ==
          toInteger w₁ + toInteger w₂
  , testProperty "unwrappedMul" $ \w₁ w₂ →
      let (h, l) = unwrappedMul (sameType w w₁) w₂ in
        toInteger h * 2 ^ finiteBitSize w + toInteger l ==
          toInteger w₁ * toInteger w₂
  ]
