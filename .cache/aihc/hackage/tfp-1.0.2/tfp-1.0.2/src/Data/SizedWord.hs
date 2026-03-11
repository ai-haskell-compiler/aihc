{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SizedWord
    ( SizedWord ) where

import Data.Bits
          (Bits, shiftL, shiftR, rotateL, rotateR, bit, testBit, popCount,
           complement, xor, bitSize, isSigned, (.&.), (.|.), )

import qualified Type.Data.Num as Num
import Type.Base.Proxy (Proxy(Proxy))


newtype SizedWord nT = SizedWord Integer

sizeT :: SizedWord nT -> Proxy nT
sizeT _ = Proxy

mask :: forall nT . Num.Natural nT => Proxy nT -> Integer
mask n = bit (Num.fromInteger n) - 1

instance Num.Natural nT => Eq (SizedWord nT) where
    (SizedWord x) == (SizedWord y) = x == y
    (SizedWord x) /= (SizedWord y) = x /= y

instance Num.Natural nT => Show (SizedWord nT) where
    showsPrec prec n =
        showsPrec prec $ toInteger n

instance Num.Natural nT => Read (SizedWord nT) where
    readsPrec prec str0 =
        [ (fromInteger n, str)
        | (n, str) <- readsPrec prec str0 ]

instance Num.Natural nT => Ord (SizedWord nT) where
    a `compare` b = toInteger a `compare` toInteger b

instance Num.Natural nT => Bounded (SizedWord nT) where
    minBound = 0
    maxBound = SizedWord $ (1 `shiftL` (Num.fromInteger (Proxy :: Proxy nT))) - 1

instance Num.Natural nT => Enum (SizedWord nT) where
    succ x
       | x == maxBound  = error $ "Enum.succ{" ++ showSizedWordType x ++ "}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = error $ "Enum.succ{" ++ showSizedWordType x ++ "}: tried to take `pred' of minBound"
       | otherwise      = x - 1
    
    fromEnum s@(SizedWord x)
        | x > toInteger (maxBound :: Int) =
            error $ "Enum.fromEnum{" ++ showSizedWordType s ++ "}: tried to take `fromEnum' on SizedWord greater than maxBound :: Int"
        | x < toInteger (minBound :: Int) =
            error $ "Enum.fromEnum{" ++ showSizedWordType s ++ "}: tried to take `fromEnum' on SizedWord smaller than minBound :: Int"
        | otherwise =
            fromInteger x
    toEnum x
        | x > fromIntegral (maxBound :: SizedWord nT) =
            error $ "Enum.fromEnum{" ++ showSizedWordTypeProxy n ++ "}: tried to take `fromEnum' on SizedWord greater than maxBound :: " ++ showSizedWordTypeProxy n
        | x < fromIntegral (minBound :: SizedWord nT) =
            error $ "Enum.fromEnum{" ++ showSizedWordTypeProxy n ++ "}: tried to take `fromEnum' on SizedWord smaller than minBound :: " ++ showSizedWordTypeProxy n
        | otherwise =
            fromInteger $ toInteger x
            where n = Proxy :: Proxy nT

instance Num.Natural nT => Num (SizedWord nT) where
    (SizedWord a) + (SizedWord b) =
        fromInteger $ a + b
    (SizedWord a) * (SizedWord b) =
        fromInteger $ a * b
    negate s@(SizedWord n) =
        fromInteger $ (n `xor` mask (sizeT s)) + 1
    a - b =
        a + (negate b)

    fromInteger n =
      let fromCardinal m = SizedWord $ m .&. mask (Proxy :: Proxy nT)
      in  if n>=0
            then fromCardinal n
            else negate $ fromCardinal $ negate n

    abs s = s
    signum s
      | s == 0 =
          0
      | otherwise =
          1

instance Num.Natural nT => Real (SizedWord nT) where
    toRational n = toRational $ toInteger n

instance Num.Natural nT => Integral (SizedWord nT) where
    a `quot` b =
        fromInteger $ toInteger a `quot` toInteger b
    a `rem` b =
        fromInteger $ toInteger a `rem` toInteger b
    a `div` b =
        fromInteger $ toInteger a `div` toInteger b
    a `mod` b =
        fromInteger $ toInteger a `mod` toInteger b
    a `quotRem` b =
        let (quot_, rem_) = toInteger a `quotRem` toInteger b
        in (fromInteger quot_, fromInteger rem_)
    a `divMod` b =
        let (div_, mod_) = toInteger a `divMod` toInteger b
        in (fromInteger div_, fromInteger mod_)
    toInteger (SizedWord x) = x

instance Num.Natural nT => Bits (SizedWord nT) where
    (SizedWord a) .&. (SizedWord b) = SizedWord $ a .&. b
    (SizedWord a) .|. (SizedWord b) = SizedWord $ a .|. b
    (SizedWord a) `xor` SizedWord b = SizedWord $ a `xor` b
    complement (SizedWord x) = SizedWord $ x `xor` mask (Proxy :: Proxy nT)
    bit b =
      case SizedWord $ bit b of
        s | b < 0 -> error $ "Bits.bit{" ++ showSizedWordType s ++ "}: tried to set negative position"
          | b >= bitSize s -> error $ "Bits.bit{" ++ showSizedWordType s ++ "}: tried to set too large position"
          | otherwise -> s
    s@(SizedWord x) `testBit` b
      | b < 0 = error $ "Bits.testBit{" ++ showSizedWordType s ++ "}: tried to test negative position"
      | b >= bitSize s = error $ "Bits.testBit{" ++ showSizedWordType s ++ "}: tried to test too large position"
      | otherwise =
         testBit x b
    s@(SizedWord x) `shiftL` b
      | b < 0 = error $ "Bits.shiftL{" ++ showSizedWordType s ++ "}: tried to shift by negative amount"
      | otherwise =
        SizedWord $ mask (Proxy :: Proxy nT) .&. (x `shiftL` b)
    s@(SizedWord x) `shiftR` b
      | b < 0 = error $ "Bits.shiftR{" ++ showSizedWordType s ++ "}: tried to shift by negative amount"
      | otherwise =
        SizedWord $ (x `shiftR` b)
    s@(SizedWord x) `rotateL` b
      | b < 0 =
        error $ "Bits.rotateL{" ++ showSizedWordType s ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedWord $ mask (Proxy :: Proxy nT) .&.
            ((x `shiftL` b) .|. (x `shiftR` (bitSize s - b)))
    s@(SizedWord x) `rotateR` b
      | b < 0 =
        error $ "Bits.rotateR{" ++ showSizedWordType s ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedWord $ mask (Proxy :: Proxy nT) .&.
            ((x `shiftR` b) .|. (x `shiftL` (bitSize s - b)))
    popCount (SizedWord x) = popCount x
    bitSize _ = Num.fromInteger (Proxy :: Proxy nT)
    isSigned _ = False

showSizedWordTypeProxy :: forall nT. Num.Natural nT => Proxy nT -> String
showSizedWordTypeProxy n =
   "SizedWord " ++ show (Num.fromInteger n :: Integer)

showSizedWordType :: forall nT. Num.Natural nT => SizedWord nT -> String
showSizedWordType _ = showSizedWordTypeProxy (Proxy :: Proxy nT)
