{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SizedInt
    ( SizedInt ) where

import Data.Bits
          (Bits, shiftL, shiftR, rotateL, rotateR, bit, testBit, popCount,
           complement, xor, bitSize, isSigned, (.&.), (.|.), )

import qualified Type.Data.Num as Num
import Type.Base.Proxy (Proxy(Proxy))


newtype SizedInt nT = SizedInt Integer

_sizeT :: SizedInt nT -> Proxy nT
_sizeT _ = Proxy

mask :: forall nT . Num.Natural nT => Proxy nT -> Integer
mask n = bit (Num.fromInteger n) - 1

signBit :: forall nT . Num.Natural nT => Proxy nT -> Int
signBit n = Num.fromInteger n - 1

isNegative :: forall nT . Num.Natural nT
           => SizedInt nT
           -> Bool
isNegative (SizedInt x) =
    testBit x $ signBit (Proxy :: Proxy nT)

instance Num.Natural nT => Eq (SizedInt nT) where
    (SizedInt x) == (SizedInt y) = x == y
    (SizedInt x) /= (SizedInt y) = x /= y

instance Num.Natural nT => Show (SizedInt nT) where
    showsPrec prec n =
        showsPrec prec $ toInteger n

instance Num.Natural nT => Read (SizedInt nT) where
    readsPrec prec str0 =
        [ (fromInteger n, str)
        | (n, str) <- readsPrec prec str0 ]

instance Num.Natural nT => Ord (SizedInt nT) where
    a `compare` b = toInteger a `compare` toInteger b

instance Num.Natural nT => Bounded (SizedInt nT) where
    minBound = SizedInt $ negate $ 1 `shiftL` (Num.fromInteger (Proxy :: Proxy nT) - 1)
    maxBound = SizedInt $ (1 `shiftL` (Num.fromInteger (Proxy :: Proxy nT) - 1)) - 1

instance Num.Natural nT => Enum (SizedInt nT) where
    succ x
       | x == maxBound  = error $ "Enum.succ{" ++ showSizedIntType x ++ "}: tried to take `succ' of maxBound"
       | otherwise      = x + 1
    pred x
       | x == minBound  = error $ "Enum.succ{" ++ showSizedIntType x ++ "}: tried to take `pred' of minBound"
       | otherwise      = x - 1
    
    fromEnum s@(SizedInt x)
        | x > toInteger (maxBound :: Int) =
            error $ "Enum.fromEnum{" ++ showSizedIntType s ++ "}: tried to take `fromEnum' on SizedInt greater than maxBound :: Int"
        | x < toInteger (minBound :: Int) =
            error $ "Enum.fromEnum{" ++ showSizedIntType s ++ "}: tried to take `fromEnum' on SizedInt smaller than minBound :: Int"
        | otherwise =
            fromInteger x
    toEnum x
        | x' > toInteger (maxBound :: SizedInt nT) =
            error $ "Enum.fromEnum{" ++ showSizedIntTypeProxy n ++ "}: tried to take `fromEnum' on SizedInt greater than maxBound :: " ++ showSizedIntTypeProxy n
        | x' < toInteger (minBound :: SizedInt nT) =
            error $ "Enum.fromEnum{" ++ showSizedIntTypeProxy n ++ "}: tried to take `fromEnum' on SizedInt smaller than minBound :: " ++ showSizedIntTypeProxy n
        | otherwise =
            fromInteger x'
            where x' = toInteger x
                  n = Proxy :: Proxy nT

instance Num.Natural nT => Num (SizedInt nT) where
    (SizedInt a) + (SizedInt b) =
        fromInteger $ a + b
    (SizedInt a) * (SizedInt b) =
        fromInteger $ a * b
    negate (SizedInt n) =
        fromInteger $ (n `xor` mask (Proxy :: Proxy nT)) + 1
    a - b =
        a + (negate b)

    fromInteger n =
      let fromCardinal m = SizedInt $ m .&. mask (Proxy :: Proxy nT)
      in  if n>=0
            then fromCardinal n
            else negate $ fromCardinal $ negate n

    abs s
      | isNegative s =
          negate s
      | otherwise =
          s
    signum s
      | isNegative s =
          -1
      | s == 0 =
          0
      | otherwise =
          1

instance Num.Natural nT => Real (SizedInt nT) where
    toRational n = toRational $ toInteger n

instance Num.Natural nT => Integral (SizedInt nT) where
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
    toInteger s@(SizedInt x) =
        if isNegative s
           then let SizedInt x' = negate s in negate x'
           else x

instance Num.Natural nT => Bits (SizedInt nT) where
    (SizedInt a) .&. (SizedInt b) = SizedInt $ a .&. b
    (SizedInt a) .|. (SizedInt b) = SizedInt $ a .|. b
    (SizedInt a) `xor` SizedInt b = SizedInt $ a `xor` b
    complement (SizedInt x) = SizedInt $ x `xor` mask (Proxy :: Proxy nT)
    bit b =
      case SizedInt $ bit b of
        s | b < 0 -> error $ "Bits.bit{" ++ showSizedIntType s ++ "}: tried to set negative position"
          | b >= bitSize s -> error $ "Bits.bit{" ++ showSizedIntType s ++ "}: tried to set too large position"
          | otherwise -> s
    s@(SizedInt x) `testBit` b
      | b < 0 = error $ "Bits.testBit{" ++ showSizedIntType s ++ "}: tried to test negative position"
      | b >= bitSize s = error $ "Bits.testBit{" ++ showSizedIntType s ++ "}: tried to test too large position"
      | otherwise =
         testBit x b
    s@(SizedInt x) `shiftL` b
      | b < 0 = error $ "Bits.shiftL{" ++ showSizedIntType s ++ "}: tried to shift by negative amount"
      | otherwise =
        SizedInt $ mask (Proxy :: Proxy nT) .&. (x `shiftL` b)
    s@(SizedInt x) `shiftR` b
      | b < 0 = error $ "Bits.shiftR{" ++ showSizedIntType s ++ "}: tried to shift by negative amount"
      | isNegative s =
        SizedInt $ mask (Proxy :: Proxy nT) .&.
            ((x `shiftR` b) .|. (mask (Proxy :: Proxy nT) `shiftL` (Num.fromInteger (Proxy :: Proxy nT) - b)))
      | otherwise =
        SizedInt $ (mask (Proxy :: Proxy nT)) .&. (x `shiftR` b)
    s@(SizedInt a) `rotateL` b
      | b < 0 =
        error $ "Bits.rotateL{" ++ showSizedIntType s ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedInt $ mask (Proxy :: Proxy nT) .&.
            ((a `shiftL` b) .|. (a `shiftR` (Num.fromInteger (Proxy :: Proxy nT) - b)))
    s@(SizedInt a) `rotateR` b
      | b < 0 =
        error $ "Bits.rotateR{" ++ showSizedIntType s ++ "}: tried to rotate by negative amount"
      | otherwise =
        SizedInt $ mask (Proxy :: Proxy nT) .&.
            ((a `shiftR` b) .|. (a `shiftL` (Num.fromInteger (Proxy :: Proxy nT) - b)))
    popCount (SizedInt x) = popCount x
    bitSize _ = Num.fromInteger (Proxy :: Proxy nT)
    isSigned _ = True


showSizedIntTypeProxy :: forall nT. Num.Natural nT => Proxy nT -> String
showSizedIntTypeProxy n =
   "SizedInt " ++ show (Num.fromInteger n :: Integer)

showSizedIntType :: forall nT. Num.Natural nT => SizedInt nT -> String
showSizedIntType _ = showSizedIntTypeProxy (Proxy :: Proxy nT)
