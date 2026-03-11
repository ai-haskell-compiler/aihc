module Sound.MIDI.Utility where

import qualified Test.QuickCheck as QC
import System.Random (Random(randomR), RandomGen)
import Data.Tuple.HT (mapFst, )
import Data.Word (Word8, )


{-# INLINE checkRange #-}
checkRange :: (Bounded a, Ord a, Show a) =>
   String -> (Int -> a) -> Int -> a
checkRange typ f x =
   let y = f x
   in  if minBound <= y && y <= maxBound
         then y
         else error (typ ++ ": value " ++ show x ++ " outside range " ++
                     show ((minBound, maxBound) `asTypeOf` (y,y)))

{-# INLINE loopM #-}
loopM :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m a
loopM p preExit postExit =
   let go =
         preExit >>= \x ->
            if p x
              then return x
              else postExit x >> go
   in  go




-- random generators

enumRandomR :: (Enum a, RandomGen g) => (a,a) -> g -> (a,g)
enumRandomR (l,r) =
   mapFst toEnum . randomR (fromEnum l, fromEnum r)

boundedEnumRandom :: (Enum a, Bounded a, RandomGen g) => g -> (a,g)
boundedEnumRandom  =  enumRandomR (minBound, maxBound)

chooseEnum :: (Enum a, Bounded a, Random a) => QC.Gen a
chooseEnum = QC.choose (minBound, maxBound)


quantityRandomR :: (Random b, RandomGen g) =>
   (a -> b) -> (b -> a) -> (a,a) -> g -> (a,g)
quantityRandomR fromQuantity toQuantity (l,r) =
   mapFst toQuantity . randomR (fromQuantity l, fromQuantity r)

boundedQuantityRandom :: (Bounded a, Random b, RandomGen g) =>
   (a -> b) -> (b -> a) -> g -> (a,g)
boundedQuantityRandom fromQuantity toQuantity =
   quantityRandomR fromQuantity toQuantity (minBound, maxBound)

chooseQuantity :: (Bounded a, Random b) =>
   (a -> b) -> (b -> a) -> QC.Gen a
chooseQuantity fromQuantity toQuantity =
   fmap toQuantity $ QC.choose (fromQuantity minBound, fromQuantity maxBound)


newtype ArbChar = ArbChar {deconsArbChar :: Char}

instance QC.Arbitrary ArbChar where
   arbitrary =
      fmap ArbChar $
      QC.frequency
         [(26, QC.choose ('a','z')),
          (26, QC.choose ('A','Z')),
          (10, QC.choose ('0','9'))]

arbitraryString :: QC.Gen String
arbitraryString =
   fmap (map deconsArbChar) QC.arbitrary


newtype ArbByte = ArbByte {deconsArbByte :: Word8}

instance QC.Arbitrary ArbByte where
   arbitrary =
      fmap (ArbByte . fromIntegral) $ QC.choose (0,0xFF::Int)

arbitraryByteList :: QC.Gen [Word8] -- ByteList
arbitraryByteList =
   fmap (map deconsArbByte) QC.arbitrary
