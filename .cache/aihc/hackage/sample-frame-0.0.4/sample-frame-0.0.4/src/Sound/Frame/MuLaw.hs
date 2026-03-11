{-
This data type can be used as sample type for stereo signals.
-}
module Sound.Frame.MuLaw (T, cons, decons, fromLinear16, toLinear16, ) where

import qualified Sound.Frame as Frame

import Foreign.Storable.Newtype as Store
import Foreign.Storable (Storable (..), )

import Data.Word (Word8, )
import Data.Int (Int16, )

import Test.QuickCheck (Arbitrary(arbitrary), )

import Prelude hiding (map, )


newtype T = Cons Word8
   deriving (Eq)


instance Show T where
   showsPrec p x =
      showParen (p >= 10)
         (showString "MuLaw.cons " . shows (decons x))

instance Arbitrary T where
   arbitrary = fmap (cons . fromIntegral :: Int -> T) arbitrary


{-# INLINE cons #-}
cons :: Word8 -> T
cons = Cons

{-# INLINE decons #-}
decons :: T -> Word8
decons (Cons a) = a


{-# INLINE fromLinear16 #-}
fromLinear16 :: Int16 -> T
fromLinear16 x16 =
   let x = fromIntegral x16 :: Int
       logi e y =
          if y < 16
            then e*16 + y
            else logi (e+1) (div (y - 16) 2)
       loga = min 127 . logi 0 . flip div 8
   in  cons . fromIntegral $
       if x >= -2
         then 255 - loga (x+6)
         else 127 - loga (5-x)

{-# INLINE toLinear16 #-}
toLinear16 :: T -> Int16
toLinear16 ymu =
   let y = fromIntegral (decons ymu) :: Int
       (e,m) = divMod y 16
   in  fromIntegral $
       if e>=8
         then (2^(15-e) * ((15-m)*2 + 33) - 33) * 4
         else (2^ (7-e) * ((m-15)*2 - 33) + 33) * 4
{-
         then ((15-m) * 2^(16-e) + (2^(15-e) - 1) * 33) * 4
         else ((m-15) * 2^(8-e) - (2^(7-e) - 1) * 33) * 4
-}


{-
propZero :: Bool
propZero =
   fromLinear16 0 == cons 255 &&
   toLinear16 (cons 255) == 0

propLinear :: T -> Bool
propLinear x =
   fromLinear16 (toLinear16 x) == x
-}


instance Storable T where
   {-# INLINE sizeOf #-}
   sizeOf = Store.sizeOf decons
   {-# INLINE alignment #-}
   alignment = Store.alignment decons
   {-# INLINE peek #-}
   peek = Store.peek cons
   {-# INLINE poke #-}
   poke = Store.poke decons


instance Frame.C T where
   {-# INLINE numberOfChannels #-}
   numberOfChannels _ = 1
   {-# INLINE sizeOfElement #-}
   sizeOfElement = Store.sizeOf decons
