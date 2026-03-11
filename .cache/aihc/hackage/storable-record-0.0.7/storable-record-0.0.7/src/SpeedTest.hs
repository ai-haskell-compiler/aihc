-- see also speed test in sample-frame package
module Main where

import qualified Data.StorableVector as SV

import qualified System.TimeIt as T

import Foreign.Storable.Newtype as StoreNew
import Foreign.Storable.Record as Store
import Foreign.Storable (Storable (..), )

import Control.Applicative (liftA2, )

import Data.Word (Word8, )


data MonoN a = MonoN {singleN :: a}
   deriving Show

instance (Storable a) => Storable (MonoN a) where
   {- INLINE sizeOf -}
   sizeOf = StoreNew.sizeOf singleN
   {- INLINE alignment -}
   alignment = StoreNew.alignment singleN
   {- INLINE peek -}
   peek = StoreNew.peek MonoN
   {- INLINE poke -}
   poke = StoreNew.poke singleN


newtype Mono a = Mono {single :: a}
   deriving Show

{-# INLINE storeMono #-}
storeMono :: Storable a => Store.Dictionary (Mono a)
storeMono =
   Store.run $ fmap Mono $ Store.element single

instance (Storable a) => Storable (Mono a) where
   {-# INLINE sizeOf #-}
   sizeOf = Store.sizeOf storeMono
   {-# INLINE alignment #-}
   alignment = Store.alignment storeMono
   {-# INLINE peek #-}
   peek = Store.peek storeMono
   {-# INLINE poke #-}
   poke = Store.poke storeMono


data Stereo a = Stereo {left, right :: a}
   deriving Show

-- inline makes performance even worse
{- INLINE storeStereo -}
storeStereo :: Storable a => Store.Dictionary (Stereo a)
storeStereo =
   Store.run $
   liftA2 Stereo
      (Store.element left)
      (Store.element right)

instance (Storable a) => Storable (Stereo a) where
   {- INLINE sizeOf -}
   sizeOf = Store.sizeOf storeStereo
   {- INLINE alignment -}
   alignment = Store.alignment storeStereo
   {- INLINE peek -}
   peek = Store.peek storeStereo
   {- INLINE poke -}
   poke = Store.poke storeStereo


size :: Int
size = 10000000

main :: IO ()
main = mapM_ T.timeIt $
   (print $ SV.last $ SV.iterateN size (1+) (0::Float)) :
   (print $ SV.last $ SV.iterateN size (1+) (0::Word8)) :
   (print $ SV.last $
    SV.iterateN size (\x -> MonoN (singleN x + 1)) (MonoN (0::Float))) :
   (print $ SV.last $
    SV.iterateN size (\x -> Mono (single x + 1)) (Mono (0::Float))) :
   (print $ SV.last $
    SV.iterateN size (\x -> Stereo (left x + 1) (right x + 3))
       (Stereo 1 2 :: Stereo Float)) :
   []
