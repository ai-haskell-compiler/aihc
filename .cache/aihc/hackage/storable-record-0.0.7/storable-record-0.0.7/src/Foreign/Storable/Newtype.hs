{- |
Storable instances for simple wrapped types.

Example:

> import qualified Foreign.Storable.Newtype as Store
>
> newtype MuLaw = MuLaw {deMuLaw :: Word8}
>
> instance Storable MuLaw where
>    sizeOf = Store.sizeOf deMuLaw
>    alignment = Store.alignment deMuLaw
>    peek = Store.peek MuLaw
>    poke = Store.poke deMuLaw
-}
module Foreign.Storable.Newtype where

import Foreign.Ptr (Ptr, castPtr, )
import Foreign.Storable (Storable, )
import qualified Foreign.Storable as Store


sizeOf :: Storable core => (wrapper -> core) -> wrapper -> Int
sizeOf unwrap = Store.sizeOf . unwrap

alignment :: Storable core => (wrapper -> core) -> wrapper -> Int
alignment unwrap = Store.alignment . unwrap


peek :: Storable core =>
   (core -> wrapper) -> Ptr wrapper -> IO wrapper
peek wrap =
   fmap wrap . Store.peek . castPtr

poke :: Storable core =>
   (wrapper -> core) -> Ptr wrapper -> wrapper -> IO ()
poke unwrap ptr =
   Store.poke (castPtr ptr) . unwrap
