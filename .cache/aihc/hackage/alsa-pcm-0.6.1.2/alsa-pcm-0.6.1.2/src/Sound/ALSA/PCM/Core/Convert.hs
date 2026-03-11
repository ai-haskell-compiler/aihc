module Sound.ALSA.PCM.Core.Convert where

import qualified Foreign.Marshal.Utils as U
import qualified Foreign.Storable as St
import Foreign.Ptr (Ptr, )

import qualified Prelude as P
import Prelude hiding (id, )


data T h c =
        Cons {fromHaskell :: h -> c, toHaskell :: c -> h}

with ::
   (St.Storable c) =>
   T h c -> h -> (Ptr c -> IO a) -> IO a
with conv a = U.with (fromHaskell conv a)

peek ::
   (St.Storable a) =>
   T h a -> Ptr a -> IO h
peek conv aptr = fmap (toHaskell conv) $ St.peek aptr


id :: T a a
id = Cons P.id P.id

int :: (Integral a, Integral b) => T a b
int = Cons fromIntegral fromIntegral

{-
word :: T Word C.CUInt
word = Cons fromIntegral fromIntegral
-}
