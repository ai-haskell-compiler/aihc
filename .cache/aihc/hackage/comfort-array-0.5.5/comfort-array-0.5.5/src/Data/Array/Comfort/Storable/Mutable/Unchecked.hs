{- |
The functions in this module miss any bound checking.
-}
module Data.Array.Comfort.Storable.Mutable.Unchecked (
   MutArray.Array(Array, shape, buffer),
   MutArray.STArray,
   MutArray.IOArray,
   MutArray.new,
   MutArray.copy,
   MutArray.create,
   MutArray.createWithSize,
   MutArray.createWithSizeAndResult,
   MutArray.unsafeCreate,
   MutArray.unsafeCreateWithSize,
   MutArray.unsafeCreateWithSizeAndResult,
   MutArray.withPtr,
   MutArray.read,
   MutArray.readMaybe,
   MutArray.write,
   MutArray.update,
   MutArray.toList,
   MutArray.fromList,
   MutArray.vectorFromList,

   ImmArray.freeze, ImmArray.unsafeFreeze,
   ImmArray.thaw, ImmArray.unsafeThaw,
   ) where

import qualified Data.Array.Comfort.Storable.Private as ImmArray
import qualified Data.Array.Comfort.Storable.Mutable.Private as MutArray
