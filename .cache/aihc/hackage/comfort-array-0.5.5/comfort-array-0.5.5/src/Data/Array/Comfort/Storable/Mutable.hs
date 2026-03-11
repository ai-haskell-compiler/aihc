module Data.Array.Comfort.Storable.Mutable (
   Array,
   MutArray.STArray,
   MutArray.IOArray,
   shape,

   MutArray.new,
   read, MutArray.readMaybe,
   write,
   update,
   toList,
   fromList,
   vectorFromList,

   MutArray.thaw,
   MutArray.freeze,
   ) where

import qualified Data.Array.Comfort.Storable.Mutable.Unchecked as MutArray
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Mutable.Unchecked (Array)
import Data.Maybe (fromMaybe)

import Foreign.Marshal.Array (pokeArray)
import Foreign.Storable (Storable)

import Control.Monad.Primitive (PrimMonad)

import Prelude hiding (read)


shape :: Array m sh a -> sh
shape = MutArray.shape


read ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> m a
read arr ix =
   fromMaybe
      (error "Array.Comfort.Storable.Mutable.read: index out of bounds")
      (MutArray.readMaybe arr ix)

write ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> a -> m ()
write arr ix a =
   if Shape.inBounds (shape arr) ix
      then MutArray.write arr ix a
      else error "Array.Comfort.Storable.Mutable.write: index out of bounds"

update ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> (a -> a) -> m ()
update arr ix f =
   if Shape.inBounds (shape arr) ix
      then MutArray.update arr ix f
      else error "Array.Comfort.Storable.Mutable.update: index out of bounds"

toList :: (PrimMonad m, Shape.C sh, Storable a) => Array m sh a -> m [a]
toList = MutArray.toList

fromList ::
   (PrimMonad m, Shape.C sh, Storable a) => sh -> [a] -> m (Array m sh a)
fromList sh xs =
   MutArray.unsafeCreateWithSize sh $ \size ptr ->
      pokeArray ptr $ take size $
      xs ++
      repeat (error "Array.Comfort.Storable.fromList: list too short for shape")

vectorFromList ::
   (PrimMonad m, Storable a) => [a] -> m (Array m (Shape.ZeroBased Int) a)
vectorFromList = MutArray.vectorFromList
