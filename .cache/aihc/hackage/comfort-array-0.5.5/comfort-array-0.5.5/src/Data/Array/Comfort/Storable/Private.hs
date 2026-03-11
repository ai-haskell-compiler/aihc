{-# LANGUAGE TypeFamilies #-}
module Data.Array.Comfort.Storable.Private where

import qualified Data.Array.Comfort.Storable.Mutable.Private as MutArray
import qualified Data.Array.Comfort.Shape as Shape
import Data.Semigroup (Semigroup((<>)))
import Data.Monoid (Monoid(mempty, mappend))

import qualified Foreign.Marshal.Array.Guarded as Alloc
import Foreign.Marshal.Array (copyArray, advancePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Storable (Storable)

import System.IO.Unsafe (unsafePerformIO)

import Control.DeepSeq (NFData, rnf)
import Control.Monad.Primitive (PrimMonad, unsafeIOToPrim)
import Control.Monad.ST (runST)
import Control.Monad (liftM)

import Data.Foldable (forM_)


data Array sh a =
   Array {
      shape :: sh,
      buffer :: ForeignPtr a
   }

instance (Shape.C sh, Show sh, Storable a, Show a) => Show (Array sh a) where
   showsPrec p arr =
      showParen (p>10) $ showString $ runST (MutArray.show =<< unsafeThaw arr)

instance (NFData sh) => NFData (Array sh a) where
   rnf (Array sh fptr) = seq fptr (rnf sh)

instance (Shape.C sh, Eq sh, Storable a, Eq a) => Eq (Array sh a) where
   a@(Array sha _) == b@(Array shb _)  =  sha==shb && toList a == toList b

reshape :: sh1 -> Array sh0 a -> Array sh1 a
reshape sh (Array _ fptr) = Array sh fptr

mapShape :: (sh0 -> sh1) -> Array sh0 a -> Array sh1 a
mapShape f arr = reshape (f $ shape arr) arr


infixl 9 !

(!) :: (Shape.Indexed sh, Storable a) => Array sh a -> Shape.Index sh -> a
(!) arr ix = runST (flip MutArray.read ix =<< unsafeThaw arr)

toList :: (Shape.C sh, Storable a) => Array sh a -> [a]
toList arr = runST (MutArray.toList =<< unsafeThaw arr)

fromList :: (Shape.C sh, Storable a) => sh -> [a] -> Array sh a
fromList sh arr = runST (unsafeFreeze =<< MutArray.fromList sh arr)

vectorFromList :: (Storable a) => [a] -> Array (Shape.ZeroBased Int) a
vectorFromList arr = runST (unsafeFreeze =<< MutArray.vectorFromList arr)


(//) ::
   (Shape.Indexed sh, Storable a) =>
   Array sh a -> [(Shape.Index sh, a)] -> Array sh a
(//) arr xs = runST (do
   marr <- thaw arr
   forM_ xs $ uncurry $ MutArray.write marr
   unsafeFreeze marr)

accumulate ::
   (Shape.Indexed sh, Storable a) =>
   (a -> b -> a) -> Array sh a -> [(Shape.Index sh, b)] -> Array sh a
accumulate f arr xs = runST (do
   marr <- thaw arr
   forM_ xs $ \(ix,b) -> MutArray.update marr ix $ flip f b
   unsafeFreeze marr)

fromAssociations ::
   (Shape.Indexed sh, Storable a) =>
   a -> sh -> [(Shape.Index sh, a)] -> Array sh a
fromAssociations a sh xs = runST (do
   marr <- MutArray.new sh a
   forM_ xs $ uncurry $ MutArray.write marr
   unsafeFreeze marr)


freeze ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   MutArray.Array m sh a -> m (Array sh a)
freeze (MutArray.Array sh fptr) =
   unsafeIOToPrim $
   liftM (Array sh) $ Alloc.freeze (Shape.size sh) fptr

thaw ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   Array sh a -> m (MutArray.Array m sh a)
thaw (Array sh fptr) =
   unsafeIOToPrim $
   liftM (MutArray.Array sh) $ Alloc.thaw (Shape.size sh) fptr

unsafeFreeze ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   MutArray.Array m sh a -> m (Array sh a)
unsafeFreeze (MutArray.Array sh fptr) =
   unsafeIOToPrim $
   liftM (Array sh) $ Alloc.freezeInplace (Shape.size sh) fptr

unsafeThaw ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   Array sh a -> m (MutArray.Array m sh a)
unsafeThaw (Array sh fptr) =
   unsafeIOToPrim $
   liftM (MutArray.Array sh) $ Alloc.thawInplace (Shape.size sh) fptr


instance (Shape.AppendSemigroup sh, Storable a) => Semigroup (Array sh a) where
   (<>) = append Shape.append

instance (Shape.AppendMonoid sh, Storable a) => Monoid (Array sh a) where
   mappend = (<>)
   mempty = fromList Shape.empty []

append ::
   (Shape.C shx, Shape.C shy, Storable a) =>
   (shx -> shy -> shz) ->
   Array shx a -> Array shy a -> Array shz a
append appendShape (Array shX x) (Array shY y) =
   unsafePerformIO $
   let sizeX = Shape.size shX in
   let sizeY = Shape.size shY in
   fmap (Array (appendShape shX shY) . fst) $
      Alloc.create (sizeX+sizeY) $ \zPtr ->
      withForeignPtr x $ \xPtr ->
      withForeignPtr y $ \yPtr -> do
         copyArray zPtr xPtr sizeX
         copyArray (advancePtr zPtr sizeX) yPtr sizeY
