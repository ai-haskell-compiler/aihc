module Data.Vector.Unboxing.Mutable
  (MVector
  ,IOVector
  ,STVector
  ,Unboxable(Rep)
  ,Generics(..)
  ,Enum(..)
  ,EnumRep(..)
  -- * Accessors
  -- ** Length information
  ,length,null
  -- ** Extracting subvectors (slicing)
  ,slice,init,tail,take,drop,splitAt,unsafeSlice,unsafeInit,unsafeTail
  ,unsafeTake,unsafeDrop
  -- ** Overlapping
  ,overlaps
  -- * Construction
  -- ** Initialisation
  ,new,unsafeNew,replicate,replicateM,clone
  -- ** Growing
  ,grow,unsafeGrow
  -- ** Restricting memory usage
  ,clear
  -- * Zipping and unzipping
  ,zip,zip3,zip4,zip5,zip6,unzip,unzip3,unzip4,unzip5,unzip6
  -- * Accessing individual elements
  ,read,write,modify,swap,unsafeRead,unsafeWrite,unsafeModify,unsafeSwap
  -- * Modifying vectors
  ,nextPermutation
  -- ** Filling and copying
  ,set,copy,move,unsafeCopy,unsafeMove
  -- * Conversions from/to other vector types
  ,coerceMVector
  ,liftCoercionM
  ,mVectorCoercion
  ,toUnboxedMVector
  ,fromUnboxedMVector
  ,coercionWithUnboxedMVector
  ) where

import Prelude (Int,Bool,Ord)
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Unboxing.Internal
import Data.Vector.Unboxing.Instances ()
import Control.Monad.ST
import Control.Monad.Primitive (PrimMonad,PrimState)
import Data.Coerce

type IOVector = MVector RealWorld
type STVector s = MVector s

length :: (Unboxable a) => MVector s a -> Int
length = G.length
{-# INLINE length #-}

null :: (Unboxable a) => MVector s a -> Bool
null = G.null
{-# INLINE null #-}

slice :: (Unboxable a) => Int -> Int -> MVector s a -> MVector s a
slice = G.slice
{-# INLINE slice #-}

init :: (Unboxable a) => MVector s a -> MVector s a
init = G.init
{-# INLINE init #-}

tail :: (Unboxable a) => MVector s a -> MVector s a
tail = G.tail
{-# INLINE tail #-}

take :: (Unboxable a) => Int -> MVector s a -> MVector s a
take = G.take
{-# INLINE take #-}

drop :: (Unboxable a) => Int -> MVector s a -> MVector s a
drop = G.drop
{-# INLINE drop #-}

splitAt :: (Unboxable a) => Int -> MVector s a -> (MVector s a, MVector s a)
splitAt = G.splitAt
{-# INLINE splitAt #-}

unsafeSlice :: (Unboxable a) => Int -> Int -> MVector s a -> MVector s a
unsafeSlice = G.unsafeSlice
{-# INLINE unsafeSlice #-}

unsafeInit :: (Unboxable a) => MVector s a -> MVector s a
unsafeInit = G.unsafeInit
{-# INLINE unsafeInit #-}

unsafeTail :: (Unboxable a) => MVector s a -> MVector s a
unsafeTail = G.unsafeTail
{-# INLINE unsafeTail #-}

unsafeTake :: (Unboxable a) => Int -> MVector s a -> MVector s a
unsafeTake = G.unsafeTake
{-# INLINE unsafeTake #-}

unsafeDrop :: (Unboxable a) => Int -> MVector s a -> MVector s a
unsafeDrop = G.unsafeDrop
{-# INLINE unsafeDrop #-}

overlaps :: (Unboxable a) => MVector s a -> MVector s a -> Bool
overlaps = G.overlaps
{-# INLINE overlaps #-}

new :: (PrimMonad m, Unboxable a) => Int -> m (MVector (PrimState m) a)
new = G.new
{-# INLINE new #-}

unsafeNew :: (PrimMonad m, Unboxable a) => Int -> m (MVector (PrimState m) a)
unsafeNew = G.unsafeNew
{-# INLINE unsafeNew #-}

replicate :: (PrimMonad m, Unboxable a) => Int -> a -> m (MVector (PrimState m) a)
replicate = G.replicate
{-# INLINE replicate #-}

replicateM :: (PrimMonad m, Unboxable a) => Int -> m a -> m (MVector (PrimState m) a)
replicateM = G.replicateM
{-# INLINE replicateM #-}

clone :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m (MVector (PrimState m) a)
clone = G.clone
{-# INLINE clone #-}

grow :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
grow = G.grow
{-# INLINE grow #-}

unsafeGrow :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m (MVector (PrimState m) a)
unsafeGrow = G.unsafeGrow
{-# INLINE unsafeGrow #-}

clear :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> m ()
clear = G.clear
{-# INLINE clear #-}

read :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m a
read = G.read
{-# INLINE read #-}

write :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> a -> m ()
write = G.write
{-# INLINE write #-}

modify :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
modify = G.modify
{-# INLINE modify #-}

swap :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> Int -> m ()
swap = G.swap
{-# INLINE swap #-}

unsafeRead :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> m a
unsafeRead = G.unsafeRead
{-# INLINE unsafeRead #-}

unsafeWrite :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> a -> m ()
unsafeWrite = G.unsafeWrite
{-# INLINE unsafeWrite #-}

unsafeModify :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> (a -> a) -> Int -> m ()
unsafeModify = G.unsafeModify
{-# INLINE unsafeModify #-}

unsafeSwap :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> Int -> Int -> m ()
unsafeSwap = G.unsafeSwap
{-# INLINE unsafeSwap #-}

nextPermutation :: (PrimMonad m, Ord e, Unboxable e) => MVector (PrimState m) e -> m Bool
nextPermutation = G.nextPermutation
{-# INLINE nextPermutation #-}

set :: (PrimMonad m, Unboxable a) => MVector (PrimState m) a -> a -> m ()
set = G.set
{-# INLINE set #-}

copy :: (PrimMonad m, Unboxable a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
copy = G.copy
{-# INLINE copy #-}

move :: (PrimMonad m, Unboxable a)
     => MVector (PrimState m) a   -- ^ target
     -> MVector (PrimState m) a   -- ^ source
     -> m ()
move = G.move
{-# INLINE move #-}

unsafeCopy :: (PrimMonad m, Unboxable a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
unsafeCopy = G.unsafeCopy
{-# INLINE unsafeCopy #-}

unsafeMove :: (PrimMonad m, Unboxable a)
           => MVector (PrimState m) a   -- ^ target
           -> MVector (PrimState m) a   -- ^ source
           -> m ()
unsafeMove = G.unsafeMove
{-# INLINE unsafeMove #-}

zip :: (Unboxable a, Unboxable b) => MVector s a -> MVector s b -> MVector s (a, b)
zip = coerce UM.zip
{-# INLINE zip #-}

zip3 :: (Unboxable a, Unboxable b, Unboxable c) => MVector s a -> MVector s b -> MVector s c -> MVector s (a, b, c)
zip3 = coerce UM.zip3
{-# INLINE zip3 #-}

zip4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => MVector s a -> MVector s b -> MVector s c -> MVector s d -> MVector s (a, b, c, d)
zip4 = coerce UM.zip4
{-# INLINE zip4 #-}

zip5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => MVector s a -> MVector s b -> MVector s c -> MVector s d -> MVector s e -> MVector s (a, b, c, d, e)
zip5 = coerce UM.zip5
{-# INLINE zip5 #-}

zip6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => MVector s a -> MVector s b -> MVector s c -> MVector s d -> MVector s e -> MVector s f -> MVector s (a, b, c, d, e, f)
zip6 = coerce UM.zip6
{-# INLINE zip6 #-}

unzip :: (Unboxable a, Unboxable b) => MVector s (a, b) -> (MVector s a, MVector s b)
unzip = coerce UM.unzip
{-# INLINE unzip #-}

unzip3 :: (Unboxable a, Unboxable b, Unboxable c) => MVector s (a, b, c) -> (MVector s a, MVector s b, MVector s c)
unzip3 = coerce UM.unzip3
{-# INLINE unzip3 #-}

unzip4 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d) => MVector s (a, b, c, d) -> (MVector s a, MVector s b, MVector s c, MVector s d)
unzip4 = coerce UM.unzip4
{-# INLINE unzip4 #-}

unzip5 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e) => MVector s (a, b, c, d, e) -> (MVector s a, MVector s b, MVector s c, MVector s d, MVector s e)
unzip5 = coerce UM.unzip5
{-# INLINE unzip5 #-}

unzip6 :: (Unboxable a, Unboxable b, Unboxable c, Unboxable d, Unboxable e, Unboxable f) => MVector s (a, b, c, d, e, f) -> (MVector s a, MVector s b, MVector s c, MVector s d, MVector s e, MVector s f)
unzip6 = coerce UM.unzip6
{-# INLINE unzip6 #-}
