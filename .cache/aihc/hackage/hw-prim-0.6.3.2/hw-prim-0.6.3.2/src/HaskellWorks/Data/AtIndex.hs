{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.AtIndex
    ( Container(..)
    , AtIndex(..)
    , Length(..)
    , atIndexOr
    , atIndexOrBeforeOrAfter
    , atIndexOrBeforeOrLast
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Length
import HaskellWorks.Data.Positioning

import qualified Data.ByteString      as BS
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

class Length v => AtIndex v where
  (!!!)     :: v -> Position -> Elem v
  atIndex   :: v -> Position -> Elem v

instance AtIndex [a] where
  (!!!)   v i = v !! fromIntegral i
  atIndex v i = v !! fromIntegral i
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex BS.ByteString where
  (!!!)   v i = BS.index v (fromIntegral i)
  atIndex v i = BS.index v (fromIntegral i)
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word8) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word16) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word32) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Word64) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word8) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word16) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word32) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Word64) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int8) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int16) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int32) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DV.Vector Int64) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DV.unsafeIndex v (fromIntegral i)
  atIndex v i = DV.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DV.! fromIntegral i
  atIndex v i = v DV.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int8) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int16) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int32) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int64) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

instance AtIndex (DVS.Vector Int) where
#if !defined(BOUNDS_CHECKING_ENABLED)
  (!!!)   v i = DVS.unsafeIndex v (fromIntegral i)
  atIndex v i = DVS.unsafeIndex v (fromIntegral i)
#else
  (!!!)   v i = v DVS.! fromIntegral i
  atIndex v i = v DVS.! fromIntegral i
#endif
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

-- | Get the element of the container at the specified position, but return 'd' if position
-- is out of bounds.
atIndexOr :: AtIndex v => Elem v -> v -> Position -> Elem v
atIndexOr d v vi = if vi >= 0 && vi < end v
  then v !!! vi
  else d
{-# INLINE atIndexOr #-}

-- | Get the element of the container at the specified position, but return 'before' if position
-- before the first element or 'after' if the position is beyond the last element.
atIndexOrBeforeOrAfter :: AtIndex v => Elem v -> Elem v -> v -> Position -> Elem v
atIndexOrBeforeOrAfter before after v vi = if vi < end v
  then if vi >= 0
    then v !!! vi
    else before
  else after
{-# INLINE atIndexOrBeforeOrAfter #-}

-- | Get the element of the container at the specified position, but return the last element
-- if the position is past the end of the container or the default value 'before'' if the position
-- is before the beginning of the vector.
-- In the case when the container is empty, then the default value 'before'' is used.
atIndexOrBeforeOrLast :: (AtIndex v, Length v) => Elem v -> v -> Position -> Elem v
atIndexOrBeforeOrLast before v vi = if vi >= 0
  then if vi < end v
    then v !!! vi
    else if end v /= 0
      then v !!! (end v - 1)
      else before
  else before
{-# INLINE atIndexOrBeforeOrLast #-}
