{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Storable.MMap (
  System.IO.MMap.Mode(..),
  unsafeMMapMVector,
  unsafeMMapVector,
  writeMMapVector
) where

import System.IO.MMap
import Foreign.Storable

import qualified Data.Vector.Storable as I
import qualified Data.Vector.Storable.Mutable as M

import qualified Data.Vector.Generic as G


import Data.Int

import Control.Monad
import Control.Monad.Primitive

-- | Map a file into memory as a mutable vector.
unsafeMMapMVector :: forall a. Storable a => FilePath -- ^ Path of the file to map
                                            -> Mode -- ^ Mapping mode
                                            -> Maybe (Int64, Int) -- ^ 'Nothing' to map entire file into memory, otherwise 'Just (fileOffset, elementCount)'
                                            -> IO (M.MVector (PrimState IO) a)
unsafeMMapMVector path mode range = 
  do (foreignPtr, offset, size) <- mmapFileForeignPtr path mode $
        case range of
          Nothing -> Nothing
          Just (start, length) -> Just (start, length * sizeOf (undefined :: a))
     return $ M.unsafeFromForeignPtr foreignPtr offset (size `div` sizeOf (undefined :: a))

-- | Map a file into memory ('ReadOnly' mode) as an immutable vector.
unsafeMMapVector :: forall a. Storable a => FilePath -- ^ Path of the file to map
                                         -> Maybe (Int64, Int) -- ^ 'Nothing' to map entire file into memory, otherwise 'Just (fileOffset, elementCount)'
                                         -> IO (I.Vector a)
unsafeMMapVector path range = 
  do (foreignPtr, offset, size) <- mmapFileForeignPtr path ReadOnly $ 
        case range of
          Nothing -> Nothing
          Just (start, length) -> Just (start, length * sizeOf (undefined :: a))
     return $ I.unsafeFromForeignPtr foreignPtr offset (size `div` sizeOf (undefined :: a))


-- | Write a file from a vector
-- Be careful with existing files, parts behind the mapped range will stay
-- as they are before the write operation.
writeMMapVector :: forall v a. (Storable a, G.Vector v a) 
                => FilePath -- ^ Path of the file to map
                -> v a      -- ^ Vector to write
                -> IO ()
writeMMapVector path src = do
    let len = G.length src
    target <- unsafeMMapMVector path ReadWriteEx (Just (0,len))
    I.copy target (G.convert src)

{-# INLINABLE writeMMapVector #-}
