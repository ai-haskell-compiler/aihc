{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE MagicHash #-}

module Foreign.Marshal.Utils
  ( with,
    new,
    fromBool,
    toBool,
    maybeNew,
    maybeWith,
    withMany,
    copyBytes,
    moveBytes,
    fillBytes,
  )
where

import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.Storable (Storable (..))
import GHC.IO (IO)
import GHC.Int (Int (..))
import GHC.Ptr (Ptr (..), nullPtr)
import GHC.Word (Word8 (..))
import Prelude (Bool (..), Eq (..), Maybe (..), Num (..), Ord (..), fromIntegral, otherwise, return, (>>))

with :: (Storable a) => a -> (Ptr a -> IO b) -> IO b
with value action =
  alloca (\pointer -> poke pointer value >> action pointer)

new :: (Storable a) => a -> IO (Ptr a)
new value = do
  pointer <- malloc
  poke pointer value
  return pointer

fromBool :: (Num a) => Bool -> a
fromBool False = 0
fromBool True = 1

toBool :: (Eq a, Num a) => a -> Bool
toBool value = value /= 0

maybeNew :: (a -> IO (Ptr b)) -> Maybe a -> IO (Ptr b)
maybeNew _ Nothing = return nullPtr
maybeNew make (Just value) = make value

maybeWith :: (a -> (Ptr b -> IO c) -> IO c) -> Maybe a -> (Ptr b -> IO c) -> IO c
maybeWith _ Nothing action = action nullPtr
maybeWith wrap (Just value) action = wrap value action

withMany :: (a -> (b -> result) -> result) -> [a] -> ([b] -> result) -> result
withMany _ [] action = action []
withMany wrap (value : values) action =
  wrap value (\marshalled -> withMany wrap values (\rest -> action (marshalled : rest)))

-- | Copy bytes between two regions that do not overlap.
copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes destination@(Ptr _) source@(Ptr _) count@(I# _)
  | count <= 0 = return ()
  | otherwise = c_memcpy destination source (fromIntegral count)

-- | Copy bytes between two regions that can overlap.
moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes destination@(Ptr _) source@(Ptr _) count@(I# _)
  | count <= 0 = return ()
  | otherwise = c_memmove destination source (fromIntegral count)

-- | Set every byte of a region to the given value.
fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes destination@(Ptr _) byte@(W8# _) count@(I# _)
  | count <= 0 = return ()
  | otherwise = c_memset destination (fromIntegral byte) (fromIntegral count)

foreign import capi unsafe "string.h memcpy"
  c_memcpy :: Ptr a -> Ptr a -> CSize -> IO ()

foreign import capi unsafe "string.h memmove"
  c_memmove :: Ptr a -> Ptr a -> CSize -> IO ()

foreign import capi unsafe "string.h memset"
  c_memset :: Ptr a -> CInt -> CSize -> IO ()
