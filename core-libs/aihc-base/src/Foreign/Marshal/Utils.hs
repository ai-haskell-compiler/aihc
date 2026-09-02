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

import Foreign.Marshal.Alloc (alloca, malloc)
import Foreign.Storable (Storable (..))
import GHC.Ptr (Ptr, nullPtr)
import GHC.Word (Word8)
import Prelude (Bool (..), Eq (..), IO, Int, Maybe (..), Num (..), error, return, (>>))

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

-- | Memory copies need the C memcpy function, which is not available.
copyBytes :: Ptr a -> Ptr a -> Int -> IO ()
copyBytes _ _ _ = error "Foreign.Marshal.Utils.copyBytes: memory copies are not available"

moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes _ _ _ = error "Foreign.Marshal.Utils.moveBytes: memory copies are not available"

fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes _ _ _ = error "Foreign.Marshal.Utils.fillBytes: memory fills are not available"
