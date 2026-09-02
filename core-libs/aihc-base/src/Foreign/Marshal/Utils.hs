{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (Addr#, Int#, RealWorld, State#, Word8#, geAddr#, readWord8OffAddr#, writeWord8OffAddr#, (+#), (-#), (<#))
import GHC.Ptr (Ptr (..), nullPtr)
import GHC.Word (Word8 (..))
import Prelude (Bool (..), Eq (..), Maybe (..), Num (..), return, (>>))

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
copyBytes (Ptr destination) (Ptr source) (I# count) =
  IO
    ( \state ->
        case copyForward destination source 0# count state of
          nextState -> (# nextState, () #)
    )

-- | Copy bytes between two regions that can overlap.
moveBytes :: Ptr a -> Ptr a -> Int -> IO ()
moveBytes (Ptr destination) (Ptr source) (I# count) =
  IO
    ( \state ->
        case geAddr# destination source of
          1# ->
            case copyBackward destination source (count -# 1#) state of
              nextState -> (# nextState, () #)
          _ ->
            case copyForward destination source 0# count state of
              nextState -> (# nextState, () #)
    )

copyForward :: Addr# -> Addr# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
copyForward destination source index count state =
  case index <# count of
    1# ->
      case readWord8OffAddr# source index state of
        (# readState, byte #) ->
          case writeWord8OffAddr# destination index byte readState of
            writeState -> copyForward destination source (index +# 1#) count writeState
    _ -> state

copyBackward :: Addr# -> Addr# -> Int# -> State# RealWorld -> State# RealWorld
copyBackward destination source index state =
  case index <# 0# of
    1# -> state
    _ ->
      case readWord8OffAddr# source index state of
        (# readState, byte #) ->
          case writeWord8OffAddr# destination index byte readState of
            writeState -> copyBackward destination source (index -# 1#) writeState

-- | Set every byte of a region to the given value.
fillBytes :: Ptr a -> Word8 -> Int -> IO ()
fillBytes (Ptr destination) (W8# byte) (I# count) =
  IO
    ( \state ->
        case fillForward destination byte 0# count state of
          nextState -> (# nextState, () #)
    )

fillForward :: Addr# -> Word8# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
fillForward destination byte index count state =
  case index <# count of
    1# ->
      case writeWord8OffAddr# destination index byte state of
        writeState -> fillForward destination byte (index +# 1#) count writeState
    _ -> state
