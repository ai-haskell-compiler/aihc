module Data.Accessor.ByteSource where

import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.State (StateT, )
import Control.Monad.Trans.Class (lift, )

import Data.Word (Word8, )


class ByteCompatible byte where
   toByte :: byte -> Word8

instance ByteCompatible Word8 where
   toByte = id


class ByteStream s where
   getWord8 :: Monad m => s -> m (Word8, s)

instance ByteCompatible byte => ByteStream [byte] where
   getWord8 xs =
      case xs of
         (c:cs) -> return (toByte c, cs)
         _ -> fail "ByteStream: no more byte available"


class Monad source => ByteSource source where
   readWord8 :: source Word8

instance (ByteStream s, Monad m) => ByteSource (StateT s m) where
   readWord8 =
      do xs <- State.get
         (c,cs) <- lift (getWord8 xs)
         State.put cs
         return c
