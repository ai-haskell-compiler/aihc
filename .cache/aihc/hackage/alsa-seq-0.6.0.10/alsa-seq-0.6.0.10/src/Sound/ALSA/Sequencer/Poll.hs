{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Poll
  ( descriptors
  ) where

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified System.Posix.Poll as Poll

import qualified Foreign.C.Types as C
import Foreign.Marshal.Array (peekArray, allocaArray, )
import Foreign.Ptr (Ptr, )

-- expose EnumSet.Cons for foreign call
import qualified Data.EnumBitSet as EnumSet

_dummyEnumSet :: EnumSet.T Int Bool
_dummyEnumSet = undefined


descriptors :: Seq.T mode -> Poll.Events -> IO [Poll.Fd]
descriptors (Seq.Cons h) e = do
  n <- snd_seq_poll_descriptors_count h e
  allocaArray (fromIntegral n) $ \p ->
    flip peekArray p . fromIntegral =<< snd_seq_poll_descriptors h p n e

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_poll_descriptors_count"
  snd_seq_poll_descriptors_count :: Ptr Seq.Core -> Poll.Events -> IO C.CInt

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_poll_descriptors"
  snd_seq_poll_descriptors :: Ptr Seq.Core -> Ptr Poll.Fd -> C.CInt -> Poll.Events -> IO C.CInt
