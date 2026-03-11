{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Address
  ( Addr.T(Addr.Cons, Addr.client, Addr.port)
  , Addr.unknown
  , Addr.subscribers
  , Addr.broadcast
  , Addr.systemTimer
  , Addr.systemAnnounce
  , parse
  ) where

import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.C.String (CString, withCAString, )
import Foreign.Ptr (Ptr, )
import Foreign.Marshal.Alloc (alloca, )
import Foreign.Storable (peek, )


{- |
Parse the given string into sequencer address.
The client and port are separated by either colon or period, e.g. 128:1.
The function also accepts client names or prefixes thereof.
Throws @(Errno 2)@ exception if no client name matches.
-}
parse
  :: Seq.T mode  -- ^ Sequencer handle.
  -> String       -- ^ String to be parsed.
  -> IO Addr.T    -- ^ The parsed address.
parse (Seq.Cons h) s =
  alloca $ \pa ->
  withCAString s $ \ps ->
    do Exc.checkResult_ "Address.parse" =<< snd_seq_parse_address h pa ps
       peek pa

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_parse_address"
  snd_seq_parse_address :: Ptr Seq.Core -> Ptr Addr.T -> CString -> IO C.CInt
