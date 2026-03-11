module Sound.ALSA.Sequencer.Client.Info.EventFilter where

import Sound.ALSA.Sequencer.Marshal.ClientInfo (T, T_, with, )
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event

import qualified Foreign.C.Types as C
import Foreign.Ptr(Ptr, )


clear :: T -> IO ()
add :: Event.Type e => T -> e -> IO ()
delete :: Event.Type e => T -> e -> IO ()
check :: Event.Type e => T -> e -> IO Bool


clear i = with i clear_

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_client_info_event_filter_clear"
  clear_ :: Ptr T_ -> IO ()

add i e =
  with i (flip add_ $ unpackEType e)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_client_info_event_filter_add"
  add_ :: Ptr T_ -> C.CInt -> IO ()

delete i e =
  with i (flip del_ $ unpackEType e)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_client_info_event_filter_del"
  del_ :: Ptr T_ -> C.CInt -> IO ()

check i e =
  fmap (/= 0) $ with i (flip check_ $ unpackEType e)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_client_info_event_filter_check"
  check_ :: Ptr T_ -> C.CInt -> IO C.CInt


unpackEType :: Event.Type e => e -> C.CInt
unpackEType e = fromIntegral $ Event.unEType $ Event.expEv e
