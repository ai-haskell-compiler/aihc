-- | Emulate new api using the old functions
module Sound.ALSA.Sequencer.Client.Info.EventFilter where

import Sound.ALSA.Sequencer.Marshal.ClientInfo (T, T_, with, )
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event

import Foreign.C.Types (CUInt, )
import Foreign.Ptr (Ptr, nullPtr, )
import Foreign.Marshal.Array (allocaArray, copyArray, )
import Foreign.Storable (sizeOf, peekElemOff, pokeElemOff, )
import Data.Bits (testBit, setBit, clearBit, )


{- |
Although alsa/seq.h says CUChar, that's not true,
because internally it uses CUInt and
that makes a difference between little and big endian memory access.
-}
type EventTypeSubset = CUInt
type EventTypeSet = Ptr EventTypeSubset


clear :: T -> IO ()
add :: Event.Type e => T -> e -> IO ()
delete :: Event.Type e => T -> e -> IO ()
check :: Event.Type e => T -> e -> IO Bool

clear i = with i (flip set_ nullPtr)

bit :: Event.Type e => e -> (Int, Int)
bit e =
  divMod
    (fromIntegral $ Event.unEType $ Event.expEv e)
    (8 * sizeOf (undefined :: EventTypeSubset))

size :: Int
size = div 32 (sizeOf (undefined :: EventTypeSubset))
  -- succ $ fst $ bit Event.maxEventType

get :: T -> IO EventTypeSet
get i = with i get_

check i e = do
  ef <- get i
  let (o,b) = bit e
  w <- peekElemOff ef o
  return $ testBit w b

set :: T -> EventTypeSet -> IO ()
set i ef = with i (flip set_ ef)

modify :: T -> (EventTypeSet -> IO ()) -> IO ()
modify i f =
  allocaArray size $ \ef' -> do
    ef <- get i
    copyArray ef' ef size
    f ef'
    set i ef'

modifyByte ::
  (Event.Type e) =>
  (EventTypeSubset -> Int -> EventTypeSubset) -> T -> e -> IO ()
modifyByte f i e =
  modify i $ \ef ->
  let (o,b) = bit e
  in  pokeElemOff ef o . flip f b =<< peekElemOff ef o

add    = modifyByte setBit
delete = modifyByte clearBit

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_client_info_get_event_filter"
  get_ :: Ptr T_ -> IO EventTypeSet

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_client_info_set_event_filter"
  set_ :: Ptr T_ -> EventTypeSet -> IO ()
