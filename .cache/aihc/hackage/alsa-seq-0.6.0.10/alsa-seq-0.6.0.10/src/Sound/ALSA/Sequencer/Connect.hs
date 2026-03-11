{-# LANGUAGE ForeignFunctionInterface #-}
module Sound.ALSA.Sequencer.Connect (
   Connect.T(Connect.Cons, Connect.source, Connect.dest),
   reverse,
   toSubscribers, fromSubscribers,

   createFrom, deleteFrom, withFrom,
   createTo,   deleteTo,   withTo,
   ) where

import qualified Sound.ALSA.Sequencer.Marshal.Connect as Connect
import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr
import qualified Sound.ALSA.Sequencer.Marshal.Port as Port
import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Exception as Exc

import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, )

import Control.Exception (bracket, )

import Prelude hiding (reverse, )


reverse :: Connect.T -> Connect.T
reverse (Connect.Cons src dst) = Connect.Cons dst src

toSubscribers :: Addr.T -> Connect.T
toSubscribers src = Connect.Cons src Addr.subscribers

fromSubscribers :: Addr.T -> Connect.T
fromSubscribers dst = Connect.Cons Addr.subscribers dst


-- | Simple subscription (w\/o exclusive & time conversion).
createFrom :: Seq.AllowInput mode => Seq.T mode -> Port.T -> Addr.T -> IO Connect.T
createFrom s@(Seq.Cons h) me a =
  do Exc.checkResult_ "connect_from" =<<
        uncurry (snd_seq_connect_from h (Port.exp me)) (Addr.exp a)
     mec <- Client.getId s
     return $ Connect.Cons a (Addr.Cons mec me)

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_connect_from"
  snd_seq_connect_from :: Ptr Seq.Core -> C.CInt -> C.CInt -> C.CInt -> IO C.CInt


-- | Simple subscription (w\/o exclusive & time conversion).
createTo :: Seq.AllowOutput mode => Seq.T mode -> Port.T -> Addr.T -> IO Connect.T
createTo s@(Seq.Cons h) me a =
  do Exc.checkResult_ "connect_to" =<<
        uncurry (snd_seq_connect_to h (Port.exp me)) (Addr.exp a)
     mec <- Client.getId s
     return $ Connect.Cons (Addr.Cons mec me) a

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_connect_to"
  snd_seq_connect_to :: Ptr Seq.Core -> C.CInt -> C.CInt -> C.CInt -> IO C.CInt


-- | Simple disconnection.
deleteFrom :: Seq.AllowInput mode => Seq.T mode -> Port.T -> Addr.T -> IO ()
deleteFrom (Seq.Cons h) me a =
  Exc.checkResult_ "disconnect_from" =<< snd_seq_disconnect_from h (Port.exp me) c p
  where (c,p) = Addr.exp a

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_disconnect_from"
  snd_seq_disconnect_from :: Ptr Seq.Core -> C.CInt -> C.CInt -> C.CInt -> IO C.CInt

-- | Simple disconnection.
deleteTo :: Seq.AllowOutput mode => Seq.T mode -> Port.T -> Addr.T -> IO ()
deleteTo (Seq.Cons h) me a =
  Exc.checkResult_ "disconnect_to" =<< snd_seq_disconnect_to h (Port.exp me) c p
  where (c,p) = Addr.exp a

foreign import ccall unsafe "alsa/asoundlib.h snd_seq_disconnect_to"
  snd_seq_disconnect_to :: Ptr Seq.Core -> C.CInt -> C.CInt -> C.CInt -> IO C.CInt


-- | Temporary subscription.
withFrom :: Seq.AllowInput mode => Seq.T mode -> Port.T -> Addr.T -> (Connect.T -> IO a) -> IO a
withFrom h me a =
  bracket (createFrom h me a) (const $ deleteFrom h me a)

-- | Temporary subscription.
withTo :: Seq.AllowOutput mode => Seq.T mode -> Port.T -> Addr.T -> (Connect.T -> IO a) -> IO a
withTo h me a =
  bracket (createTo h me a) (const $ deleteTo h me a)
