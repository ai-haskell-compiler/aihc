module Sound.ALSA.Sequencer.Marshal.Address where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Marshal.Client as Client
import qualified Sound.ALSA.Sequencer.Marshal.Port as Port

import qualified Sound.ALSA.Sequencer.Utility as U

import qualified Foreign.C.Types as C
import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, pokeByteOff, peekByteOff, )


data T = Cons
   { client :: !Client.T
   , port   :: !Port.T
   } deriving (Eq, Ord)

instance Show T where
   showsPrec prec (Cons c p) =
      U.showsRecord prec "Address" [U.showsField c, U.showsField p]


exp :: T -> (C.CInt,C.CInt)
exp a = (Client.exp (client a), Port.exp (port a))


instance Storable T where
  sizeOf _    = #{size snd_seq_addr_t}
  alignment _ = #{alignment snd_seq_addr_t}
  peek p      = do cl <- #{peek snd_seq_addr_t, client} p
                   po <- #{peek snd_seq_addr_t, port} p
                   return Cons { client = cl, port = po }
  poke p v    = #{poke snd_seq_addr_t, client} p (client v)
             >> #{poke snd_seq_addr_t, port}   p (port v)


unknown :: T
unknown =
   Cons {
      client = Client.unknown,
      port   = Port.unknown
   }

-- | The address of all subscribed ports.
subscribers :: T
subscribers =
   Cons {
      client = Client.subscribers,
      port   = Port.unknown
   }

broadcast :: T
broadcast =
   Cons {
      client = Client.broadcast,
      port   = Port.unknown
   }

systemTimer :: T
systemTimer =
   Cons {
      client = Client.system,
      port   = Port.systemTimer
   }

systemAnnounce :: T
systemAnnounce =
   Cons {
      client = Client.system,
      port   = Port.systemAnnounce
   }
