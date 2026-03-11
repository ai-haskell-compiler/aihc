module Sound.ALSA.Sequencer.Marshal.Connect where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Marshal.Address as Addr

import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, pokeByteOff, peekByteOff, )


data T = Cons
   { source :: !Addr.T
   , dest   :: !Addr.T
   } deriving (Show,Eq,Ord)

instance Storable T where
  sizeOf _    = #{size snd_seq_connect_t}
  alignment _ = #{alignment snd_seq_connect_t}
  peek p      = do s <- #{peek snd_seq_connect_t, sender} p
                   d <- #{peek snd_seq_connect_t, dest} p
                   return Cons { source = s, dest = d }
  poke p v    = #{poke snd_seq_connect_t, sender} p (source v)
             >> #{poke snd_seq_connect_t, dest}   p (dest v)
