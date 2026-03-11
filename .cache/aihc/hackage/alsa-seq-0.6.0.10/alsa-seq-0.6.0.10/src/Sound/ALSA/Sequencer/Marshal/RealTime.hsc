module Sound.ALSA.Sequencer.Marshal.RealTime where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Utility as U

import qualified Data.Word as Word
import Foreign.Storable
          (Storable, sizeOf, alignment, peek, poke, pokeByteOff, peekByteOff, )


data T = Cons
   { secs :: !(#{intfieldtype snd_seq_real_time_t, tv_sec})
   , nano :: !(#{intfieldtype snd_seq_real_time_t, tv_nsec})
   }
   deriving (Eq)

instance Show T where
   showsPrec prec (Cons s n) =
      U.showsRecord prec "RealTime" [U.showsField s, U.showsField n]


instance Storable T where
  sizeOf _    = #{size snd_seq_real_time_t}
  alignment _ = #{alignment snd_seq_real_time_t}
  peek p      = do s <- #{peek snd_seq_real_time_t, tv_sec} p
                   n <- #{peek snd_seq_real_time_t, tv_nsec} p
                   return Cons { secs = s, nano = n }
  poke p v    = #{poke snd_seq_real_time_t, tv_sec} p (secs v)
             >> #{poke snd_seq_real_time_t, tv_nsec} p (nano v)
