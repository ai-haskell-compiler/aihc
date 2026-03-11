module Sound.ALSA.Sequencer.Marshal.Time where

#include <alsa/asoundlib.h>
#include <Sound/ALSA/Sequencer/Marshal/Template.h>

import qualified Sound.ALSA.Sequencer.Marshal.RealTime as RealTime

import qualified Sound.ALSA.Sequencer.Utility as U
import qualified Foreign.Storable as St
-- import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr, castPtr, )
import qualified Data.FlagSet as FlagSet
import qualified Data.Word as Word
import Data.Monoid (mappend, )

import qualified Data.Accessor.Basic as Acc


data T = Cons {mode :: Mode, stamp :: Stamp}

instance Show T where
   showsPrec prec (Cons m st) =
      U.showsRecord prec "Time"
         [U.showsField m, U.showsField st]


consAbs :: Stamp -> T
consAbs = Cons Absolute

consRel :: Stamp -> T
consRel = Cons Relative


modeAcc :: Acc.T T Mode
modeAcc =
   Acc.fromSetGet (\x ev -> ev{mode = x}) mode

stampAcc :: Acc.T T Stamp
stampAcc =
   Acc.fromSetGet (\x ev -> ev{stamp = x}) stamp


type Tick = #{inttype snd_seq_tick_time_t}

data Mode = Absolute | Relative
   deriving (Eq, Show, Enum)

data Stamp =
     Tick !Tick
   | Real !RealTime.T
     deriving Show


class Flag flag where

type FlagContainer = #{intfieldtype snd_seq_event_t, flags}

stampTick, stampReal :: Flag flag => FlagSet.MaskedValue FlagContainer flag
stampTick =
   FlagSet.MaskedValue
      #{const SND_SEQ_TIME_STAMP_MASK}
      #{const SND_SEQ_TIME_STAMP_TICK}
stampReal =
   FlagSet.MaskedValue
      #{const SND_SEQ_TIME_STAMP_MASK}
      #{const SND_SEQ_TIME_STAMP_REAL}


modeAbs, modeRel :: Flag flag => FlagSet.MaskedValue FlagContainer flag
modeAbs =
   FlagSet.MaskedValue
      #{const SND_SEQ_TIME_MODE_MASK}
      #{const SND_SEQ_TIME_MODE_ABS}
modeRel =
   FlagSet.MaskedValue
      #{const SND_SEQ_TIME_MODE_MASK}
      #{const SND_SEQ_TIME_MODE_REL}


peek :: Flag flag => FlagSet.T FlagContainer flag -> Ptr T -> IO T
peek flags p =
   fmap
      (Cons
         (if FlagSet.match flags modeAbs
            then Absolute
            else Relative)) $
   peekStamp flags $ castPtr p

poke :: Flag flag => Ptr T -> T -> IO (FlagSet.MaskedValue FlagContainer flag)
poke p (Cons m st) =
   fmap (mappend
      (case m of
         Absolute -> modeAbs
         Relative -> modeRel)) $
   pokeStamp (castPtr p) st


peekStamp :: Flag flag => FlagSet.T FlagContainer flag -> Ptr Stamp -> IO Stamp
peekStamp flags p =
   if FlagSet.match flags stampTick
     then fmap Tick $ St.peek $ castPtr p
     else fmap Real $ St.peek $ castPtr p

pokeStamp :: Flag flag => Ptr Stamp -> Stamp -> IO (FlagSet.MaskedValue FlagContainer flag)
pokeStamp p ts = case ts of
   Tick t -> St.poke (castPtr p) t >> return stampTick
   Real t -> St.poke (castPtr p) t >> return stampReal
