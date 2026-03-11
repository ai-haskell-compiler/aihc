module System.Posix.Poll (
  Fd(..),
  Event(..),
  Events,
  inp, pri, out, err, hup, nVal,
  ) where

#include <poll.h>

import Foreign.C.Types (CShort, )
import Foreign.Storable
          (Storable(sizeOf, alignment, peek, poke),
           peekByteOff, pokeByteOff, )
import qualified System.Posix.Types as Posix
import Data.Ix (Ix, range, index, inRange, rangeSize, )
import Data.Maybe (fromMaybe, )
import qualified Data.Ix.Enum as IxEnum
import qualified Data.EnumBitSet as EnumSet

data Event 
  = Other Int
  | In
  | Pri
  | Out
  | Err
  | Hup
  | NVal
  deriving (Eq, Ord, Show)

eventFlagSet :: Event -> Events
eventFlagSet cap =
   case cap of
      Other n -> EnumSet.singletonByPosition n
      In      -> inp
      Pri     -> pri
      Out     -> out
      Err     -> err
      Hup     -> hup
      NVal    -> nVal

{- |
The Enum instance may not be very efficient,
but it should hardly be used, at all.
Better use constants such as 'inp' and set manipulation.
If the binary logarithm is computed by constant unfolding,
performance would be better, but direct set manipulation is still faster.
We implement the 'Enum' instance in this way,
in order to stay independent from the particular Poll definitions,
that may differ between platforms.
-}
instance Enum Event where
   fromEnum cap =
      case cap of
         Other n -> n
         _ -> EnumSet.mostSignificantPosition (eventFlagSet cap)
   toEnum n =
      fromMaybe (Other n) $
      lookup (EnumSet.singletonByPosition n) $
      map (\ev -> (eventFlagSet ev, ev)) $
         In :
         Pri :
         Out :
         Err :
         Hup :
         NVal :
         []

instance Ix Event where
   range     = IxEnum.range
   index     = IxEnum.index
   inRange   = IxEnum.inRange
   rangeSize = IxEnum.rangeSize

{-
pollMask :: PollEvent -> CShort
pollMask PollIn	  = #{const POLLIN}
pollMask PollPri  = #{const POLLPRI}
pollMask PollOut  = #{const POLLOUT}
pollMask PollErr  = #{const POLLERR}
pollMask PollHup  = #{const POLLHUP}
pollMask PollNVal = #{const POLLNVAL}
-}

inp, pri, out, err, hup, nVal :: Events
inp  = EnumSet.Cons #{const POLLIN}
pri  = EnumSet.Cons #{const POLLPRI}
out  = EnumSet.Cons #{const POLLOUT}
err  = EnumSet.Cons #{const POLLERR}
hup  = EnumSet.Cons #{const POLLHUP}
nVal = EnumSet.Cons #{const POLLNVAL}


type Events = EnumSet.T CShort Event

data Fd = Fd
  { fd :: Posix.Fd
  , events :: Events
  , rEvents :: Events
  }

instance Storable Fd where
  sizeOf _    = #size struct pollfd
  alignment _ = 4
  peek p      = do
    f <- #{peek struct pollfd, fd}      p
    e <- #{peek struct pollfd, events}  p
    r <- #{peek struct pollfd, revents} p
    return $ Fd (Posix.Fd f) e r
  poke p (Fd (Posix.Fd f) e r) = do
    #{poke struct pollfd, fd}      p f
    #{poke struct pollfd, events}  p e
    #{poke struct pollfd, revents} p r
