{- |
Parser which handles the running state
that is used in MIDI messages in realtime and files.
The running state consists of a message code and the message channel.
-}
module Sound.MIDI.Parser.Status
   (T, Status, set, get, run, lift,
    Channel, fromChannel, toChannel, ) where

import qualified Sound.MIDI.Parser.Class as Parser

import qualified Control.Monad.Exception.Synchronous  as Sync
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.State (StateT, evalStateT, )
import Control.Monad (liftM, )

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Arbitrary(arbitrary, shrink), )

import Sound.MIDI.Utility (checkRange, )
import Data.Ix (Ix)


{- |
The 'T' monad parses a track of a MIDI File.
In MIDI, a shortcut is used for long strings of similar MIDI events:
If a stream of consecutive events all have the same type and channel,
the type and channel can be omitted for all but the first event.
To implement this /feature/,
the parser must keep track of the type and channel of the most recent MIDI Event.
This is done by managing a 'Status' in the parser.
-}
type T parser = StateT Status parser

type Status = Maybe (Int,Channel)


set :: Monad parser => Status -> Parser.Fragile (T parser) ()
set = Trans.lift . State.put

get :: Monad parser => Parser.Fragile (T parser) Status
get = Trans.lift State.get

run :: Monad parser => T parser a -> parser a
run = flip evalStateT Nothing


lift :: Monad parser => Parser.Fragile parser a -> Parser.Fragile (T parser) a
lift = Sync.mapExceptionalT Trans.lift


-- * Channel definition

{- |
This definition should be in Message.Channel,
but this results in a cyclic import.
-}
newtype Channel = Channel {fromChannel :: Int} deriving (Show, Eq, Ord, Ix)

toChannel :: Int -> Channel
toChannel = checkRange "Channel" Channel

instance Enum Channel where
   toEnum   = toChannel
   fromEnum = fromChannel

instance Bounded Channel where
   minBound = Channel  0
   maxBound = Channel 15

instance Arbitrary Channel where
   arbitrary = liftM toChannel $ QC.choose (0,15)
   shrink = map (toChannel . flip mod 16) . shrink . fromChannel
