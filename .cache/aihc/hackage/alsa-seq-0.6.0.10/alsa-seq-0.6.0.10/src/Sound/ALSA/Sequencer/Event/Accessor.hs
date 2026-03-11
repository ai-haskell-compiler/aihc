module Sound.ALSA.Sequencer.Event.Accessor (
   highPriority,
   tag,
   queue,
   time,
   timeStamp,
   source,
   dest,
   connect,
   body,
   ) where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Queue as Queue
import qualified Sound.ALSA.Sequencer.Marshal.Time as Time
import qualified Sound.ALSA.Sequencer.Marshal.Event as Event

import qualified Data.Accessor.Basic as Acc


highPriority :: Acc.T Event.T Bool
tag :: Acc.T Event.T Event.Tag
queue :: Acc.T Event.T Queue.T
time :: Acc.T Event.T Time.T
timeStamp :: Acc.T Event.T Time.Stamp
source :: Acc.T Event.T Addr.T
dest :: Acc.T Event.T Addr.T
connect :: Acc.T Event.T Connect.T
body :: Acc.T Event.T Event.Data

highPriority =
   Acc.fromSetGet (\x ev -> ev{Event.highPriority = x}) Event.highPriority
tag =
   Acc.fromSetGet (\x ev -> ev{Event.tag = x}) Event.tag
queue =
   Acc.fromSetGet (\x ev -> ev{Event.queue = x}) Event.queue
time =
   Acc.fromSetGet (\x ev -> ev{Event.time = x}) Event.time
timeStamp = time Acc..> Time.stampAcc
source =
   Acc.fromSetGet (\x ev -> ev{Event.source = x}) Event.source
dest =
   Acc.fromSetGet (\x ev -> ev{Event.dest = x}) Event.dest
connect =
   Acc.fromWrapper
      (\(Connect.Cons src dst) -> (src, dst))
      (uncurry Connect.Cons)
   Acc.<.
   Acc.merge source dest
body =
   Acc.fromSetGet (\x ev -> ev{Event.body = x}) Event.body
