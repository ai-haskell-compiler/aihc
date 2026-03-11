module Sound.ALSA.Sequencer.Query (
  C,
  init,
  next,
  first,
  loop_,
  loop,
  ) where

import qualified Sound.ALSA.Sequencer.Marshal.Sequencer as Seq
import qualified Sound.ALSA.Sequencer.Area as Area

import Control.Monad (liftM2, when, )

import Prelude hiding (init, )


class Area.C info => C info where
  init :: info -> IO ()
  next :: Seq.T mode -> info -> IO Bool

first :: (C info) => Seq.T mode -> IO info
first h = do
  i <- Area.malloc
  init i
  _b <- next h i
  return i

loop_ :: (C info) => Seq.T mode -> (info -> IO ()) -> (info -> IO ()) -> IO ()
loop_ h start f = do
  i <- Area.malloc
  start i
  init i
  let go = do
         b <- next h i
         when b $ f i >> go
  go

loop :: (C info) => Seq.T mode -> (info -> IO ()) -> (info -> IO a) -> IO [a]
loop h start f = do
  i <- Area.malloc
  start i
  init i
  let go = do
         b <- next h i
         if b then liftM2 (:) (f i) go else return []
  go
