module Control.Concurrent.Split.Chan (
   T,
   In,
   Out,
   new,
   read,
   write,
   ) where

import qualified Control.Concurrent.Split.Class as Split

import qualified Control.Concurrent.Chan as Chan

import Prelude (IO, return, )


newtype T dir a = Cons (Chan.Chan a)

type In  = T Split.In
type Out = T Split.Out


instance Split.C T where
   new = new
   read = read
   write = write

new :: IO (In a, Out a)
new = do
   v <- Chan.newChan
   return (Cons v, Cons v)

read :: Out a -> IO a
read (Cons v) = Chan.readChan v

write :: In a -> a -> IO ()
write (Cons v) a = Chan.writeChan v a
