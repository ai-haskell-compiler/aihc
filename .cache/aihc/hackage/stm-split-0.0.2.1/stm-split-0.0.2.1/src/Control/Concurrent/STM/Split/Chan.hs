module Control.Concurrent.STM.Split.Chan (
   T,
   In,
   Out,
   newIO,
   new,
   read,
   write,
   writeIO,
   ) where

import qualified Control.Concurrent.STM.Split.Class as Split

import qualified Control.Concurrent.STM.TChan as Chan
import Control.Monad.STM (STM, atomically, )

import Prelude (IO, return, ($), )


newtype T dir a = Cons (Chan.TChan a)

type In  = T Split.In
type Out = T Split.Out


instance Split.C T where
   newIO = newIO
   new = new
   read = read
   write = write

newIO :: IO (In a, Out a)
newIO = do
   v <- Chan.newTChanIO
   return (Cons v, Cons v)

new :: STM (In a, Out a)
new = do
   v <- Chan.newTChan
   return (Cons v, Cons v)

read :: Out a -> STM a
read (Cons v) = Chan.readTChan v

write :: In a -> a -> STM ()
write (Cons v) a = Chan.writeTChan v a

writeIO :: In a -> a -> IO ()
writeIO chan a = atomically $ write chan a
