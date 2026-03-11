module Control.Concurrent.STM.Split.MVar (
   T,
   In,
   Out,
   newEmptyIO,
   newEmpty,
   newIO,
   new,
   take, tryTake,
   put, tryPut,
   write,
   ) where

import qualified Control.Concurrent.STM.Split.Class as Split

import qualified Control.Concurrent.STM.TMVar as MVar
import Control.Monad.STM (STM, )

import Prelude (IO, Maybe, Bool, fmap, ($), (.), return, (>>), )


newtype T dir a = Cons (MVar.TMVar a)

type In  = T Split.In
type Out = T Split.Out


instance Split.C T where
   newIO = newEmptyIO
   new = newEmpty
   read = take
   write = put

split :: MVar.TMVar a -> (In a, Out a)
split v = (Cons v, Cons v)

newEmptyIO :: IO (In a, Out a)
newEmptyIO = fmap split $ MVar.newEmptyTMVarIO

newEmpty :: STM (In a, Out a)
newEmpty = fmap split $ MVar.newEmptyTMVar

newIO :: a -> IO (In a, Out a)
newIO = fmap split . MVar.newTMVarIO

new :: a -> STM (In a, Out a)
new = fmap split . MVar.newTMVar

take :: Out a -> STM a
take (Cons v) = MVar.takeTMVar v

tryTake :: Out a -> STM (Maybe a)
tryTake (Cons v) = MVar.tryTakeTMVar v

put :: In a -> a -> STM ()
put (Cons v) a = MVar.putTMVar v a

tryPut :: In a -> a -> STM Bool
tryPut (Cons v) a = MVar.tryPutTMVar v a


{- |
Write value to 'MVar.TMVar' and overwrite existing content. It never blocks.
Please note, that this function is different
from the generic 'Class.write', which blocks on 'MVar.TMVar's.
-}
write :: In a -> a -> STM ()
write var a =
   clear var >> put var a

clear :: In a -> STM ()
clear (Cons v) =
   MVar.tryTakeTMVar v >> return ()
