module Control.Concurrent.Split.MVar (
   T,
   In,
   Out,
   newEmpty,
   take, tryTake,
   put, tryPut,
   ) where

import qualified Control.Concurrent.Split.Class as Split

import qualified Control.Concurrent.MVar as MVar

import Prelude (IO, Maybe, Bool, return, )


newtype T dir a = Cons (MVar.MVar a)

type In  = T Split.In
type Out = T Split.Out


instance Split.C T where
   new = newEmpty
   read = take
   write = put

newEmpty :: IO (In a, Out a)
newEmpty = do
   v <- MVar.newEmptyMVar
   return (Cons v, Cons v)

take :: Out a -> IO a
take (Cons v) = MVar.takeMVar v

tryTake :: Out a -> IO (Maybe a)
tryTake (Cons v) = MVar.tryTakeMVar v

put :: In a -> a -> IO ()
put (Cons v) a = MVar.putMVar v a

tryPut :: In a -> a -> IO Bool
tryPut (Cons v) a = MVar.tryPutMVar v a
