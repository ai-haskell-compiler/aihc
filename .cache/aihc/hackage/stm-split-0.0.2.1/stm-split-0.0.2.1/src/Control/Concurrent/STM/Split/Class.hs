module Control.Concurrent.STM.Split.Class (
   In, Out, C(newIO, new, read, write),
   ) where

import Control.Monad.STM (STM, )

import Prelude (IO, )


data In = In
data Out = Out

_dummyIn :: In
_dummyIn = In

_dummyOut :: Out
_dummyOut = Out

class C chan where
   newIO :: IO (chan In a, chan Out a)
   new :: STM (chan In a, chan Out a)
   read :: chan Out a -> STM a
   write :: chan In a -> a -> STM ()
