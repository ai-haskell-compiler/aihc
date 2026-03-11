module Control.Concurrent.Split.Class (In, Out, C, new, read, write, ) where

import Prelude (IO, )

data In = In
data Out = Out

_dummyIn :: In
_dummyIn = In

_dummyOut :: Out
_dummyOut = Out

class C chan where
   new :: IO (chan In a, chan Out a)
   read :: chan Out a -> IO a
   write :: chan In a -> a -> IO ()
