module GHC.IO.Handle.Types
  ( Handle (..),
    HandleState (..),
    newHandle,
  )
where

import GHC.Base (Monad (..), String)
import GHC.IO (IO)
import GHC.IO.FD (IOHandle)
import GHC.IO.IOMode (IOMode)
import GHC.MVar (MVar, newMVar)
import GHC.Ptr (Ptr)

-- | A named, serialized reference to mutable IO resource state.
data Handle = FileHandle String (MVar HandleState)

data HandleState
  = HandleOpen (Ptr IOHandle) IOMode
  | HandleClosed

newHandle :: String -> Ptr IOHandle -> IOMode -> IO Handle
newHandle name rawHandle mode = do
  state <- newMVar (HandleOpen rawHandle mode)
  return (FileHandle name state)
