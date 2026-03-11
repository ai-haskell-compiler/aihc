module Network.Socket.Wait
  ( -- * Simple Wait Api
    wait
    -- * Advanced Wait Api
  , waitWith
  , EventHandlers (..)
  , defaultDelay
  ) where
import Network.Socket.Wait.Internal