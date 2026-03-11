-- | Abstract sockets connected to 'Box'es.
module Box.Socket.Types
  ( PostSend (..),
    SocketStatus (..),
  )
where

import GHC.Generics (Generic)

-- | Whether to stay open after an emitter ends or send a close after a delay in seconds.
data PostSend = StayOpen | CloseAfter Double deriving (Generic, Eq, Show)

-- | Whether a socket remains open or closed after an action finishes.
data SocketStatus = SocketOpen | SocketClosed | SocketBroken deriving (Generic, Eq, Show)
