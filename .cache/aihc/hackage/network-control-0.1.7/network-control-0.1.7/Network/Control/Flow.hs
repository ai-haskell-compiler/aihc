{-# LANGUAGE RecordWildCards #-}

module Network.Control.Flow (
    -- * Flow control

    -- | This is based on the total approach of QUIC rather than
    --   the difference approach of HTTP\/2 because QUIC'one is
    --   considered safer. Please refer to [Using HTTP\/3 Stream Limits in HTTP\/2](https://datatracker.ietf.org/doc/draft-thomson-httpbis-h2-stream-limits/) to understand that QUIC's approaches are better though its topic is about stream concurrency.

    -- ** Constants for flow control.
    defaultMaxStreams,
    defaultMaxStreamData,
    defaultMaxData,

    -- ** Flow control for sending
    TxFlow (..),
    newTxFlow,
    txWindowSize,
    WindowSize,

    -- ** Flow control for receiving
    RxFlow (..),
    newRxFlow,
    rxWindowSize,
    FlowControlType (..),
    maybeOpenRxWindow,
    checkRxLimit,
) where

import Data.Bits

-- | Default max streams. (64)
defaultMaxStreams :: Int
defaultMaxStreams = 64

-- | Default max data of a stream. (256K bytes)
defaultMaxStreamData :: Int
defaultMaxStreamData = 262144

-- | Default max data of a connection.
--
-- By default, this is set to @defaultMaxStreams * defaultMaxStreamData@. This
-- ensures that streams that are not currently handled cannot exhaust the
-- connection window.
--
-- If you use a smaller connection window size, you __must__ ensure that if you
-- are handling fewer concurrent streams than allowed by 'defaultMaxStreams',
-- that the unhandled streams cannot exhaust the connection window, or risk the
-- entire system deadlocking.
defaultMaxData :: Int
defaultMaxData = defaultMaxStreamData * defaultMaxStreams

-- | Window size.
type WindowSize = Int

-- | Flow for sending
--
-- @
-- -------------------------------------->
--        ^           ^
--     txfSent    txfLimit
--
--        |-----------| The size which this node can send
--        txWindowSize
-- @
data TxFlow = TxFlow
    { txfSent :: Int
    -- ^ The total size of sent data.
    , txfLimit :: Int
    -- ^ The total size of data which can be sent.
    }
    deriving (Eq, Show)

-- | Creating TX flow with a receive buffer size.
newTxFlow :: WindowSize -> TxFlow
newTxFlow win = TxFlow 0 win

-- | 'txfLimit' - 'txfSent'.
txWindowSize :: TxFlow -> WindowSize
txWindowSize TxFlow{..} = txfLimit - txfSent

-- | Flow for receiving.
--
-- The goal of 'RxFlow' is to ensure that our network peer does not send us data
-- faster than we can consume it. We therefore impose a maximum number of
-- unconsumed bytes that we are willing to receive from the peer, which we refer
-- to as the buffer size:
--
-- @
--                    rxfBufSize
--           |---------------------------|
-- -------------------------------------------->
--           ^              ^
--      rxfConsumed    rxvReceived
-- @
--
-- The peer does not know of course how many bytes we have consumed of the data
-- that they sent us, so they keep track of their own limit of how much data
-- they are allowed to send. We keep track of this limit also:
--
-- @
--                    rxfBufSize
--           |---------------------------|
-- -------------------------------------------->
--           ^              ^       ^
--      rxfConsumed    rxvReceived  |
--                               rxfLimit
-- @
--
-- Each time we receive data from the peer, we check that they do not exceed the
-- limit ('checkRxLimit'). When we consume data, we periodically send the peer
-- an update (known as a _window update_) of what their new limit is
-- ('maybeOpenRxWindow'). To decrease overhead, we only this if the window
-- update is at least half the window size.
data RxFlow = RxFlow
    { rxfBufSize :: Int
    -- ^ Maxinum number of unconsumed bytes the peer can send us
    --
    -- See discussion above for details.
    , rxfConsumed :: Int
    -- ^ How much of the data that the peer has sent us have we consumed?
    --
    -- This is an absolute number: the total about of bytes consumed over the
    -- lifetime of the connection or stream (i.e., not relative to the window).
    , rxfReceived :: Int
    -- ^ How much data have we received from the peer?
    --
    -- Like 'rxfConsumed', this is an absolute number.
    , rxfLimit :: Int
    -- ^ Current limit on how many bytes the peer is allowed to send us.
    --
    -- Like 'rxfConsumed, this is an absolute number.
    }
    deriving (Eq, Show)

-- | Creating RX flow with an initial window size.
newRxFlow :: WindowSize -> RxFlow
newRxFlow win = RxFlow win 0 0 win

-- | 'rxfLimit' - 'rxfReceived'.
--
-- This is the number of bytes the peer is still allowed to send before they
-- must wait for a window update; see 'RxFlow' for details.
rxWindowSize :: RxFlow -> WindowSize
rxWindowSize RxFlow{..} = rxfLimit - rxfReceived

-- | The representation of window size update.
data FlowControlType
    = -- | HTTP\/2 style
      FCTWindowUpdate
    | -- | QUIC style
      FCTMaxData

-- | Record that we have consumed some received data
--
-- May return a window update; see 'RxFlow' for details.
maybeOpenRxWindow
    :: Int
    -- ^ The consumed size.
    -> FlowControlType
    -> RxFlow
    -> (RxFlow, Maybe Int)
    -- ^ 'Just' if the size should be informed to the peer.
maybeOpenRxWindow consumed fct flow@RxFlow{..}
    | winUpdate >= threshold =
        let flow' =
                flow
                    { rxfConsumed = rxfConsumed'
                    , rxfLimit = rxfLimit'
                    }
            update = case fct of
                FCTWindowUpdate -> winUpdate
                FCTMaxData -> rxfLimit'
         in (flow', Just update)
    | otherwise =
        let flow' = flow{rxfConsumed = rxfConsumed'}
         in (flow', Nothing)
  where
    rxfConsumed' = rxfConsumed + consumed

    -- Minimum window update size
    threshold = rxfBufSize `unsafeShiftR` 1

    -- The window update, /if/ we choose to send it
    rxfLimit' = rxfConsumed' + rxfBufSize
    winUpdate = rxfLimit' - rxfLimit

-- | Checking if received data is acceptable against the
--   current window.
checkRxLimit
    :: Int
    -- ^ The size of received data.
    -> RxFlow
    -> (RxFlow, Bool)
    -- ^ Acceptable if 'True'.
checkRxLimit received flow@RxFlow{..}
    | received' <= rxfLimit =
        let flow' = flow{rxfReceived = received'}
         in (flow', True)
    | otherwise = (flow, False)
  where
    received' = rxfReceived + received
