{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.ZMQ4.Patterns.RequestReply (
  -- * Type class
  RequestReply

  -- * Server and client
, responder
, request
) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)

import Data.Binary
import qualified Data.ByteString.Lazy as BL

import System.ZMQ4.Monadic

-- | A request-reply type class.
--
-- @a@ is the request type, @b@ is the response type.
--
-- Example:
--
-- >>> {-# LANGUAGE DataKinds #-}
-- >>> {-# LANGUAGE TypeApplications #-}
-- >>>
-- >>> import Control.Concurrent.Async
-- >>> import Data.Binary
-- >>>
-- >>> data A = A deriving (Binary, Show)
-- >>> data B = B deriving (Binary, Show)
-- >>>
-- >>> instance RequestReply A B
-- >>>
-- >>> reply :: A -> IO B
-- >>> reply _ = return B
-- >>>
-- >>> main :: IO ()
-- >>> main = withAsync (responder "tcp://*:5000" reply) $ \_ ->
-- >>>     requester "tcp://127.0.0.1:5000" A >>= print
--
class (Binary a, Binary b) => RequestReply a b | a -> b where

-- | Start responding using the given type class.
--
-- See 'RequestReply' for an example.
--
-- Silently ignores a request when decoding fails
responder :: forall a b .
            RequestReply a b
         => String              -- ^ Address to bind to
         -> (a -> IO b)         -- ^ Reply function
         -> IO ()
responder addr reply = runZMQ $ do
    rep <- socket Rep
    bind rep addr

    forever $ (decodeOrFail . BL.fromStrict <$> receive rep) >>= \case
        Left _ -> return ()
        Right (_, _, a :: a) -> do
            (b :: b) <- liftIO $ reply a
            let !msg = encode b
            send' rep [] msg


-- | Request a reply.
--
-- See 'RequestReply' for an example.
--
-- Throws an error when the response cannot be decoded.
request :: forall a b.
           RequestReply a b
        => String               -- ^ Address of the REP socket
        -> a                    -- ^ The request
        -> IO b                 -- ^ The reply
request addr x = runZMQ $ do
    req <- socket Req
    connect req addr

    let !msg = encode x
    send' req [] msg

    bs <- receive req
    return $! decode (BL.fromStrict bs)
