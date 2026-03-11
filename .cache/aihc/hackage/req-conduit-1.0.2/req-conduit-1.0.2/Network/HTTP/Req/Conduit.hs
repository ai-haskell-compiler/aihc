-- |
-- Module      :  Network.HTTP.Req.Conduit
-- Copyright   :  © 2016–present Mark Karpov, Michael Snoyman
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module extends functionality available in "Network.HTTP.Req" with
-- Conduit helpers for streaming big request bodies.
--
-- The package re-uses some pieces of code from the @http-conduit@ package,
-- but not to the extent that depending on that package becomes reasonable.
module Network.HTTP.Req.Conduit
  ( -- * Streaming request bodies
    ReqBodySource (..),

    -- * Streaming response bodies
    -- $streaming-response
    responseBodySource,
  )
where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Conduit (ConduitT, await, yield, ($$+), ($$++))
import Data.IORef
import Data.Int (Int64)
import Network.HTTP.Client qualified as L
import Network.HTTP.Req

----------------------------------------------------------------------------
-- Request bodies

-- | This body option streams contents of request body from the given
-- source. The 'Int64' value is size of the data in bytes.
--
-- Using of this body option does not set the @Content-Type@ header.
data ReqBodySource = ReqBodySource Int64 (ConduitT () ByteString IO ())

instance HttpBody ReqBodySource where
  getRequestBody (ReqBodySource size src) =
    L.RequestBodyStream size (srcToPopperIO src)

----------------------------------------------------------------------------
-- Response interpretations

-- $streaming-response
--
-- The easiest way to stream response of an HTTP request is to use the
-- 'reqBr' function in conjunction with 'responseBodySource':
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Data.Conduit ((.|), runConduitRes)
-- > import Data.Default.Class
-- > import Network.HTTP.Req
-- > import Network.HTTP.Req.Conduit
-- > import qualified Data.Conduit.Binary as CB
-- >
-- > main :: IO ()
-- > main = runReq def $ do
-- >   let size = 100000 :: Int
-- >   reqBr GET (https "httpbin.org" /: "bytes" /~ size) NoReqBody mempty $ \r ->
-- >     runConduitRes $
-- >       responseBodySource r .| CB.sinkFile "my-file.bin"
--
-- This solution benefits from the fact that Req still handles all the
-- details like handling of exceptions and retrying for us. However this
-- approach is only viable when the entire pipeline can be run in 'IO' monad
-- (in the function that is the last argument of 'reqBr').
--
-- If you need to use a more complex monad, use the lower-level function
-- 'req'':
--
-- > {-# LANGUAGE FlexibleInstances #-}
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > module Main (main) where
-- >
-- > import Control.Exception (throwIO)
-- > import Control.Monad.IO.Class (MonadIO (..))
-- > import Control.Monad.Trans.Resource (ResourceT)
-- > import Data.Conduit
-- > import Network.HTTP.Req
-- > import Network.HTTP.Req.Conduit
-- > import qualified Data.Conduit.Binary as CB
-- > import qualified Network.HTTP.Client as L
-- >
-- > instance MonadHttp (ConduitM i o (ResourceT IO)) where
-- >   handleHttpException = liftIO . throwIO
-- >
-- > main :: IO ()
-- > main = runConduitRes $ do
-- >   let size = 100000 :: Int
-- >   req' GET (https "httpbin.org" /: "bytes" /~ size) NoReqBody mempty
-- >     (\request manager ->
-- >       bracketP (L.responseOpen request manager) L.responseClose
-- >         responseBodySource)
-- >     .| CB.sinkFile "my-file.bin"
--
-- 'req'' does not open\/close connections, handle exceptions, and does not
-- perform retrying though, so you're on your own.

-- | Turn @'L.Response' 'L.BodyReader'@ into a producer.
--
-- @since 1.0.0
responseBodySource ::
  (MonadIO m) =>
  -- | Response with body reader
  L.Response L.BodyReader ->
  -- | Response body as a 'C.Producer'
  ConduitT i ByteString m ()
responseBodySource = bodyReaderSource . L.responseBody

----------------------------------------------------------------------------
-- Helpers

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.
srcToPopperIO :: ConduitT () ByteString IO () -> L.GivesPopper ()
srcToPopperIO src f = do
  (rsrc0, ()) <- src $$+ return ()
  irsrc <- newIORef rsrc0
  let popper :: IO ByteString
      popper = do
        rsrc <- readIORef irsrc
        (rsrc', mres) <- rsrc $$++ await
        writeIORef irsrc rsrc'
        case mres of
          Nothing -> return B.empty
          Just bs
            | B.null bs -> popper
            | otherwise -> return bs
  f popper

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.
bodyReaderSource :: (MonadIO m) => L.BodyReader -> ConduitT i ByteString m ()
bodyReaderSource br = go
  where
    go = do
      bs <- liftIO (L.brRead br)
      unless (B.null bs) $ do
        yield bs
        go
