{-# LANGUAGE DeriveFunctor #-}

module Network.Wai.Streaming ( Flush(..)
                             -- * ByteStrings
                             , streamingRequest
                             , streamingResponse
                             , streamingBody

                             -- * Flushed Builders
                             -- $flush
                             , streamingResponseF
                             , streamingBodyF
                             ) where

import Streaming
import Network.Wai
import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, Builder)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Streaming.Prelude as S
import Data.ByteString as BS

data Flush a = Chunk a
             | Flush
             deriving (Show, Functor)

-- | Stream the 'Request' body
streamingRequest :: MonadIO m => Request -> Stream (Of ByteString) m ()
streamingRequest req = loop
  where
    go = liftIO (requestBody req)
    loop = do
      bs <- go
      unless (BS.null bs) $ do
        yield bs
        loop


-- | Stream strict 'ByteString's into a 'Response'
streamingResponse :: Stream (Of ByteString) IO r
                  -> Status
                  -> ResponseHeaders
                  -> Response
streamingResponse src status headers =
  responseStream status headers (streamingBody src)


-- | Stream strict 'ByteString's into a 'StreamingBody'
streamingBody :: Stream (Of ByteString) IO r -> StreamingBody
streamingBody src write flush = void $ effects $ for src writer
  where
    writer a = do
      lift (write (byteString a))
      lift flush

-- $flush
--
-- Using Flush allows you to explicitly control flushing behavior

-- | Stream 'Builder's into a 'Response'
streamingResponseF :: Stream (Of (Flush Builder)) IO r
                   -> Status
                   -> ResponseHeaders
                   -> Response
streamingResponseF src status headers =
  responseStream status headers (streamingBodyF src)


-- | Stream 'Builder's into a 'StreamingBody'
streamingBodyF :: Stream (Of (Flush Builder)) IO r -> StreamingBody
streamingBodyF src write flush = void $ effects $ for src writer
  where
    writer (Chunk a) = lift (write a)
    writer Flush     = lift flush
