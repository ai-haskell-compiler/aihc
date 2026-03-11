-- | A light-weight wrapper around @Network.Wai@ to provide easy pipes support.
module Pipes.Wai
    ( Flush(..)
      -- * Request body
    , producerRequestBody
      -- * Response body
    , responseProducer
    , responseRawProducer
      -- * Re-export
    , module Network.Wai
    ) where

import Network.Wai
import Pipes
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Control.Monad (unless)
import Network.HTTP.Types
import Blaze.ByteString.Builder (Builder)
import Data.IORef

data Flush a = Chunk a | Flush
             deriving (Eq, Ord, Show)

instance Functor Flush where
  fmap f c = case c of
    Chunk a -> Chunk $ f a
    Flush -> Flush

-- | Stream the request body.
--
-- Since 3.0.0
producerRequestBody :: MonadIO m => Request -> Producer ByteString m ()
producerRequestBody req =
    loop
  where
    go = liftIO (requestBody req)

    loop = do
        bs <- go
        unless (S.null bs) $ do
            yield bs
            loop

-- | Create an HTTP response out of a @Producer@.
--
-- Since 3.0.0
responseProducer :: Status -> ResponseHeaders -> Producer (Flush Builder) IO () -> Response
responseProducer s hs src = responseStream s hs $ \send flush ->
  runEffect $ for src $ \mbuilder -> case mbuilder of
    Chunk b -> lift $ send b
    Flush -> lift flush

-- | Create a raw response using a @Producer@ and @Consumer@ to represent the input
-- and output, respectively.
--
-- Since 3.0.0
responseRawProducer :: (MonadIO m, MonadIO n)
                    => (Producer ByteString m () -> Consumer ByteString n () -> IO ())
                    -> Response
                    -> Response
responseRawProducer app = responseRaw app'
  where
    app' recv send =
        app src sink
      where
        src = do
          bs <- liftIO recv
          unless (S.null bs) $ do
            yield bs
            src
        sink = (await >>= liftIO . send) >> sink
