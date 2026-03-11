module UnliftIO.Streams
  ( -- * Stream types
    InputStream
  , OutputStream

    -- * Creating streams
  , makeInputStream
  , makeOutputStream

    -- * Primitive stream operations
  , read
  , unRead
  , peek
  , write
  , writeTo
  , atEOF

    -- * Utility streams

  , nullInput
  , nullOutput

    -- * Batteries included
  , module UnliftIO.Streams.ByteString
  , module UnliftIO.Streams.Combinators
  , module UnliftIO.Streams.File
  , module UnliftIO.Streams.List
  , module UnliftIO.Streams.Text
  ) where

import           Control.Monad.IO.Unlift (MonadUnliftIO, liftIO, withRunInIO)
import           Prelude hiding (read)
import           System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S
import           UnliftIO.Streams.Combinators
import           UnliftIO.Streams.ByteString
import           UnliftIO.Streams.File
import           UnliftIO.Streams.List
import           UnliftIO.Streams.Text

{-# INLINE makeInputStream #-}
makeInputStream :: MonadUnliftIO m => m (Maybe a) -> m (InputStream a)
makeInputStream f = withRunInIO $ \io -> S.makeInputStream (io $ f)

{-# INLINE makeOutputStream #-}
makeOutputStream :: MonadUnliftIO m => (Maybe a -> m ()) -> m (OutputStream a)
makeOutputStream f = withRunInIO $ \io -> S.makeOutputStream (io . f)

{-# INLINE peek #-}
peek :: MonadUnliftIO m => InputStream a -> m (Maybe a)
peek as = liftIO $ S.peek as

{-# INLINE read #-}
read :: MonadUnliftIO m => InputStream a -> m (Maybe a)
read as = liftIO $ S.read as

{-# INLINE unRead #-}
unRead :: MonadUnliftIO m => a -> InputStream a -> m ()
unRead a as = liftIO $ S.unRead a as

{-# INLINE write #-}
write :: MonadUnliftIO m => Maybe a -> OutputStream a -> m ()
write a as = liftIO $ S.write a as

{-# INLINE writeTo #-}
writeTo :: MonadUnliftIO m => OutputStream a -> Maybe a -> m ()
writeTo as a = liftIO $ S.writeTo as a

{-# INLINE atEOF #-}
atEOF :: MonadUnliftIO m => InputStream a -> m Bool
atEOF as = liftIO $ S.atEOF as

{-# INLINE nullInput #-}
nullInput :: MonadUnliftIO m => m (InputStream a)
nullInput = liftIO $ S.nullInput

{-# INLINE nullOutput #-}
nullOutput :: MonadUnliftIO m => m (OutputStream a)
nullOutput = liftIO $ S.nullOutput
