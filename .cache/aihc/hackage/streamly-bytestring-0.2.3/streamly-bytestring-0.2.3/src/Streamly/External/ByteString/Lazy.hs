{-# LANGUAGE CPP             #-}

module Streamly.External.ByteString.Lazy
  ( chunkReader
  , reader

  , toChunks
  , fromChunks
  , fromChunksIO

  -- Deprecated
  , read
  , readChunks
  )
where

import Data.Word (Word8)
import Streamly.Data.Array (Array)
import System.IO.Unsafe (unsafeInterleaveIO)
import Streamly.Data.Stream (Stream)

-- Internal imports
import Data.ByteString.Lazy.Internal (ByteString(..), chunk)

#if MIN_VERSION_streamly_core(0,2,0)
import Streamly.Internal.Data.Unfold (Unfold(..))
import Streamly.Internal.Data.Stream (Step(..))
#else
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import Streamly.Internal.Data.Stream.StreamD.Type (Step(..))
#endif

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Unfold as Unfold
import qualified Streamly.Data.Stream as Stream

import Prelude hiding (read)

#if MIN_VERSION_streamly_core(0,3,0)
#define UNFOLD_EACH Unfold.unfoldEach
#else
#define UNFOLD_EACH Unfold.many
#endif

-- | Unfold a lazy ByteString to a stream of 'Array' 'Words'.
{-# INLINE  chunkReader #-}
chunkReader :: Monad m => Unfold m ByteString (Array Word8)
chunkReader = Unfold step seed
  where
    seed = return
    step (Chunk bs bl) = return $ Yield (Strict.toArray bs) bl
    step Empty = return Stop

-- | Unfold a lazy ByteString to a stream of Word8
{-# INLINE reader #-}
reader :: Monad m => Unfold m ByteString Word8
reader = UNFOLD_EACH Array.reader readChunks

-- TODO: "toChunks" should be called "read" instead
-- | Convert a lazy 'ByteString' to a serial stream of 'Array' 'Word8'.
{-# INLINE toChunks #-}
toChunks :: Monad m => ByteString -> Stream m (Array Word8)
toChunks = Stream.unfold readChunks

{-
newtype LazyIO a = LazyIO { runLazy :: IO a } deriving (Functor, Applicative)

liftToLazy :: IO a -> LazyIO a
liftToLazy = LazyIO

instance Monad LazyIO where
    return = pure
    LazyIO a >>= f = LazyIO (unsafeInterleaveIO a >>= unsafeInterleaveIO . runLazy . f)
-}

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString'.
--
-- IMPORTANT NOTE: This function is lazy only for lazy monads
-- (e.g. Identity). For strict monads (e.g. /IO/) it consumes the entire input
-- before generating the output. For /IO/ monad please use fromChunksIO
-- instead.
--
-- For strict monads like /IO/ you could create a newtype wrapper to make the
-- monad bind operation lazy and lift the stream to that type using hoist, then
-- you can use this function to generate the bytestring lazily. For example you
-- can wrap the /IO/ type to make the bind lazy like this:
--
-- @
-- newtype LazyIO a = LazyIO { runLazy :: IO a } deriving (Functor, Applicative)
--
-- liftToLazy :: IO a -> LazyIO a
-- liftToLazy = LazyIO
--
-- instance Monad LazyIO where
--   return = pure
--   LazyIO a >>= f = LazyIO (unsafeInterleaveIO a >>= unsafeInterleaveIO . runLazy . f)
-- @
--
-- /fromChunks/ can then be used as,
-- @
-- {-# INLINE fromChunksIO #-}
-- fromChunksIO :: Stream IO (Array Word8) -> IO ByteString
-- fromChunksIO str = runLazy (fromChunks (Stream.hoist liftToLazy str))
-- @
{-# INLINE fromChunks #-}
fromChunks :: Monad m => Stream m (Array Word8) -> m ByteString
fromChunks = Stream.foldr chunk Empty . fmap Strict.fromArray

-- | Convert a serial stream of 'Array' 'Word8' to a lazy 'ByteString' in the
-- /IO/ monad.
{-# INLINE fromChunksIO #-}
fromChunksIO :: Stream IO (Array Word8) -> IO ByteString
fromChunksIO =
    -- Although the /IO/ monad is strict in nature we emulate laziness using
    -- 'unsafeInterleaveIO'.
    Stream.foldrM (\x b -> chunk x <$> unsafeInterleaveIO b) (pure Empty)
        . fmap Strict.fromArray

--------------------------------------------------------------------------------
-- Deprecated
--------------------------------------------------------------------------------

{-# DEPRECATED readChunks "Please use chunkReader instead." #-}
{-# INLINE  readChunks #-}
readChunks :: Monad m => Unfold m ByteString (Array Word8)
readChunks = chunkReader

{-# DEPRECATED read "Please use reader instead." #-}
{-# INLINE read #-}
read :: Monad m => Unfold m ByteString Word8
read = reader
