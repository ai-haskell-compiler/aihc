-- | @pipes@ utilities for encoding and decoding values as byte streams
--
-- The tutorial at the bottom of this module illustrates how to use this
-- library.
--
-- In this module, the following type synonym compatible with the @lens@,
-- @lens-family@ and @lens-family-core@ libraries is used but not exported:
--
-- @
-- type Lens' a b = forall f . 'Functor' f => (b -> f b) -> (a -> f a)
-- @

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RankNTypes         #-}

module Pipes.Binary (
  -- * Encoding
    encode
  -- ** Explicit 'Put'
  , encodePut

  -- * Decoding
  , decode
  , decoded
  -- ** Including lengths
  , decodeL
  , decodedL
  -- ** Explicit 'Get'
  , decodeGet
  , decodeGetL

  -- * Types
  , DecodingError(..)

  -- * Exports
  -- $exports
  , module Data.Binary
  , module Data.Binary.Get
  , module Data.Binary.Put
  , module Data.ByteString
  , module Pipes.Parse

  -- * Tutorial
  -- $tutorial
  ) where

import           Control.Exception                (Exception)
#if !MIN_VERSION_transformers(0,6,0)
import           Control.Monad.Trans.Error        (Error)
#endif
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Binary                      (Binary (..))
import qualified Data.Binary
import           Data.Binary.Get                  (ByteOffset, Get)
import qualified Data.Binary.Get                  as Get
import           Data.Binary.Put                  (Put)
import qualified Data.Binary.Put                  as Put
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.Data                        (Data, Typeable)
import           GHC.Generics                     (Generic)
import           Pipes
import qualified Pipes.ByteString
import           Pipes.Parse                      (Parser)

--------------------------------------------------------------------------------

type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)

--------------------------------------------------------------------------------

-- | Convert a value to a byte stream.
--
-- @
-- 'encode' :: ('Monad' m, 'Binary' a) => a -> 'Producer'' 'ByteString' m ()
-- @
--
-- Keep in mind that a single encode value might be split into many 'ByteString'
-- chunks, that is, the lenght of the obtained 'Producer' might be greater than
-- 1.
--
-- /Hint:/ You can easily turn this 'Producer'' into a 'Pipe' that encodes
-- 'Binary' instances as they flow downstream using:
--
-- @
-- 'for' 'cat' 'encode' :: ('Monad' m, 'Binary' a) => 'Pipe' a 'ByteString' m r
-- @
encode :: (Monad m, Binary a) => a -> Proxy x' x () ByteString m ()
encode = encodePut . put
{-# INLINE encode #-}

-- | Like 'encode', except this uses an explicit 'Put'.
--
-- @
-- 'encodePut' :: ('Monad' m) => 'Put' -> 'Producer'' 'ByteString' m ()
-- @
encodePut :: (Monad m) => Put -> Proxy x' x () ByteString m ()
encodePut = \p -> Pipes.ByteString.fromLazy (Put.runPut p)
{-# INLINE encodePut #-}

--------------------------------------------------------------------------------

-- | Parse a value from a byte stream.
decode :: (Monad m, Binary a) => Parser ByteString m (Either DecodingError a)
decode = do
    x <- decodeL
    return (case x of
       Left   e     -> Left  e
       Right (_, a) -> Right a)
{-# INLINE decode #-}

-- | /Improper lens/ that turns a stream of bytes into a stream of decoded
-- values.
--
-- By /improper lens/ we mean that in practice you can't expect the
-- /Monad Morphism Laws/ to be true when using 'decoded' with
-- 'Control.Lens.zoom'.
--
-- @
-- 'Control.Lens.zoom' 'decoded' ('return' r) /= 'return' r
-- 'Control.Lens.zoom' 'decoded' (m >>= f)  /= 'Control.Lens.zoom' 'decoded' m >>= 'Control.Lens.zoom' 'decoded' . f
-- @
decoded
  :: (Monad m, Binary a)
  => Lens' (Producer ByteString m r)
           (Producer a m (Either (DecodingError, Producer ByteString m r) r))
decoded k p = fmap _encode (k (_decode p))
  where
    _decode p0 = do
      x <- lift (nextSkipEmpty p0)
      case x of
         Left r         -> return (Right r)
         Right (bs, p1) -> do
            (ea, p2) <- lift $ S.runStateT decode (yield bs >> p1)
            case ea of
               Left  e -> return (Left (e, p2))
               Right a -> yield a >> _decode p2
    _encode p0 = do
      er <- for p0 encode
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
{-# INLINABLE decoded #-}

--------------------------------------------------------------------------------

-- | Like 'decode', but also returns the length of input consumed in order to
-- to decode the value.
decodeL
  :: (Monad m, Binary a)
  => Parser ByteString m (Either DecodingError (ByteOffset, a))
decodeL = decodeGetL get
{-# INLINE decodeL #-}

-- | Like 'decoded', except this tags each decoded value with the length of
-- input consumed in order to decode it.
decodedL
  :: (Monad m, Binary a)
  => Lens' (Producer ByteString m r)
           (Producer (ByteOffset, a) m
                     (Either (DecodingError, Producer ByteString m r) r))
decodedL k p = fmap _encode (k (_decode p))
  where
    _decode p0 = do
      x <- lift (nextSkipEmpty p0)
      case x of
         Left r         -> return (Right r)
         Right (bs, p1) -> do
            (ea, p2) <- lift $ S.runStateT decodeL (yield bs >> p1)
            case ea of
               Left  e -> return (Left (e, p2))
               Right a -> yield a >> _decode p2
    _encode p0 = do
      er <- for p0 (\(_, a) -> encode a)
      case er of
         Left (_, p1) -> p1
         Right r      -> return r
{-# INLINABLE decodedL #-}

--------------------------------------------------------------------------------

-- | Like 'decode', except this requires an explicit 'Get' instead of any
-- 'Binary' instance.
decodeGet :: (Monad m) => Get a -> Parser ByteString m (Either DecodingError a)
decodeGet m = do
    x <- decodeGetL m
    return (case x of
       Left   e     -> Left  e
       Right (_, a) -> Right a)
{-# INLINE decodeGet #-}

-- | Like 'decodeL', except this requires an explicit 'Get' instead of any
-- 'Binary' instance.
decodeGetL
  :: (Monad m)
  => Get a -> Parser ByteString m (Either DecodingError (ByteOffset, a))
decodeGetL m = S.StateT (go id (Get.runGetIncremental m))
  where
    go diffP decoder p0 = case decoder of
      Get.Fail _ off str -> return (Left (DecodingError off str), diffP p0)
      Get.Done bs off  a -> return (Right (off, a), yield bs >> p0)
      Get.Partial k      -> do
         x <- nextSkipEmpty p0
         case x of
            Left   e       -> go diffP (k Nothing) (return e)
            Right (bs, p1) -> go (diffP . (yield bs >>)) (k (Just bs)) p1
{-# INLINABLE decodeGetL #-}

--------------------------------------------------------------------------------

-- | A 'Get' decoding error, as provided by 'Get.Fail'.
data DecodingError = DecodingError
  { deConsumed :: {-# UNPACK #-} !ByteOffset
    -- ^ Number of bytes consumed before the error
  , deMessage  :: !String
    -- ^ Error message
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance Exception DecodingError
#if !MIN_VERSION_transformers(0,6,0)
instance Error     DecodingError
#endif

--------------------------------------------------------------------------------
-- Internal stuff

-- | Like 'Pipes.next', except it skips leading 'BS.null' chunks.
nextSkipEmpty
  :: Monad m
  => Producer ByteString m r
  -> m (Either r (ByteString, Producer ByteString m r))
nextSkipEmpty = go where
    go p0 = do
      x <- next p0
      case x of
         Left  _        -> return x
         Right (a,p1)
          | BS.null a   -> go p1
          | otherwise   -> return x
{-# INLINABLE nextSkipEmpty #-}

--------------------------------------------------------------------------------

-- $exports
--
--  The following types are re-exported from this module for your convenience:
--
--  [From "Data.Binary"] 'Binary'
--
--  [From "Data.Binary.Put"] 'Put'
--
--  [From "Data.Binary.Get"] 'Get', 'ByteOffset'
--
--  [From "Data.ByteString"] 'ByteString'
--
--  [From "Pipes.Parse"] 'Parser'

--------------------------------------------------------------------------------

{- $tutorial

    Use 'encode' to convert values to byte streams

> -- example.hs
>
> import Pipes
> import qualified Pipes.Prelude as P
> import Pipes.Binary
>
> readInts :: Int -> Producer Int IO ()
> readInts n = P.readLn >-> P.take n
>
> encodedValues :: Producer ByteString IO ()
> encodedValues = do
>     for (readInts 3) encode  -- Encode 3 Ints read from user input
>     encode 'C'               -- Encode a 'Char'
>     encode True              -- Encode a 'Bool'

    Use 'decode' to parse a single decoded value or 'decoded' to access a stream
    of decoded values:

> -- example.hs
>
> import Data.ByteString (ByteString)
> import Pipes.Parse
> import Prelude hiding (splitAt)
>
> -- We need to import 'zoom', which can be found in many packages and all work
> -- equally fine for our purposes. Read "Pipes.Parse.Tutorial" for details.
> --
> -- * From the package @lens-family-core@: 'Lens.Family.State.Strict.zoom'
> -- * From the package @lens-family@:      'Lens.Family2.State.Strict.zoom'
> -- * From the package @lens@:             'Control.Lens.Zoom.zoom'
> import Lens.Family.State.Strict (zoom)
>
> decoder :: Parser ByteString IO ()
> decoder = do
>     xs <- zoom (decoded . splitAt 3) drawAll      -- Decode up to three 'Int's
>     lift $ print (xs :: [Int])
>     y  <- decode                                  -- Decode a single 'Char'
>     lift $ print (y :: Either DecodingError Char)
>     z  <- zoom decoded draw                       -- Same as 'decode', but
>     lift $ print (z :: Maybe Bool)                -- with a 'Maybe'
>
> main = evalStateT decoder encodedValues

    Here are some example inputs:

> $ ./example
> 1<Enter>
> 2<Enter>
> 3<Enter>
> [1,2,3]
> Right 'C'
> Just True
> $ ./example
> <Ctrl-D>
> []
> Right 'C'
> Just True

-}
