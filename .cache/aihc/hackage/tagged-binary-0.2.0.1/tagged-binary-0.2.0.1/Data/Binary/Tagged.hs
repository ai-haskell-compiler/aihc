-- |
-- Module      : Data.Binary.Tagged
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Provides tools for serializing and decoding data into 'ByteString'
-- tagged with information about its type.  Really, most of this should be
-- used by libraries and frameworks and abstracted over.  Typical use cases
-- are the polymorphic communication channels in distributed computing used
-- by Cloud Haskell and distributed-process --- data of any type can come
-- through the channel, and the framework can chose to ignore, queue, or
-- accept data depending on the type the data is tagged with.  Designed to
-- work with cross-platform GHC backends like ghcjs.
--
-- When decoding data, the result is polymorphic, and you should either
-- allow GHC to infer what you want somehow somewhere, or specify it
-- explicitly.
--
-- Quick example:
--
-- > > let x = encodeTagged (1 :: Int)
-- > > decodeTagged x :: Maybe Bool
-- > Nothing
-- > > decodeTagged x :: Maybe Int
-- > Just 1
--
-- The interface is very similar to that of "Data.Dynamic".
--
-- Also provided here is the internal 'TagFingerprint' data type, so that
-- you can categorize, sort, and queue 'Tagged' or 'ByteString' based on
-- the types they represent.
--
-- It might be significant to note that the current 'TagFingerprint'
-- implementation is a little shaky; it's a bit tricky getting all GHC
-- platforms to agree on a meaningful 'TypeRep' serialization, and we will
-- have a better implementation eventually.  For now, it just uses an MD5
-- hash of the string name of the type.  So for now, don't encode/decode
-- things with the same type name but exist in different modules
-- ("Data.Text.Text" or "Data.Text.Lazy.Text", for example) through the
-- same polymorphic channel! This is a bit limiting, admittedly, but until
-- I or the backend maintainers find out a way to ensure that type
-- fingerprints match up per backend, be aware of this limitation.
--

module Data.Binary.Tagged (
    -- * Encoding and decoding tagged data
    encodeTagged    -- :: (Binary a, Typeable a) => a -> ByteString
  , decodeTagged    -- :: (Binary a, Typeable a) => ByteString -> Maybe a
  , bsFingerprint   -- :: ByteString -> Maybe TagFingerprint
    -- * Manipulating
  , Tagged          -- abstract, instances: Show, Eq, Binary, Typeable, Generic
  , TagFingerprint  -- abstract, instances: Show, Eq, Ord, Binary, Typeable, Generic, Default
  , typeFingerprint -- :: Typeable a => a -> TagFingerprint
  ) where

import Data.Binary
import Data.Binary.Tagged.Internal
import Data.ByteString.Lazy
import Data.Typeable

-- | Encode data into a 'ByteString' with its type data tagged.
--
-- Remember that for now, types are distinguished by their string names, so
-- two types of the same name in different modules will not have unique
-- tags.
encodeTagged :: (Binary a, Typeable a) => a -> ByteString
encodeTagged = encode . tag

-- | Decode tagged data from a 'ByteString'.  The return type is
-- polymorphic, so it'll attempt to decode it by inferred or specified
-- type.
--
--  * If the data is not decoded, @Nothing@ is returned.
--
--  * If successfully decoded data is tagged with a 'Fingerprint' not
--  matching the desired type, @Nothing@ is also returned.
--
--  * If the data is successfully decoded *and* the tagged 'Fingerprint'
--  matches the desired type, @Just x@ is returned, where @x@ is the
--  originally encoded data (with its tag stripped).
decodeTagged :: (Binary a, Typeable a) => ByteString -> Maybe a
decodeTagged bs = case decodeOrFail bs of
                      Left _        -> Nothing
                      Right (_,_,x) -> getTagged x
