{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Data.Binary.Tagged.Internal
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
--
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : portable
--
-- Internals for the library, exported in case you should need it.
-- Usually, the parts you would need should be re-exported in
-- "Data.Binary.Tagged".
--

module Data.Binary.Tagged.Internal (
    -- * Data types
    Tagged          -- abstract, instances: Show, Eq, Binary, Typeable, Generic
  , TagFingerprint  -- abstract, instances: Show, Eq, Ord, Binary, Typeable, Generic, Default
    -- * Tagging and extracting data
  , tag             -- :: Typeable a => a -> Tagged a
  , getTagged       -- :: Typeable a => Tagged a -> Maybe a
  , tagMatched      -- :: Typeable a => Tagged a -> Bool
    -- * 'TagFingerprint' utilities
  , typeFingerprint -- :: Typeable a => a -> TagFingerprint
  , tagFingerprint  -- :: Tagged a -> TagFingerprint
  , bsFingerprint   -- :: ByteString -> Maybe TagFingerprint
  , emptyTagFP      -- :: TagFingerprint
  ) where

import Control.Monad hiding       (mapM_)
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy.Char8 as LC
import Data.Digest.Pure.MD5
import Data.Maybe
import Data.Typeable
import GHC.Generics
import Prelude.Compat


-- | A data type tupling together data with a 'TagFingerprint',
-- representing data tagged with its type.
--
-- It's best to interface directly with data using 'encodeTagged',
-- 'decodeTagged', etc, using 'tag' to tag data and 'extractTagged' to
-- extract data from valid tagged data.  This type is exported mostly when
-- you want to specifically decode a 'ByteString' into tagged data, and
-- manually extract it yourself.  If you are writing a framework, it is
-- preferred to handle this for the end user.
data Tagged a = Tagged !TagFingerprint !a
                deriving (Show, Eq, Generic, Typeable)

instance Binary a => Binary (Tagged a) where
    put (Tagged fp x) = do
      put TagLead
      put fp
      put x
    get = do
      _ <- get :: Get TagLead
      Tagged <$> get <*> get

-- | A data type representing a fingerprint for a 'Typeable' type.
-- Ideally, this would be 'Data.Typeable.Internal''s own 'Fingerprint'
-- types; however, for some reason, the fingerprints for the same data type
-- from the same modules differ between different GHC backends.  So for
-- now, it is just a 'ByteString' representation of the name of the type.
-- This is literally a bad idea, and so two types with the same name but
-- from different modules will share a non-unique 'TagFingerprint'.
-- Hopefully in the future when I find out a way to fix this or the GHC
-- backend maintainers find a way to provide consistent type fingerprints,
-- this will be fixed.
--
-- This type is mostly used for the ability to categorized Tagged items
-- by their type.
--
-- 'emptyTagFP' gives a 'TagFingerprint' that will most likely never be
-- matched by any actual tag from a real type, so can be used as a test if
-- needed.  This replaces functionality that used to come from the
-- 'Default' instance.
newtype TagFingerprint = TagFP MD5Digest
                         deriving (Show, Typeable, Generic, Eq, Ord)

instance Binary TagFingerprint

-- | 'TagFingerprint' that is meant to never be matched by any actual
-- normal type's 'TagFingerprint'.
emptyTagFP :: TagFingerprint
emptyTagFP = TagFP (md5 "")

-- | Put at the start of a 'Tagged' to signify that it is a 'Tagged'.
data TagLead = TagLead

instance Binary TagLead where
    put _ = mapM_ put leadingBytes
    get = do
      forM_ leadingBytes $ \b ->
        guard . (== b) =<< get
      return TagLead

leadingBytes :: [Word8]
leadingBytes = [0xfe,0xfe]

-- | Wrap data inside a 'Tagged' tuple.
tag :: Typeable a => a -> Tagged a
tag x = Tagged (typeFingerprint x) x

-- | Compute the 'Fingerprint' representing a type.  It is non-strict on
-- its parameter, so passing in undefined should work if you want to just
-- get the 'Fingerprint' of a specific type without having data of that
-- type on hand:
--
-- > typeFingerprint (undefined :: Int)
--
typeFingerprint :: Typeable a => a -> TagFingerprint
typeFingerprint = TagFP . md5 . LC.pack . show . typeOf

-- | Extract data out of a 'Tagged', but only the type of the data matches
-- the type represented by the fingerprint.  It is polymorphic on its
-- output and meant to be used when decoding a 'Tagged' item with a desired
-- type.
getTagged :: Typeable a => Tagged a -> Maybe a
getTagged (Tagged tfp x) | tfp == xfp = Just x
                         | otherwise  = Nothing
  where
    xfp = typeFingerprint x

-- | Check if the type inside the 'Tagged' matches the fingerprint.
tagMatched :: Typeable a => Tagged a -> Bool
tagMatched = isJust . getTagged

-- | Extract the 'Fingerprint' out of a 'Tagged'.  Mostly used so that you
-- can categorize and associate Tagged items; to check if a 'Tagged' is
-- of a desired typed, 'getTagged' and 'tagMatched' might be more useful.
tagFingerprint :: Tagged a -> TagFingerprint
tagFingerprint (Tagged fp _) = fp

-- | With a 'ByteString', expecting tagged data, returns the 'Fingerprint'
-- that the data is tagged with.  Returns @Nothing@ if the data is not
-- decodable as tagged data.  Might accidentally decode untagged data
-- though!
bsFingerprint :: ByteString -> Maybe TagFingerprint
bsFingerprint bs = case getRes of
                     Left _         -> Nothing
                     Right (_,_,fp) -> Just fp

  where
    getRes  = flip runGetOrFail bs $ do
                _ <- get :: Get TagLead
                get

