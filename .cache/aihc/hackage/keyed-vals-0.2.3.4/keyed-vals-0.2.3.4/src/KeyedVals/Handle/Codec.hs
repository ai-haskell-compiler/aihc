{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Provides a typeclass that converts types to and from keys or vals and
combinators that help it to encode data using 'Handle'

This serves to decouple the encoding/decoding, making it straightforward to use
the typed interface in 'KeyedVals.Handle.Typed' with a wide set of
encoding/decoding schemes
-}
module KeyedVals.Handle.Codec (
  -- * decode/encode support
  EncodeKV (..),
  DecodeKV (..),
  decodeOr,
  decodeOr',
  decodeOrGone,
  decodeOrGone',

  -- * decode encoded @ValsByKey@
  decodeKVs,

  -- * save encoded @ValsByKey@ using a @Handle@
  saveEncodedKVs,
  updateEncodedKVs,

  -- * error conversion
  FromHandleErr (..),
) where

import Data.Bifunctor (bimap)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import KeyedVals.Handle


-- | Specifies how type @a@ encodes as a @Key@ or a @Val@.
class EncodeKV a where
  encodeKV :: a -> Val


-- | Specifies how type @a@ can be decoded from a @Key@ or a @Val@.
class DecodeKV a where
  decodeKV :: Val -> Either Text a


-- | Specifies how to turn 'HandleErr' into a custom error type @err@.
class FromHandleErr err where
  fromHandleErr :: HandleErr -> err


instance FromHandleErr HandleErr where
  fromHandleErr = id


-- | Like 'decodeOr', but transforms 'Nothing' to 'Gone'.
decodeOrGone ::
  (DecodeKV b, FromHandleErr err) =>
  Key ->
  Maybe Val ->
  Either err b
decodeOrGone key x =
  case decodeOr x of
    Left err -> Left err
    Right mb -> maybe (Left $ fromHandleErr $ Gone key) Right mb


-- | Like 'decodeOr'', but transforms 'Nothing' to 'Gone'.
decodeOrGone' ::
  (DecodeKV b, FromHandleErr err) =>
  Key ->
  Either err (Maybe Val) ->
  Either err b
decodeOrGone' key = either Left $ decodeOrGone key


-- | Decode a value, transformi decode errors to type @err@.
decodeOr' ::
  (DecodeKV b, FromHandleErr err) =>
  Either err (Maybe Val) ->
  Either err (Maybe b)
decodeOr' = either Left decodeOr


-- | Decode a value, transforming decode errors to type @err@.
decodeOr ::
  (DecodeKV a, FromHandleErr err) =>
  Maybe Val ->
  Either err (Maybe a)
decodeOr = maybe (pure Nothing) (fmap Just . firstEither notDecoded . decodeKV)


notDecoded :: FromHandleErr err => Text -> err
notDecoded = fromHandleErr . NotDecoded


decode' :: (FromHandleErr err, DecodeKV a) => Val -> Either err a
decode' = either (Left . notDecoded) Right . decodeKV


-- | Decodes a 'Map' from a @ValsByKey@ with encoded @Keys@ and @Vals@.
decodeKVs ::
  (Ord a, DecodeKV a, DecodeKV b, FromHandleErr err) =>
  ValsByKey ->
  Either err (Map a b)
decodeKVs =
  let step _ _ (Left x) = Left x
      step k v (Right m) = case (decode' k, decode' v) of
        (Left x, _) -> Left x
        (_, Left y) -> Left y
        (Right k', Right v') -> Right $ Map.insert k' v' m
   in Map.foldrWithKey step (Right Map.empty)


-- | Like 'saveEncodedKVs', but updates the keys rather than completely replacing it.
updateEncodedKVs ::
  (Ord a, EncodeKV a, EncodeKV b, Monad m, FromHandleErr err) =>
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
updateEncodedKVs = saveOrUpdateKVs True


{- | Encode a 'Map' as a 'ValsByKey' with the @'Key's@ and @'Val's@ encoded.

- 'HandleErr' may be transformed to different error type
-}
saveEncodedKVs ::
  (Ord a, EncodeKV a, EncodeKV b, Monad m, FromHandleErr err) =>
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
saveEncodedKVs = saveOrUpdateKVs False


-- | Encode any 'Map' as a 'ValsByKey' by encoding its @'Key's@ and @'Val's@.
saveOrUpdateKVs ::
  (Ord a, EncodeKV a, EncodeKV b, Monad m, FromHandleErr err) =>
  -- | when @True@, the dict is updated
  Bool ->
  Handle m ->
  Key ->
  Map a b ->
  m (Either err ())
saveOrUpdateKVs _ _ _ kvs | Map.size kvs == 0 = pure $ Right ()
saveOrUpdateKVs update h key dict =
  let asRemote =
        Map.fromList
          . fmap (bimap encodeKV encodeKV)
          . Map.toList
      saver = if update then (updateKVs h) else (saveKVs h)
   in fmap (firstEither fromHandleErr) $ saver key $ asRemote dict


firstEither :: (err1 -> err2) -> Either err1 b -> Either err2 b
firstEither f = either (Left . f) Right
