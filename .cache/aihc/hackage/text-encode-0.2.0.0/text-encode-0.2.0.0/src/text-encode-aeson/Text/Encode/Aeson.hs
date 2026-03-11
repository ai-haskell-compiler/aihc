{- | Derive 'FromJSON' and 'ToJSON' using 'TextEncode'.

@
    data MyType = ...

    instance 'TextEncode' MyType where ...

    deriving via 'ViaTextEncode' MyType instance 'FromJSON' MyType
    deriving via 'ViaTextEncode' MyType instance 'ToJSON' MyType
@
-}
module Text.Encode.Aeson (
    module Text.Encode,
    AesonEncoding (..),
) where

import Text.Encode

import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
    Value (..),
    eitherDecode,
    encode,
 )
import Data.Coerce (coerce)
import Data.Typeable (Typeable)

instance (TextEncode a, Typeable a) => FromJSON (ViaTextEncode a) where
    parseJSON (String txt) = either fail pure $ decodeText txt
    parseJSON raw = fail $ typedError @a "parseJSON" "Expected String, got " $ encode raw

instance (TextEncode a) => ToJSON (ViaTextEncode a) where
    toJSON = String . encodeText
    toEncoding = toEncoding . encodeText

    {-# INLINE toJSON #-}
    {-# INLINE toEncoding #-}

instance (TextEncode a, Typeable a) => FromJSONKey (ViaTextEncode a)
instance (TextEncode a) => ToJSONKey (ViaTextEncode a)

{- | Derive 'TextEncode' using 'FromJSON' and 'ToJSON'.

@
    data MyType = ...

    instance 'FromJSON' MyType where ...
    instance 'ToJSON' MyType where ...

    deriving via 'AesonEncoding' MyType instance 'TextEncode' MyType
@

__N.B.__ Do not use this on any type for which you are using 'ViaTextEncode' to
derive 'FromJSON' or 'ToJSON'. Your code will loop infinitely.
-}
newtype AesonEncoding a = AesonEncoding a
    deriving (FromJSON, ToJSON) via a

instance (FromJSON a, ToJSON a) => LazyByteStringPrimitives (AesonEncoding a) where
    lazyByteStringEncode = coerce $ encode @a
    lazyByteStringDecode = coerce $ eitherDecode @a

    {-# INLINE lazyByteStringEncode #-}
    {-# INLINE lazyByteStringDecode #-}

deriving via LazyByteStringEncoding (AesonEncoding a) instance (FromJSON a, ToJSON a) => TextEncode (AesonEncoding a)
