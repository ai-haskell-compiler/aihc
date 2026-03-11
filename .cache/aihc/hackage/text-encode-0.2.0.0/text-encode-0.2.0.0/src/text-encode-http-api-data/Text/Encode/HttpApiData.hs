{- | Derive 'FromFormKey', 'ToFormKey', 'FromHttpApiData', and 'ToHttpApiData'
using 'TextEncode'.

@
    data MyType = ...

    instance 'TextEncode' MyType where ...

    deriving via 'ViaTextEncode' MyType instance 'FromHttpApiData' MyType
    deriving via 'ViaTextEncode' MyType instance 'ToHttpApiData' MyType
@
-}
module Text.Encode.HttpApiData (
    module Text.Encode,
    HttpApiDataEncoding (..),
) where

import Text.Encode

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Text.Convert (asByteString, asString, asText)
import Web.FormUrlEncoded (FromFormKey (..), ToFormKey (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

instance (TextEncode a) => ToFormKey (ViaTextEncode a) where
    toFormKey = coerce $ encodeText @a
    {-# INLINE toFormKey #-}

instance (TextEncode a) => FromFormKey (ViaTextEncode a) where
    parseFormKey = coerce $ first asText . decodeText @a
    {-# INLINE parseFormKey #-}

instance (TextEncode a) => ToHttpApiData (ViaTextEncode a) where
    toUrlPiece = coerce $ encodeText @a
    toHeader = asByteString . toUrlPiece
    toQueryParam = toUrlPiece

    {-# INLINE toUrlPiece #-}
    {-# INLINE toHeader #-}
    {-# INLINE toQueryParam #-}

instance (TextEncode a) => FromHttpApiData (ViaTextEncode a) where
    parseUrlPiece = coerce $ first asText . decodeText @a
    parseHeader = parseUrlPiece . asText
    parseQueryParam = parseUrlPiece

    {-# INLINE parseUrlPiece #-}
    {-# INLINE parseHeader #-}
    {-# INLINE parseQueryParam #-}

{- | Derive 'TextEncode' using 'FromHttpApiData' and 'ToHttpApiData'.

@
    data MyType = ...

    instance 'FromHttpApiData' MyType where ...
    instance 'ToHttpApiData' MyType where ...

    deriving via 'HttpApiDataEncoding' MyType instance 'TextEncode' MyType
@

__N.B.__ Do not use this on any type for which you are using 'ViaTextEncode' to
derive 'FromHttpApiData' or 'ToHttpApiData'. Your code will loop infinitely.
-}
newtype HttpApiDataEncoding a = HttpApiDataEncoding a
    deriving (FromHttpApiData, ToHttpApiData) via a

instance (FromHttpApiData a, ToHttpApiData a) => TextPrimitives (HttpApiDataEncoding a) where
    textEncode = toUrlPiece
    textDecode = first asString . parseUrlPiece

    {-# INLINE textEncode #-}
    {-# INLINE textDecode #-}

deriving via TextEncoding (HttpApiDataEncoding a) instance (FromHttpApiData a, ToHttpApiData a) => TextEncode (HttpApiDataEncoding a)
