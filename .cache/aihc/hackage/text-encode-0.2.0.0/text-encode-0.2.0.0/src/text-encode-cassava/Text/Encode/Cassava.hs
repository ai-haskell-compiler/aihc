{- | Derive 'FromField' and 'ToField' using 'TextEncode'.

@
    data MyType = ...

    instance 'TextEncode' MyType where ...

    deriving via 'ViaTextEncode' MyType instance 'FromField' MyType
    deriving via 'ViaTextEncode' MyType instance 'ToField' MyType
@
-}
module Text.Encode.Cassava (
    module Text.Encode,
    CassavaEncoding (..),
) where

import Text.Encode

import Data.Coerce (coerce)
import Data.Csv (FromField (..), ToField (..), runParser)

instance (TextEncode a) => FromField (ViaTextEncode a) where
    parseField = either fail pure . coerce (decodeByteString @a)
    {-# INLINE parseField #-}

instance (TextEncode a) => ToField (ViaTextEncode a) where
    toField = coerce $ encodeByteString @a
    {-# INLINE toField #-}

{- | Derive 'TextEncode' using 'FromField' and 'ToField'.

@
    data MyType = ...

    instance 'FromField' MyType where ...
    instance 'ToField' MyType where ...

    deriving via 'CassavaEncoding' MyType instance 'TextEncode' MyType
@

__N.B.__ Do not use this on any type for which you are using 'ViaTextEncode' to
derive 'FromField' or 'ToField'. Your code will loop infinitely.
-}
newtype CassavaEncoding a = CassavaEncoding a
    deriving (FromField, ToField) via a

instance (FromField a, ToField a) => ByteStringPrimitives (CassavaEncoding a) where
    byteStringEncode = coerce $ toField @a
    byteStringDecode = coerce $ runParser . parseField @a

    {-# INLINE byteStringEncode #-}
    {-# INLINE byteStringDecode #-}

deriving via ByteStringEncoding (CassavaEncoding a) instance (FromField a, ToField a) => TextEncode (CassavaEncoding a)
