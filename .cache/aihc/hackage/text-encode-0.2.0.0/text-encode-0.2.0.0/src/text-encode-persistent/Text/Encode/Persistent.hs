{- | Derive 'PersistField' using 'TextEncode'.

@
    data MyType = ...

    instance 'TextEncode' MyType where ...

    deriving via 'ViaTextEncode' MyType instance 'PersistField' MyType
@
-}
module Text.Encode.Persistent (
    module Text.Encode,
    PersistentEncode (..),
) where

import Text.Encode

import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Typeable (Typeable)
import Database.Persist.Class (PersistField (..))
import Database.Persist.PersistValue (PersistValue (..))
import Text.Convert (asString, asText)

instance (TextEncode a, Typeable a) => PersistField (ViaTextEncode a) where
    {-# INLINE toPersistValue #-}
    toPersistValue = coerce $ PersistText . encodeText @(ViaTextEncode a)

    fromPersistValue (PersistText bs) = coerce $ first asText $ decodeText @(ViaTextEncode a) bs
    fromPersistValue x = Left $ asText $ "DeriveTextEncode PersistentEncode requires PersistText: " <> show x

{- | Derive 'TextEncode' using 'PersistField'.

@
    data MyType = ...

    instance 'FromPersistField' MyType where ...

    deriving via 'PersistentEncoding' MyType instance 'TextEncode' MyType
@

__N.B.__ Do not use this on any type for which you are using 'ViaTextEncode' to
derive 'PersistField'. Your code will loop infinitely.
-}
newtype PersistentEncode a = PersistentEncode a
    deriving (PersistField) via a

instance (PersistField a) => TextPrimitives (PersistentEncode a) where
    textEncode x =
        case toPersistValue x of
            (PersistText txt) -> txt
            x' -> error $ "PersistentEncode requires PersistText: " <> show x'

    textDecode = first asString . fromPersistValue . PersistText
    {-# INLINE textDecode #-}

deriving via TextEncoding (PersistentEncode a) instance (PersistField a) => TextEncode (PersistentEncode a)
