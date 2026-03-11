{- | Derive 'FromField' and 'ToField' using 'TextEncode'.

@
    data MyType = ...

    instance 'TextEncode' MyType where ...

    deriving via 'ViaTextEncode' MyType instance 'FromField' MyType
    deriving via 'ViaTextEncode' MyType instance 'ToField' MyType
@
-}
module Text.Encode.PostgresqlSimple (
    module Text.Encode,
    TextEncodePostgresqlSimpleError (..),
) where

import Text.Encode

import Control.Exception (Exception)
import Control.Monad ((<=<))
import Data.Coerce (coerce)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField (FromField (..), conversionError)
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Text.Convert (Text)

newtype TextEncodePostgresqlSimpleError = TextEncodePostgresqlSimpleError String
    deriving (Show)

instance Exception TextEncodePostgresqlSimpleError

instance (TextEncode a, Typeable a) => FromField (ViaTextEncode a) where
    fromField f =
        either (conversionError . TextEncodePostgresqlSimpleError) (pure . ViaTextEncode)
            . decodeText @a
            <=< fromField @Text f
    {-# INLINE fromField #-}

instance (TextEncode a, Typeable a) => ToField (ViaTextEncode a) where
    toField = coerce $ toField . encodeText @a
    {-# INLINE toField #-}
