
{-# Language TypeSynonymInstances
           , FlexibleInstances
           , ScopedTypeVariables
           , OverloadedStrings
           , UndecidableInstances
           #-}

module Data.Text.ToText where

import Prelude
import Data.Text
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Builder as TLB (toLazyText)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

import Data.Text.ToTextBuilder

import Text.Read
import Data.Typeable
import Data.Maybe (maybe)
import Data.Monoid

class (Typeable a, Read a) => ToText a where
    toText     :: a -> Text
    toText = toStrict . toLazyText
    toLazyText :: a -> TL.Text

    fromText :: Text -> Either Text a
    fromText v = maybe (Left $ "parse failed: `" <> v <>"` can not be parsed as" <> pack (Prelude.show (typeOf (undefined :: a))) ) Right . readMaybe . unpack $ v

    maybeFromText :: Text -> Maybe a
    maybeFromText = either (const Nothing) Just . fromText

    fromLazyText :: TL.Text -> Either Text a
    fromLazyText = fromText . toStrict

    maybeFromLazyText :: TL.Text -> Maybe a
    maybeFromLazyText = either (const Nothing) Just . fromLazyText

instance {-# OVERLAPPABLE #-} (ToTextBuilder a, Read a, Typeable a) => ToText a where
    toLazyText = TLB.toLazyText . toTextBuilder

instance ToText Text where
    toText     = id
    toLazyText = TL.fromStrict

instance ToText TL.Text where
    toLazyText = id

instance ToText String where
    toText     = pack
    toLazyText = TL.pack

instance ToText BS.ByteString where
    toText     = T.decodeUtf8
    toLazyText = TL.decodeUtf8 . BSL.fromChunks . (:[])

instance ToText BSL.ByteString where
    toLazyText = TL.decodeUtf8
