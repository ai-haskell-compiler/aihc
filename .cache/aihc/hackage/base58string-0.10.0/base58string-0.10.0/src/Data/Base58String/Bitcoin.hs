module Data.Base58String.Bitcoin ( B58.Base58String
                                 , b58String
                                 , fromBinary
                                 , toBinary
                                 , fromBytes
                                 , toBytes
                                 , fromText
                                 , toText ) where

import           Control.Applicative (pure)
import           Data.Aeson
import qualified Data.Binary         as B (Binary)
import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

import qualified Data.Base58String   as B58

-- | Our mapping table from binary to base58, based on Bitcoin's table
table :: BS.ByteString
table = BS.pack
        $  [49..57]
        ++ [65..72]
        ++ [74..78]
        ++ [80..90]
        ++ [97..107]
        ++ [109..122]

instance FromJSON B58.Base58String where
 parseJSON = withText "Base58tring" $ pure . b58String . TE.encodeUtf8

instance ToJSON B58.Base58String where
 toJSON = String . toText

b58String :: BS.ByteString -> B58.Base58String
b58String = B58.b58String table

fromBinary :: B.Binary a => a -> B58.Base58String
fromBinary = B58.fromBinary table

toBinary :: B.Binary a => B58.Base58String -> a
toBinary = B58.toBinary table

fromBytes :: BS.ByteString -> B58.Base58String
fromBytes = B58.fromBytes table

toBytes :: B58.Base58String -> BS.ByteString
toBytes = B58.toBytes table

fromText :: T.Text -> B58.Base58String
fromText = B58.fromText table

toText :: B58.Base58String -> T.Text
toText = B58.toText
