module Data.Base32String.Default ( B32.Base32String
                                 , b32String
                                 , b32String'
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

import qualified Data.Base32String   as B32

-- | Our mapping table from binary to base32, based on Rfc4648: [A-Z|2-7]
table :: BS.ByteString
table = BS.pack
        $  [65..90]
        ++ [50..55]

instance FromJSON B32.Base32String where
 parseJSON = withText "Base32tring" $ pure . b32String . TE.encodeUtf8

instance ToJSON B32.Base32String where
 toJSON = String . toText

b32String :: BS.ByteString -> B32.Base32String
b32String = B32.b32String table

b32String' :: BS.ByteString -> B32.Base32String
b32String' = B32.b32String' table

fromBinary :: B.Binary a => a -> B32.Base32String
fromBinary = B32.fromBinary table

toBinary :: B.Binary a => B32.Base32String -> a
toBinary = B32.toBinary table

fromBytes :: BS.ByteString -> B32.Base32String
fromBytes = B32.fromBytes table

toBytes :: B32.Base32String -> BS.ByteString
toBytes = B32.toBytes table

fromText :: T.Text -> B32.Base32String
fromText = B32.fromText table

toText :: B32.Base32String -> T.Text
toText = B32.toText
