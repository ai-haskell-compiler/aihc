
module Data.Radius.Scalar (
  AtText (..), AtString (..), AtInteger (..), AtIpV4 (..),
  Bin128, fromBin128, mayBin128, word64Bin128, bin128Zero,
  ) where

import Data.Monoid ((<>))
import Data.Word (Word32, Word64)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import Data.Serialize (encodeLazy)


newtype AtText     =  AtText     { unAtText :: String }        deriving (Eq, Ord, Show)
newtype AtString   =  AtString   { unAtString :: ByteString }  deriving (Eq, Ord, Show)
newtype AtInteger  =  AtInteger  { unAtInteger :: Word32 }     deriving (Eq, Ord, Show)
newtype AtIpV4     =  AtIpV4     { unAtIpV4 :: Word32 }        deriving (Eq, Ord, Show)

newtype Bin128  =  Bin128 ByteString deriving (Eq, Ord, Show)

fromBin128 :: Bin128 -> ByteString
fromBin128 (Bin128 s) = s

mayBin128 :: ByteString -> Maybe Bin128
mayBin128 bs
  | BS.length bs == 16 = Just $ Bin128 bs
  | otherwise          = Nothing

word64Bin128 :: Word64 -> Word64 -> Bin128
word64Bin128 h l = Bin128 . toStrict $ encodeLazy h <> encodeLazy l

bin128Zero :: Bin128
bin128Zero = word64Bin128 0 0
