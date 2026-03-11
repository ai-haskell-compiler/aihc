module Codec.Archive.Zip.Conduit.Types where

import           Control.Exception (Exception(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit as C
import           Data.Conduit.Binary (sourceLbs)
import           Data.Semigroup (Semigroup(..))
import           Data.String (IsString(..))
import qualified Data.Text as T
import           Data.Time.LocalTime (LocalTime)
import           Data.Typeable (Typeable)
import           Data.Word (Word32, Word64)

-- |Errors thrown during zip file processing
newtype ZipError = ZipError String
  deriving (Show, Typeable)

instance IsString ZipError where
  fromString = ZipError

instance Exception ZipError where
  displayException (ZipError e) = "ZipError: " ++ e

-- |Summary information at the end of a zip stream.
data ZipInfo = ZipInfo
  { zipComment :: ByteString
  } deriving (Eq, Show)

-- |(The beginning of) a single entry in a zip stream, which may be any file or directory.
-- As per zip file conventions, directory names should end with a slash and have no data, but this library does not ensure that.
data ZipEntry = ZipEntry
  { zipEntryName :: Either T.Text ByteString -- ^File name (in posix format, no leading slashes), either UTF-8 encoded text or raw bytes (CP437), with a trailing slash for directories
  , zipEntryTime :: LocalTime -- ^Modification time
  , zipEntrySize :: Maybe Word64 -- ^Size of file data (if known); checked on zipping and also used as hint to enable zip64. Disables compression for known 0-byte files.
  , zipEntryExternalAttributes :: Maybe Word32 -- ^Host-dependent attributes, often MS-DOS directory attribute byte (only supported when zipping)
  } deriving (Eq, Show)

-- |The data contents for a 'ZipEntry'. For empty entries (e.g., directories), use 'mempty'.
data ZipData m
  = ZipDataByteString BSL.ByteString -- ^A known ByteString, which will be fully evaluated (not streamed)
  | ZipDataSource (C.ConduitM () ByteString m ()) -- ^A byte stream producer, streamed (and compressed) directly into the zip

instance Monad m => Semigroup (ZipData m) where
  ZipDataByteString a <> ZipDataByteString b = ZipDataByteString $ mappend a b
  a <> b = ZipDataSource $ mappend (sourceZipData a) (sourceZipData b)

instance Monad m => Monoid (ZipData m) where
  mempty = ZipDataByteString BSL.empty
  mappend = (<>)

-- |Normalize any 'ZipData' to a simple source
sourceZipData :: Monad m => ZipData m -> C.ConduitM () ByteString m ()
sourceZipData (ZipDataByteString b) = sourceLbs b
sourceZipData (ZipDataSource s) = s

-- |Convert between unpacked (as 'Codec.Archive.Zip.Conduit.UnZip.unZipStream' produces) and packed (as 'Codec.Archive.Zip.Conduit.Zip.zipStream' consumes) representations.
-- This is mainly for testing purposes, or if you really want to re-zip a stream on the fly for some reason.
-- Note that each 'ZipData' must be consumed completely before the next entry can be produced.
-- packZipEntries :: C.Conduit (Either ZipEntry BS.ByteString) m (ZipEntry, ZipData m)
