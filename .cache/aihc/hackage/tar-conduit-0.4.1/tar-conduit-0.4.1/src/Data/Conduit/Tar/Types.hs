{-# LANGUAGE CPP #-}
#if WINDOWS
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#endif
-- | Module contains all the types necessary for tarball processing.
module Data.Conduit.Tar.Types
    ( Header(..)
    , PaxHeader
    , PaxState (..)
    , initialPaxState
    , TarChunk(..)
    , TarException(..)
    , TarCreateException(..)
    , FileType(..)
    , FileInfo(..)
    , FileOffset
    , ByteCount
    , UserID
    , GroupID
    , DeviceID
    , EpochTime
    , CUid(..)
    , CGid(..)
    , encodeFilePath
    , decodeFilePath
    , getFileInfoPath
    ) where

import           Control.Exception        (Exception)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Short    (ShortByteString)
import           Data.Word
import           System.Posix.Types
import qualified Data.ByteString.Char8         as S8
import           Data.Map                 (Map)
import           Data.Text                     as T
import           Data.Text.Encoding            as T
import           Data.Text.Encoding.Error      as T
#if WINDOWS
import           Data.Bits
import           Foreign.Storable
newtype CUid =
  CUid Word32
  deriving ( Bounded
           , Enum
           , Eq
           , Integral
           , Num
           , Ord
           , Read
           , Real
           , Show
           , Bits
           , Storable
           )
newtype CGid =
  CGid Word32
  deriving ( Bounded
           , Enum
           , Eq
           , Integral
           , Num
           , Ord
           , Read
           , Real
           , Show
           , Bits
           , Storable
           )
type UserID = CUid
type GroupID = CGid
#endif

data FileType
    = FTNormal
    | FTHardLink !ByteString
    | FTSymbolicLink !ByteString
    | FTCharacterSpecial
    | FTBlockSpecial
    | FTDirectory
    | FTFifo
    | FTOther !Word8
    deriving (Show, Eq)


data FileInfo = FileInfo
    { filePath      :: !ByteString -- ^ File path.
    , fileUserId    :: !UserID  -- ^ Unix user id.
    , fileUserName  :: !ByteString  -- ^ Unix user name.
    , fileGroupId   :: !GroupID -- ^ Unix group id.
    , fileGroupName :: !ByteString  -- ^ Unix group name.
    , fileMode      :: !FileMode -- ^ Unix file permissions
    , fileSize      :: !FileOffset -- ^ File size
    , fileType      :: !FileType  -- ^ File type. `FTNormal`, `FTHardLink` (@since 0.3.0),
                                  -- `FTSymbolicLink` and `FTDirectory` are the only ones supported
                                  -- for now
    , fileModTime   :: !EpochTime -- ^ File modification timestamp
    } deriving (Show, Eq)


data Header = Header
    { headerOffset         :: !FileOffset
    , headerPayloadOffset  :: !FileOffset
    , headerFileNameSuffix :: !ShortByteString
    , headerFileMode       :: !CMode
    , headerOwnerId        :: !UserID
    , headerGroupId        :: !GroupID
    , headerPayloadSize    :: !FileOffset
    , headerTime           :: !EpochTime
    , headerLinkIndicator  :: !Word8
    , headerLinkName       :: !ShortByteString
    , headerMagicVersion   :: !ShortByteString
    , headerOwnerName      :: !ShortByteString
    , headerGroupName      :: !ShortByteString
    , headerDeviceMajor    :: !DeviceID
    , headerDeviceMinor    :: !DeviceID
    , headerFileNamePrefix :: !ShortByteString
    }
    deriving Show

-- | Type synonym representing a pax extended header.
type PaxHeader = Map ByteString ByteString

-- | Type representing states (global, next file) given pax extended headers.
data PaxState = PaxState PaxHeader PaxHeader

-- | The initial state before applying any pax extended headers.
initialPaxState :: PaxState
initialPaxState = PaxState mempty mempty

data TarChunk
    = ChunkHeader Header
    | ChunkPayload !FileOffset !ByteString
    | ChunkException TarException
    deriving Show

-- | This the the exception type that is used in this module.
--
-- More constructors are susceptible to be added without bumping the major
-- version of this module.
data TarException
    = NoMoreHeaders
    | UnexpectedPayload !FileOffset
    | IncompleteHeader  !FileOffset
    | IncompletePayload !FileOffset !ByteCount
    | ShortTrailer      !FileOffset
    | BadTrailer        !FileOffset
    | InvalidHeader     !FileOffset
    | BadChecksum       !FileOffset
    | FileTypeError     !FileOffset !Char !String
    | UnsupportedType   !FileType
    deriving Show
instance Exception TarException


data TarCreateException
    = FileNameTooLong   !FileInfo
    | TarCreationError  !String
    deriving Show
instance Exception TarCreateException

-- | Convert `FilePath` into a UTF-8 encoded `ByteString`
encodeFilePath :: FilePath -> S8.ByteString
encodeFilePath = T.encodeUtf8 . T.pack

-- | Convert UTF-8 encoded `ByteString` back into the `FilePath`.
decodeFilePath :: S8.ByteString -> FilePath
decodeFilePath = T.unpack . T.decodeUtf8With T.lenientDecode

-- | Get the `FilePath`.
getFileInfoPath :: FileInfo -> FilePath
getFileInfoPath = decodeFilePath . filePath
