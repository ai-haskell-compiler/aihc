------------------------------------------------------------------------------
-- |
-- Module      : System.Linux.FileExtents
--
-- Stability   : provisional
-- Portability : non-portable (requires Linux)
--
-- This module can be used to retrieve information about how a
-- particular file is stored on disk (i.e. the file fragmentation).
-- It accomplishes that by calling the FIEMAP ioctl provided by
-- recent versions of the Linux kernel. This ioctl is specific to Linux
-- and therefore this module is not portable.
--
-- For more information about the FIEMAP ioctl see @filesystems/fiemap.txt@
-- in the kernel documentation.
--
------------------------------------------------------------------------------


module System.Linux.FileExtents
    ( -- * Extent flags
      -- |See @filesystems/fiemap.txt@ in the kernel documentation for a more
      -- detailed description of each of these flags.
      ExtentFlags
    , efLast
    , efUnknown
    , efDelalloc
    , efEncoded
    , efDataEncrypted
    , efNotAligned
    , efDataInline
    , efDataTail
    , efUnwritten
    , efMerged
    , efShared
    -- * Extents
    , Extent(..)
    -- * Request flags
    , ReqFlags(..)
    , defReqFlags
    -- * Getting extent information
    , getExtentsFd
    , getExtents
    , getExtentCountFd
    , getExtentCount
    ) where

import Control.Monad
import Control.Exception
import Data.Maybe

import Foreign hiding (void)
import Foreign.C
import System.Posix.Types
import System.Posix.IO

#include <sys/ioctl.h>
#include <linux/fs.h>
#include "fiemap.h"

--------------------------------------------------------------------------------
-- extent flags

type ExtentFlags = Word32

-- |Last extent in file.
efLast          :: ExtentFlags
efLast          = #const FIEMAP_EXTENT_LAST

-- |Data location unknown.
efUnknown       :: ExtentFlags
efUnknown       = #const FIEMAP_EXTENT_UNKNOWN

-- |Location still pending.
efDelalloc      :: ExtentFlags
efDelalloc      = #const FIEMAP_EXTENT_DELALLOC

-- |Data cannot be read while fs is unmounted.
efEncoded       :: ExtentFlags
efEncoded       = #const FIEMAP_EXTENT_ENCODED

-- |Data is encrypted by fs.
efDataEncrypted :: ExtentFlags
efDataEncrypted = #const FIEMAP_EXTENT_DATA_ENCRYPTED

-- |Extent offsets may not be block aligned.
efNotAligned    :: ExtentFlags
efNotAligned    = #const FIEMAP_EXTENT_NOT_ALIGNED

-- |Data mixed with metadata.
efDataInline    :: ExtentFlags
efDataInline    = #const FIEMAP_EXTENT_DATA_INLINE

-- |Multiple files in block.
efDataTail      :: ExtentFlags
efDataTail      = #const FIEMAP_EXTENT_DATA_TAIL

-- |Space allocated, but no data (i.e. zero).
efUnwritten     :: ExtentFlags
efUnwritten     = #const FIEMAP_EXTENT_UNWRITTEN

-- |File does not natively support extents. Result merged for efficiency.
efMerged        :: ExtentFlags
efMerged        = #const FIEMAP_EXTENT_MERGED

-- |Space shared with other files.
efShared        :: ExtentFlags
efShared        = #const FIEMAP_EXTENT_SHARED

--------------------------------------------------------------------------------
-- extent type

-- |Description of a single extent. All offsets and lengths are in bytes.
data Extent = Extent
    { extLogical :: Word64    -- ^ Offset relative to the beginning of the file.
    , extPhysical :: Word64   -- ^ Offset relative to the beginning of the underlying block device.
    , extLength :: Word64     -- ^ The length of the extent.
    , extFlags :: ExtentFlags -- ^ Flags for this extent.
    }
  deriving (Show, Eq)

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

instance Storable Extent where
    sizeOf _ = #size struct fiemap_extent
    alignment _ = (#alignment struct fiemap_extent)
    peek ptr = do
        extLogical_  <- (#peek struct fiemap_extent, fe_logical ) ptr
        extPhysical_ <- (#peek struct fiemap_extent, fe_physical) ptr
        extLength_   <- (#peek struct fiemap_extent, fe_length  ) ptr
        extFlags_    <- (#peek struct fiemap_extent, fe_flags   ) ptr
        return (Extent extLogical_ extPhysical_ extLength_ extFlags_)
    poke ptr ext = do
        memset (castPtr ptr) 0 (#size struct fiemap_extent)
        (#poke struct fiemap_extent, fe_logical ) ptr (extLogical ext)
        (#poke struct fiemap_extent, fe_physical) ptr (extPhysical ext)
        (#poke struct fiemap_extent, fe_length  ) ptr (extLength ext)
        (#poke struct fiemap_extent, fe_flags   ) ptr (extFlags ext)

--------------------------------------------------------------------------------
-- request flags

-- |Flags that modify the behavior of extent information requests.
data ReqFlags = ReqFlags
    { rfSync :: Bool  -- ^ Sync the file before requesting its extents.
    , rfXattr :: Bool -- ^ Retrieve the extents of the inode's extended attribute lookup tree, instead of its data tree.
    , rfCache :: Bool -- ^ Request caching of the extents (not supported by older kernels).
    }
  deriving (Show, Eq)

-- |Default values for the request flags. All options are disabled.
defReqFlags :: ReqFlags
defReqFlags = ReqFlags False False False

encodeFlags :: ReqFlags -> Word32
encodeFlags f =
    (if rfSync f then (#const FIEMAP_FLAG_SYNC) else 0)
      .|.
    (if rfXattr f then (#const FIEMAP_FLAG_XATTR) else 0)
      .|.
    (if rfCache f then (#const FIEMAP_FLAG_CACHE) else 0)

--------------------------------------------------------------------------------
-- get extents

-- | Retrieve the list of all extents associated with the file
-- referenced by the file descriptor. Extents returned mirror those on disk
-- - that is, the logical offset of the first returned extent may start
-- before the requested range, and the last returned extent may end after
-- the end of the requested range.
--
-- Note: 'getExtentsFd' might call the FIEMAP ioctl multiple times in order to
-- retrieve all the extents of the file. This is necessary when the file
-- has too many fragments. If the file is modified in the meantime, the
-- returned list might be inconsistent.
getExtentsFd
    :: ReqFlags
    -> Fd
    -> Maybe (Word64, Word64) -- ^ The range (offset and length) within the file to look extents for. Use 'Nothing' for the entire file.
    -> IO [Extent]
getExtentsFd = getExtentsPathFd "getExtentsFd" Nothing

-- |Like 'getExtentsFd' except that it operates on file paths instead of
-- file descriptors.
getExtents :: ReqFlags -> FilePath -> Maybe (Word64, Word64) -> IO [Extent]
getExtents flags path range =
    bracket (openFd path ReadOnly defaultFileFlags) closeFd $ \fd ->
        getExtentsPathFd "getExtents" (Just path) flags fd range

getExtentsPathFd :: String -> Maybe FilePath -> ReqFlags -> Fd -> Maybe (Word64, Word64) -> IO [Extent]
getExtentsPathFd loc path flags fd range =
    allocaBytes allocSize $ \fiemap -> do
        let (start, len) = fromMaybe (0, maxBound) range
        memset (castPtr fiemap) 0 (#size struct fiemap)
        l <- getExtentsPathFd' start len fiemap
        return (concat l)
  where
    getExtentsPathFd' start len fiemap = do
        (#poke struct fiemap, fm_start       ) fiemap start
        (#poke struct fiemap, fm_length      ) fiemap len
        (#poke struct fiemap, fm_flags       ) fiemap flags'
        (#poke struct fiemap, fm_extent_count) fiemap maxExtentCount
        ioctl_fiemap loc path fd fiemap
        mappedExtents <- (#peek struct fiemap, fm_mapped_extents) fiemap :: IO Word32
        let extentsPtr = fiemap `plusPtr` (#offset struct fiemap, fm_extents)
        extents <- peekArray (fromIntegral mappedExtents) extentsPtr
        case extents of
            (_ : _) | mappedExtents == maxExtentCount
                    , lExt <- last extents
                    , lExtEnd <- extLogical lExt + extLength lExt
                    , bytesLeft <- start + len - lExtEnd
                    , bytesLeft > 0 -> do
                more <- getExtentsPathFd' lExtEnd bytesLeft fiemap
                return (extents : more)
            _ -> return [extents]
    flags' = encodeFlags flags
    maxExtentCount :: Word32
    maxExtentCount = (fromIntegral allocSize - (#size struct fiemap)) `quot` (#size struct fiemap_extent);
    allocSize = 16 * 1024

--------------------------------------------------------------------------------
-- get extent count

-- |Like 'getExtentsFd' except that it returns the number of extents
-- instead of a list.
getExtentCountFd :: ReqFlags -> Fd -> Maybe (Word64, Word64) -> IO Word32
getExtentCountFd = getExtentCountPathFd "getExtentCountFd" Nothing

-- |Like 'getExtents' except that it returns the number of extents
-- instead of a list.
getExtentCount :: ReqFlags -> FilePath -> Maybe (Word64, Word64) -> IO Word32
getExtentCount flags path range =
    bracket (openFd path ReadOnly defaultFileFlags) closeFd $ \fd ->
        getExtentCountPathFd "getExtentCount" (Just path) flags fd range

getExtentCountPathFd :: String -> Maybe FilePath -> ReqFlags -> Fd -> Maybe (Word64, Word64) -> IO Word32
getExtentCountPathFd loc path flags fd range = do
    let (start, len) = fromMaybe (0, maxBound) range
    allocaBytes (#size struct fiemap) $ \fiemap -> do
        memset (castPtr fiemap) 0 (#size struct fiemap)
        (#poke struct fiemap, fm_start       ) fiemap start
        (#poke struct fiemap, fm_length      ) fiemap len
        (#poke struct fiemap, fm_flags       ) fiemap flags'
        (#poke struct fiemap, fm_extent_count) fiemap (0 :: Word32)
        ioctl_fiemap loc path fd fiemap
        #{peek struct fiemap, fm_mapped_extents} fiemap
  where
    flags' = encodeFlags flags

--------------------------------------------------------------------------------
-- auxiliary stuff

foreign import ccall unsafe ioctl :: Fd -> CULong -> Ptr a -> IO CInt

ioctl_fiemap :: String -> Maybe FilePath -> Fd -> Ptr a -> IO ()
ioctl_fiemap loc mPath fd buf =
    case mPath of
        Nothing ->
            throwErrnoIfMinus1_ loc $ ioctl fd (#const FS_IOC_FIEMAP) buf
        Just path ->
            throwErrnoPathIfMinus1_ loc path $ ioctl fd (#const FS_IOC_FIEMAP) buf
{-# INLINE ioctl_fiemap #-}

foreign import ccall unsafe "string.h memset"
    c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)

memset :: Ptr a -> Word8 -> CSize -> IO ()
memset p b l = void $ c_memset p (fromIntegral b) l
