#define BTRFS_RAW_PATHS 0
{- |
Module      : System.Linux.Btrfs

Stability   : provisional
Portability : non-portable (requires Linux)

Most functions in this module come in two flavors: one that operates on
file descriptors and another one that operates on file paths. The former
can be distinguished by the @Fd@ suffix in their names.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#if BTRFS_RAW_PATHS
##define FILEPATH RawFilePath
module System.Linux.Btrfs.ByteString
{-# DEPRECATED "This module is deprecated and will be removed in a\
 future version of this library. Please leave a comment on\
 https://github.com/redneb/hs-btrfs/issues/5 if you think that is\
 should not be removed." #-}
#else
##define FILEPATH FilePath
module System.Linux.Btrfs
#endif
    (
    -- * Basic types
      FileSize, ObjectType, ObjectId, InodeNum, SubvolId
    , CompressionType, compressNone, compressZlib, compressLZO, compressZstd
    -- * File cloning/deduplication
    , cloneFd, clone, cloneNew
    , cloneRangeFd, cloneRange
    , CloneResult(..)
    , cloneRangeIfSameFd, cloneRangeIfSame
    -- * Subvolumes and snapshots
    , createSubvol
    , destroySubvol
    , snapshotFd, snapshot
    , getSubvolReadOnlyFd, getSubvolReadOnly
    , setSubvolReadOnlyFd, setSubvolReadOnly
    , getSubvolFd, getSubvol
    , lookupSubvolFd, lookupSubvol
    , resolveSubvolFd, resolveSubvol
    , rootSubvol
    , listSubvolsFd, listSubvols
    , listSubvolPathsFd, listSubvolPaths
    , childSubvolsFd, childSubvols
    , childSubvolPathsFd, childSubvolPaths
    , SubvolInfo(..)
    , getSubvolInfoFd, getSubvolInfo
    , getSubvolByUuidFd, getSubvolByUuid
    , getSubvolByReceivedUuidFd, getSubvolByReceivedUuid
    , getDefaultSubvolFd, getDefaultSubvol
    , setDefaultSubvolFd, setDefaultSubvol
    -- * Defrag
    -- | There is a limitation in the kernel whereby a defrag operation
    -- will be silently aborted when the calling process receives any
    -- signal. This does not play well with GHC's rts which in some
    -- cases uses signals as a way to preempt haskell threads. So in order
    -- to use 'defrag' or 'defragRange', you must compile your program with
    -- GHC >=8.2 and the use the threaded runtime which does not use
    -- signals anymore. Alternatively, for older versions of GHC, you can
    -- use something like the @withRTSSignalsBlocked@ function from
    -- <http://www.serpentine.com/blog/2010/09/04/dealing-with-fragile-c-libraries-e-g-mysql-from-haskell/ here>.
    , defragFd, defrag
    , DefragRangeArgs(..), defaultDefragRangeArgs
    , defragRangeFd, defragRange
    -- * File system info
    , FSInfo
    , fsiDeviceCount, fsiUuid, fsiNodeSize, fsiSectorSize, fsiCloneAlignment
    , getFSInfoFd, getFSInfo
    -- * File system label
    , getFSLabelFd, getFSLabel
    , setFSLabelFd, setFSLabel
    -- * Sync
    , syncFd, sync
    , startSyncFd, startSync
    , waitSyncFd, waitSync
    -- * Inspect internal
    , resolveLogicalFd, resolveLogical
    , resolveInodeFd, resolveInode
    , lookupInodeFd, lookupInode
    -- * Miscellaneous
    , getFileNoCOWFd, getFileNoCOW
    , setFileNoCOWFd, setFileNoCOW
    -- * Tree search
    -- | Low-level API for tree search using the @BTRFS_IOC_TREE_SEARCH@
    -- @ioctl@.
    , SearchKey(..)
    , defaultSearchKey
    , SearchHeader(..)
    , treeSearchFd, treeSearch
    , treeSearchListFd, treeSearchList
    , findFirstItemFd, findFirstItem
    ) where

import System.Posix.Types
import System.Posix.IO hiding (openFd, sync)
import System.Posix.Files
import System.IO.Error
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock (UTCTime)
import Data.Monoid
import Prelude

import Foreign
import Foreign.C.Types
import Foreign.C.String (CStringLen)
import Foreign.C.Error

import System.Linux.Btrfs.FilePathLike
import Data.Word.Endian
import System.Linux.Btrfs.Time
import System.Linux.Btrfs.UUID
import System.Linux.Capabilities

#include <btrfs/ioctl.h>
#include <btrfs/ctree.h>

#include <linux/fs.h>

foreign import ccall safe
    ioctl :: Fd -> CULong -> Ptr a -> IO CInt
foreign import ccall unsafe "ioctl"
    ioctl_fast :: Fd -> CULong -> Ptr a -> IO CInt

type FileSize = Word64

type ObjectType = Word8

type ObjectId = Word64

type InodeNum = ObjectId

type SubvolId = ObjectId

newtype CompressionType = CompressionType Word32
    deriving Eq

instance Show CompressionType where
    show t
        | t == compressNone = "compressNone"
        | t == compressZlib = "compressZlib"
        | t == compressLZO  = "compressLZO"
        | t == compressZstd = "compressZstd"
        | otherwise = error "unknown compression type"

compressNone, compressZlib, compressLZO, compressZstd :: CompressionType
compressNone = CompressionType (#const BTRFS_COMPRESS_NONE)
compressZlib = CompressionType (#const BTRFS_COMPRESS_ZLIB)
compressLZO  = CompressionType (#const BTRFS_COMPRESS_LZO)
compressZstd = CompressionType (#const BTRFS_COMPRESS_ZSTD)

--------------------------------------------------------------------------------

cloneFd :: Fd -> Fd -> IO ()
cloneFd srcFd dstFd =
    throwErrnoIfMinus1_ "cloneFd" $
        ioctl_fast dstFd (#const BTRFS_IOC_CLONE) srcFdP
  where
    srcFdP = intPtrToPtr (fromIntegral srcFd)

-- | Clone an entire file to an existing file.
--
-- Note: calls the @BTRFS_IOC_CLONE@/@FICLONE@ @ioctl@.
clone
    :: FILEPATH -- ^ The source file.
    -> FILEPATH -- ^ The destination file.
    -> IO ()
clone srcPath dstPath =
    withFd srcPath ReadOnly $ \srcFd ->
    withFd dstPath WriteOnly $ \dstFd ->
        cloneFd srcFd dstFd

-- | Like 'clone' except that it will create or truncate the destination
-- file if necessary. This is similar to @cp --reflink=always@.
--
-- Note: calls the @BTRFS_IOC_CLONE@/@FICLONE@ @ioctl@.
cloneNew :: FILEPATH -> FILEPATH -> IO ()
cloneNew srcPath dstPath =
    withFd srcPath ReadOnly $ \srcFd -> do
        stat <- getFdStatus srcFd
        let mode = fileMode stat
        bracket (openFd dstPath WriteOnly defaultFileFlags {trunc = True, creat = Just mode}) closeFd $ \dstFd ->
            cloneFd srcFd dstFd

cloneRangeFd :: Fd -> FileSize -> FileSize -> Fd -> FileSize -> IO ()
cloneRangeFd srcFd srcOff srcLen dstFd dstOff =
    allocaBytesZero (#size struct btrfs_ioctl_clone_range_args) $ \cra -> do
        (#poke struct btrfs_ioctl_clone_range_args, src_fd     ) cra (fromIntegral srcFd :: Int64)
        (#poke struct btrfs_ioctl_clone_range_args, src_offset ) cra (srcOff :: Word64)
        (#poke struct btrfs_ioctl_clone_range_args, src_length ) cra (srcLen :: Word64)
        (#poke struct btrfs_ioctl_clone_range_args, dest_offset) cra (dstOff :: Word64)
        throwErrnoIfMinus1_ "cloneRangeFd" $
            ioctl_fast dstFd (#const BTRFS_IOC_CLONE_RANGE) cra

-- | Clones a range of bytes from a file to another file. All ranges must
-- be block-aligned (the block size can be obtained using 'getFSInfo' and
-- 'fsiCloneAlignment').
--
-- Note: calls the @BTRFS_IOC_CLONE_RANGE@/@FICLONERANGE@ @ioctl@.
cloneRange
    :: FILEPATH -- ^ The source file.
    -> FileSize -- ^ The offset within the source file.
    -> FileSize -- ^ The length of the range. A length of 0 selects the range
                -- from the source offset to the end.
    -> FILEPATH -- ^ The destination file.
    -> FileSize -- ^ The offset within the destination file.
    -> IO ()
cloneRange srcPath srcOff srcLen dstPath dstOff =
    withFd srcPath ReadOnly $ \srcFd ->
    withFd dstPath WriteOnly $ \dstFd ->
        cloneRangeFd srcFd srcOff srcLen dstFd dstOff

#ifdef BTRFS_IOC_FILE_EXTENT_SAME
data SameExtentInfoIn = SameExtentInfoIn
    Fd       -- file descriptor (stored as Int64)
    FileSize -- offset

instance Storable SameExtentInfoIn where
    sizeOf _ = (#size struct btrfs_ioctl_same_extent_info)
    alignment _ = alignment (undefined :: CInt)
    poke ptr (SameExtentInfoIn dstFd dstOff) = do
        memset ptr 0 (#size struct btrfs_ioctl_same_extent_info)
        let dstFd' = fromIntegral dstFd :: Int64
        (#poke struct btrfs_ioctl_same_extent_info, fd) ptr dstFd'
        (#poke struct btrfs_ioctl_same_extent_info, logical_offset) ptr dstOff
    peek _ = error "not implemented"

data SameExtentInfoOut = SameExtentInfoOut
    Int32    -- status
    FileSize -- bytes deduped

instance Storable SameExtentInfoOut where
    sizeOf _ = (#size struct btrfs_ioctl_same_extent_info)
    alignment _ = alignment (undefined :: CInt)
    poke _ _ = error "not implemented"
    peek ptr = do
        status <- (#peek struct btrfs_ioctl_same_extent_info, status       ) ptr
        bytes  <- (#peek struct btrfs_ioctl_same_extent_info, bytes_deduped) ptr
        return (SameExtentInfoOut status bytes)
#endif

-- | The result of a 'cloneRangeIfSame' operation.
data CloneResult
    = CRError IOError    -- ^ Cloning failed because of an error.
    | CRDataDiffers      -- ^ No cloning was performed because the contents
                         -- of the source and the destination file differ.
    | CRSuccess FileSize -- ^ Cloning succeeded, the returned integer
                         -- indicates the number of bytes that were
                         -- deduped.
    deriving (Show, Eq)

cloneRangeIfSameFd :: Fd -> FileSize -> FileSize -> [(Fd, FileSize)] -> IO [CloneResult]
#ifndef BTRFS_IOC_FILE_EXTENT_SAME
cloneRangeIfSameFd _ _ _ _ =
    error "System.Linux.Btrfs.cloneRangeIfSameFd: not supported"
#else
cloneRangeIfSameFd srcFd srcOff srcLen dsts = do
    unless (dstCount <= maxCount) $
        ioError $ flip ioeSetErrorString ("too many destination files (more than " ++
                                          show maxCount ++ ")")
                $ mkIOError illegalOperationErrorType "cloneRangeIfSameFd" Nothing Nothing
    allocaBytes saSize $ \sa -> do
        memset sa 0 (#size struct btrfs_ioctl_same_args)
        (#poke struct btrfs_ioctl_same_args, logical_offset) sa srcOff
        (#poke struct btrfs_ioctl_same_args, length        ) sa srcLen
        (#poke struct btrfs_ioctl_same_args, dest_count    ) sa dstCount'
        let info = (#ptr struct btrfs_ioctl_same_args, info) sa
        pokeArray info (map (uncurry SameExtentInfoIn) dsts)
        throwErrnoIfMinus1_ "cloneRangeIfSameFd" $
            ioctl srcFd (#const BTRFS_IOC_FILE_EXTENT_SAME) sa
        res <- peekArray dstCount info
        return $ flip map res $ \(SameExtentInfoOut status bytes) ->
            if status == 0 then
                CRSuccess bytes
            else if status == (#const BTRFS_SAME_DATA_DIFFERS) then
                CRDataDiffers
            else if status <= 0 then
                CRError $ errnoToIOError "cloneRangeIfSameFd"
                                         (Errno $ fromIntegral $ -status)
                                         Nothing Nothing
            else
                error $ "unknown status value (" ++ show status ++ ")"
  where
    saSize = (#size struct btrfs_ioctl_same_args) +
             dstCount * (#size struct btrfs_ioctl_same_extent_info)
    dstCount = length dsts
    dstCount' = fromIntegral dstCount :: Word64
    maxCount = fromIntegral (maxBound :: Word16)
#endif

-- | Similar to 'cloneRange' except that it performs the cloning only if
-- the data ranges contain identical data.
-- Additionally, it accepts multiple destination files. The same thing can
-- be accomplished with 'cloneRange' in conjunction with file locking but
-- this function uses in-kernel locking to guarantee that the deduplicated
-- data is identical at the time of the operation. On the other hand, this
-- function will not clone arbitrarily large ranges; the kernel has an upper
-- limit for the length and if cloning bigger ranges is desired then it
-- has to be called multiple times. Note that cloning may succeed for some
-- of the destination files and fail for others. Because of that, this
-- function returns a list of outcomes, one for each destination file, and
-- no exceptions will be raised for the failed files.
--
-- Note: calls the @BTRFS_IOC_FILE_EXTENT_SAME@/@FIDEDUPERANGE@ @ioctl@.
--
-- /Requires Linux 3.12 or later./
cloneRangeIfSame
    :: FILEPATH               -- ^ The source file.
    -> FileSize               -- ^ The offset within the source file.
    -> FileSize               -- ^ The length of the range.
    -> [(FILEPATH, FileSize)] -- ^ The destination files and corresponding offsets.
    -> IO [CloneResult]
cloneRangeIfSame srcPath srcOff srcLen dstsP0 = do
    -- we check if the process has the CAP_SYS_ADMIN capability
    -- if it does we use ReadOnly to open the destination files
    -- this allows privileged users to operate on readonly snapshots
    isAdmin <- hasSysAdminCap
    let openMode = if isAdmin then ReadOnly else WriteOnly
    withFd srcPath ReadOnly $ \srcFd ->
        loop srcFd openMode (reverse dstsP0) []
  where
    loop srcFd openMode ((dstPath, dstOff) : dstsP) dsts =
        withFd dstPath openMode $ \fd ->
            loop srcFd openMode dstsP ((fd, dstOff) : dsts)
    loop srcFd _ [] dsts =
        cloneRangeIfSameFd srcFd srcOff srcLen dsts

--------------------------------------------------------------------------------

simpleSubvolOp :: String -> FILEPATH -> CULong -> IO ()
simpleSubvolOp loc path req =
    withSplitPathOpenParent loc (#const BTRFS_PATH_NAME_MAX) path $ \(cName, l) dirFd ->
        allocaBytesZero (#size struct btrfs_ioctl_vol_args) $ \iva -> do
            let ivaName = (#ptr struct btrfs_ioctl_vol_args, name) iva
            copyBytes ivaName cName l
            throwErrnoIfMinus1_ loc $
                ioctl dirFd req iva

-- | Create an (initially) empty new subvolume.
--
-- Note: calls the @BTRFS_IOC_SUBVOL_CREATE@ @ioctl@.
createSubvol :: FILEPATH -> IO ()
createSubvol path =
    simpleSubvolOp "createSubvol" path (#const BTRFS_IOC_SUBVOL_CREATE)

-- | Destroy (delete) a subvolume. The directory that corresponds to the
-- subvolume is removed asynchronously. As a result, the subvolume may
-- appear again after a crash. If this is not acceptable, call 'startSync'
-- followed by a 'waitSync', after the @destroySubvol@ call.
--
-- Note: calls the @BTRFS_IOC_SNAP_DESTROY@ @ioctl@.
destroySubvol :: FILEPATH -> IO ()
destroySubvol path =
    simpleSubvolOp "destroySubvol" path (#const BTRFS_IOC_SNAP_DESTROY)

snapshotFd :: Fd -> FILEPATH -> Bool -> IO ()
snapshotFd srcFd dstPath readOnly =
    withSplitPathOpenParent "snapshotFd" (#const BTRFS_SUBVOL_NAME_MAX) dstPath $ \(cName, l) dirFd ->
        allocaBytesZero (#size struct btrfs_ioctl_vol_args_v2) $ \iva -> do
            let ivaName = (#ptr struct btrfs_ioctl_vol_args_v2, name) iva
            copyBytes ivaName cName l
            (#poke struct btrfs_ioctl_vol_args_v2, fd) iva (fromIntegral srcFd :: Int64)
            when readOnly $
                setFlags ((#ptr struct btrfs_ioctl_vol_args_v2, flags) iva)
                    ((#const BTRFS_SUBVOL_RDONLY) :: Word64)
            throwErrnoIfMinus1_ "snapshotFd" $
                ioctl dirFd (#const BTRFS_IOC_SNAP_CREATE_V2) iva

-- | Create a snapshot of an existing subvolume.
--
-- Note: calls the @BTRFS_IOC_SNAP_CREATE_V2@ @ioctl@.
snapshot
    :: FILEPATH -- ^ The source subvolume.
    -> FILEPATH -- ^ The destination subvolume (must not exist).
    -> Bool     -- ^ Create a read-only snapshot?
    -> IO ()
snapshot srcPath dstPath readOnly =
    withFd srcPath ReadOnly $ \srcFd ->
        snapshotFd srcFd dstPath readOnly

getSubvolReadOnlyFd :: Fd -> IO Bool
getSubvolReadOnlyFd fd =
    alloca $ \flagsPtr -> do
        throwErrnoIfMinus1_ "getSubvolReadOnlyFd" $
            ioctl fd (#const BTRFS_IOC_SUBVOL_GETFLAGS) flagsPtr
        flags <- peek flagsPtr :: IO Word64
        return (flags .&. (#const BTRFS_SUBVOL_RDONLY) /= 0)

-- | Is the subvolume read-only?
--
-- Note: calls the @BTRFS_IOC_SUBVOL_GETFLAGS@ @ioctl@.
getSubvolReadOnly :: FILEPATH -> IO Bool
getSubvolReadOnly path = withFd path ReadOnly getSubvolReadOnlyFd

setSubvolReadOnlyFd :: Fd -> Bool -> IO ()
setSubvolReadOnlyFd fd readOnly =
    alloca $ \flagsPtr -> do
        throwErrnoIfMinus1_ "setSubvolReadOnlyFd" $
            ioctl fd (#const BTRFS_IOC_SUBVOL_GETFLAGS) flagsPtr
        if readOnly then
            setFlags flagsPtr ((#const BTRFS_SUBVOL_RDONLY) :: Word64)
        else
            clearFlags flagsPtr ((#const BTRFS_SUBVOL_RDONLY) :: Word64)
        throwErrnoIfMinus1_ "setSubvolReadOnlyFd" $
            ioctl fd (#const BTRFS_IOC_SUBVOL_SETFLAGS) flagsPtr

-- | Make a subvolume read-only (or read-write).
--
-- Note: calls the @BTRFS_IOC_SUBVOL_GETFLAGS@ and
-- @BTRFS_IOC_SUBVOL_SETFLAGS@ @ioctl@s.
setSubvolReadOnly :: FILEPATH -> Bool -> IO ()
setSubvolReadOnly path readOnly =
    withFd path ReadOnly $ \fd -> setSubvolReadOnlyFd fd readOnly

getSubvolFd :: Fd -> IO SubvolId
getSubvolFd fd = do
    (subvolId, _) <- lookupInodeFd fd 0 (#const BTRFS_FIRST_FREE_OBJECTID)
    return subvolId

-- | Find the id of the subvolume where the given file resides. This is
-- merely a wrapper around 'lookupInode' provided for convenience.
getSubvol :: FILEPATH -> IO SubvolId
getSubvol path = withFd path ReadOnly getSubvolFd

lookupSubvolFd :: Fd -> SubvolId -> IO (SubvolId, InodeNum, FILEPATH)
lookupSubvolFd fd subvolId = do
    let sk = defaultSearchKey
            { skTreeId      = (#const BTRFS_ROOT_TREE_OBJECTID)
            , skMinObjectId = subvolId
            , skMaxObjectId = subvolId
            , skMinType     = (#const BTRFS_ROOT_BACKREF_KEY)
            , skMaxType     = (#const BTRFS_ROOT_BACKREF_KEY)
            }
    findFirstItemFd fd sk $ \sh rr -> do
        (dirId, name) <- peekRootRef rr
        return (shOffset sh, dirId, name)

-- | Given the id of a subvolume, find the id of the parent subvolume, the
-- inode number of the directory containing it, and its name. This is
-- a wrapper around 'treeSearch'.
lookupSubvol
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> SubvolId -- ^ The id of the subvolume.
    -> IO (SubvolId, InodeNum, FILEPATH)
lookupSubvol path subvolId =
    withFd path ReadOnly $ \fd ->
        lookupSubvolFd fd subvolId

resolveSubvolFd :: Fd -> SubvolId -> IO FILEPATH
resolveSubvolFd fd subvolId
    | subvolId == rootSubvol = return mempty
    | otherwise = do
        (parentId, dirId, name) <- lookupSubvolFd fd subvolId
        parentPath <- resolveSubvolFd fd parentId
        if dirId == (#const BTRFS_FIRST_FREE_OBJECTID) then
            return (parentPath </> name)
        else do
            (_, dirName) <- lookupInodeFd fd parentId dirId
            return (parentPath </> dirName </> name)

-- | Given the id of a subvolume, find its path relative to the root of the
-- volume. This function calls 'lookupSubvol' recursively.
resolveSubvol
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> SubvolId -- ^ The id of the subvolume.
    -> IO FILEPATH
resolveSubvol path subvolId =
    withFd path ReadOnly $ \fd ->
        resolveSubvolFd fd subvolId

-- | The id the root subvolume.
rootSubvol :: SubvolId
rootSubvol = (#const BTRFS_FS_TREE_OBJECTID)

listSubvolsFd :: Fd -> IO [(SubvolId, SubvolId, InodeNum, FILEPATH)]
listSubvolsFd fd = do
    let sk = defaultSearchKey
            { skTreeId      = (#const BTRFS_ROOT_TREE_OBJECTID)
            , skMinObjectId = (#const BTRFS_FIRST_FREE_OBJECTID)
            , skMaxObjectId = (#const BTRFS_LAST_FREE_OBJECTID)
            , skMinType     = (#const BTRFS_ROOT_BACKREF_KEY)
            , skMaxType     = (#const BTRFS_ROOT_BACKREF_KEY)
            }
    treeSearchListFd fd sk unpack
  where
    unpack sh rr
        | shType sh /= (#const BTRFS_ROOT_BACKREF_KEY) =
            return Nothing
        | otherwise = do
            (dirId, name) <- peekRootRef rr
            return $ Just (shObjectId sh, shOffset sh, dirId, name)

-- | Find all subvolumes of the given volume. For each subvolume found, it
-- returns: its id, the id of its parent subvolume, the inode number of the
-- directory containing it, and its name. This is a wrapper around
-- 'treeSearch'.
listSubvols :: FILEPATH -> IO [(SubvolId, SubvolId, InodeNum, FILEPATH)]
listSubvols path =
    withFd path ReadOnly listSubvolsFd

listSubvolPathsFd :: Fd -> IO [(SubvolId, SubvolId, FILEPATH)]
listSubvolPathsFd fd = do
    subvols <- listSubvolsFd fd
    forM subvols $ \(subvolId, parentId, _, _) -> do
        path <- resolveSubvolFd fd subvolId
        return (subvolId, parentId, path)

-- | Find all subvolumes of the given volume. For each subvolume found, it
-- returns: its id, the id of its parent subvolume, and its path relative
-- to the root of the volume. This is a wrapper around 'treeSearch' and
-- 'resolveSubvol'.
listSubvolPaths :: FILEPATH -> IO [(SubvolId, SubvolId, FILEPATH)]
listSubvolPaths path =
    withFd path ReadOnly listSubvolPathsFd

childSubvolsFd :: Fd -> SubvolId -> IO [(SubvolId, InodeNum, FILEPATH)]
childSubvolsFd fd subvolId = do
    let sk = defaultSearchKey
            { skTreeId      = (#const BTRFS_ROOT_TREE_OBJECTID)
            , skMinObjectId = subvolId
            , skMaxObjectId = subvolId
            , skMinType     = (#const BTRFS_ROOT_REF_KEY)
            , skMaxType     = (#const BTRFS_ROOT_REF_KEY)
            }
    treeSearchListFd fd sk unpack
  where
    unpack sh rr
        | shType sh /= (#const BTRFS_ROOT_REF_KEY) =
            return Nothing
        | otherwise = do
            (dirId, name) <- peekRootRef rr
            return $ Just (shOffset sh, dirId, name)

-- | Find all child subvolumes of the given subvolume. For each child,
-- returns its id, the inode number of the directory containing it, and its
-- name. This is a wrapper around 'treeSearch'.
childSubvols
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> SubvolId -- ^ The id of the subvolume.
    -> IO [(SubvolId, InodeNum, FILEPATH)]
childSubvols path subvolId =
    withFd path ReadOnly $ \fd ->
        childSubvolsFd fd subvolId

childSubvolPathsFd :: Fd -> SubvolId -> IO [(SubvolId, FILEPATH)]
childSubvolPathsFd fd subvolId = do
    childs <- childSubvolsFd fd subvolId
    forM childs $ \(childId, dirId, name) ->
        if dirId == (#const BTRFS_FIRST_FREE_OBJECTID) then
            return (childId, name)
        else do
            (_, dirName) <- lookupInodeFd fd subvolId dirId
            return (childId, dirName </> name)

-- | Find all child subvolumes of the given subvolume. For each child,
-- returns its id and its path relative to the root of the parent.
-- This is a wrapper around 'treeSearch' and 'lookupInode'.
childSubvolPaths
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> SubvolId -- ^ The id of the subvolume.
    -> IO [(SubvolId, FILEPATH)]
childSubvolPaths path subvolId =
    withFd path ReadOnly $ \fd ->
        childSubvolPathsFd fd subvolId

-- | Information about a subvolume.
data SubvolInfo = SubvolInfo
    { siGeneration :: Word64
        -- ^ The generation when the subvolume was last modified.
    , siLastSnapshot :: Maybe Word64
        -- ^ The generation when the most recent snapshot of this subvolume was taken.
    , siParSnapGen :: Maybe Word64
        -- ^ The generation of the snapshot parent at the time when the snapshot
        -- was taken. Defined if only if this is a snapshot.
    , siReadOnly :: Bool
        -- ^ Is this a read-only subvolume?
    , siUuid :: Maybe UUID
        -- ^ The UUID of the subvolume.
    , siPUuid :: Maybe UUID
        -- ^ The UUID of the snapshot parent.
    , siReceivedUuid :: Maybe UUID
        -- ^ The UUID of the source subvolume that this subvolume was
        -- received from. This is always defined for received subvolumes.
    , siCTransId :: Maybe Word64
        -- ^ The generation when an inode was last modified.
    , siOTransId :: Maybe Word64
        -- ^ The generation when the subvolume was created.
    , siSTransId :: Maybe Word64
        -- ^ The generation of the source subvolume that this subvolume was
        -- received from. This is always defined for received subvolumes.
    , siRTransId :: Maybe Word64
        -- ^ The generation when the subvolume was received. This is always
        -- defined for received subvolumes.
    , siCTime :: Maybe UTCTime
        -- ^ The time when an inode was last modified.
    , siOTime :: Maybe UTCTime
        -- ^ The time when the subvolume was created.
    , siSTime :: Maybe UTCTime
        -- ^ The timestamp that corresponds to 'siSTransId'.
    , siRTime :: Maybe UTCTime
        -- ^ The time when the subvolume was received. This is always
        -- defined for received subvolumes.
    }
  deriving (Show, Eq)

getSubvolInfoFd :: Fd -> SubvolId -> IO SubvolInfo
getSubvolInfoFd fd subvolId
    | subvolId /= rootSubvol &&
        (subvolId < (#const BTRFS_FIRST_FREE_OBJECTID) || subvolId > (#const BTRFS_LAST_FREE_OBJECTID)) =
          ioError $ mkIOError doesNotExistErrorType
                              "getSubvolInfoFd"
                              Nothing Nothing
    | otherwise = do
        let sk = defaultSearchKey
                { skTreeId      = (#const BTRFS_ROOT_TREE_OBJECTID)
                , skMinObjectId = subvolId
                , skMaxObjectId = subvolId
                , skMinType     = (#const BTRFS_ROOT_ITEM_KEY)
                , skMaxType     = (#const BTRFS_ROOT_ITEM_KEY)
                }
        findFirstItemFd fd sk unpack
  where
    unpack sh ri = do
        LE64 generation <- (#peek struct btrfs_root_item, generation) ri
        LE64 lastSnapshot <- (#peek struct btrfs_root_item, last_snapshot) ri
        LE64 flags <- (#peek struct btrfs_root_item, flags) ri
        LE64 generationV2 <- (#peek struct btrfs_root_item, generation_v2) ri
        let nv2 = generationV2 < generation -- not version 2
        uuid <- (#peek struct btrfs_root_item, uuid) ri :: IO UUID
        pUuid <- (#peek struct btrfs_root_item, parent_uuid) ri :: IO UUID
        receivedUuid <- (#peek struct btrfs_root_item, received_uuid) ri :: IO UUID
        LE64 cTransId <- (#peek struct btrfs_root_item, ctransid) ri
        LE64 oTransId <- (#peek struct btrfs_root_item, otransid) ri
        LE64 sTransId <- (#peek struct btrfs_root_item, stransid) ri
        LE64 rTransId <- (#peek struct btrfs_root_item, rtransid) ri
        BtrfsTime cTime <- (#peek struct btrfs_root_item, ctime) ri
        BtrfsTime oTime <- (#peek struct btrfs_root_item, otime) ri
        BtrfsTime sTime <- (#peek struct btrfs_root_item, stime) ri
        BtrfsTime rTime <- (#peek struct btrfs_root_item, rtime) ri
        return $ SubvolInfo
            { siGeneration = generation
            , siLastSnapshot = nothingIf (lastSnapshot == 0) $ lastSnapshot
            , siParSnapGen = nothingIf (shOffset sh == 0) $ shOffset sh
            , siReadOnly = flags .&. (#const BTRFS_SUBVOL_RDONLY) /= 0
            , siUuid = nothingIf nv2 uuid
            , siPUuid = nothingIf (nv2 || shOffset sh == 0) pUuid
            , siReceivedUuid = nothingIf (nv2 || sTransId == 0) receivedUuid
            , siCTransId = nothingIf nv2 cTransId
            , siOTransId = nothingIf (nv2 || oTransId == 0) oTransId
            , siSTransId = nothingIf (nv2 || sTransId == 0) sTransId
            , siRTransId = nothingIf (nv2 || rTransId == 0) rTransId
            , siCTime = nothingIf nv2 cTime
            , siOTime = nothingIf (nv2 || oTransId == 0) oTime
            , siSTime = nothingIf (nv2 || sTransId == 0) sTime
            , siRTime = nothingIf (nv2 || rTransId == 0) rTime
            }

-- | Retrieve information about a subvolume. This is a wrapper around 'treeSearch'.
getSubvolInfo
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> SubvolId -- ^ The id of the subvolume.
    -> IO SubvolInfo
getSubvolInfo path subvolId =
    withFd path ReadOnly $ \fd ->
        getSubvolInfoFd fd subvolId

searchByUuidFd :: ObjectType -> Fd -> UUID -> IO SubvolId
searchByUuidFd typ fd (UUID hBE lBE) = do
    let sk = defaultSearchKey
            { skTreeId      = (#const BTRFS_UUID_TREE_OBJECTID)
            , skMinObjectId = hLE
            , skMaxObjectId = hLE
            , skMinType     = typ
            , skMaxType     = typ
            , skMinOffset   = lLE
            , skMaxOffset   = lLE
            }
    findFirstItemFd fd sk $ \_ ptr ->
        liftM fromLE64 $ peek ptr
  where
    -- UUID is stored as two big-endian integers
    -- but in the UUID tree, little-endian integers are used
    lLE = invert64 lBE
    hLE = invert64 hBE

getSubvolByUuidFd :: Fd -> UUID -> IO SubvolId
getSubvolByUuidFd =
    searchByUuidFd (#const BTRFS_UUID_KEY_SUBVOL)

-- | Find the id of a subvolume, given its UUID. This is a wrapper around
-- 'treeSearch'.
--
-- /Requires Linux 3.12 or later./
getSubvolByUuid
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> UUID     -- ^ The UUID of the subvolume.
    -> IO SubvolId
getSubvolByUuid path uuid =
    withFd path ReadOnly $ \fd ->
        getSubvolByUuidFd fd uuid

getSubvolByReceivedUuidFd :: Fd -> UUID -> IO SubvolId
getSubvolByReceivedUuidFd =
    searchByUuidFd (#const BTRFS_UUID_KEY_RECEIVED_SUBVOL)

-- | Find the id of a subvolume, given its 'siReceivedUuid'. This is a
-- wrapper around 'treeSearch'.
--
-- /Requires Linux 3.12 or later./
getSubvolByReceivedUuid
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> UUID     -- ^ The 'siReceivedUuid' of the subvolume.
    -> IO SubvolId
getSubvolByReceivedUuid path uuid =
    withFd path ReadOnly $ \fd ->
        getSubvolByReceivedUuidFd fd uuid

getDefaultSubvolFd :: Fd -> IO SubvolId
getDefaultSubvolFd fd = do
    let sk = defaultSearchKey
            { skTreeId      = (#const BTRFS_ROOT_TREE_OBJECTID)
            , skMinObjectId = (#const BTRFS_ROOT_TREE_DIR_OBJECTID)
            , skMaxObjectId = (#const BTRFS_ROOT_TREE_DIR_OBJECTID)
            , skMinType     = (#const BTRFS_DIR_ITEM_KEY)
            , skMaxType     = (#const BTRFS_DIR_ITEM_KEY)
            }
    l <- treeSearchListFd fd sk $ \_ ptr -> do
        LE16 nameLen <- (#peek struct btrfs_dir_item, name_len) ptr
        let cName = ptr `plusPtr` (#size struct btrfs_dir_item)
        name <- peekFilePathLen (cName, fromIntegral nameLen)
        if name /= "default" then
            return Nothing
        else do
            let location = ptr `plusPtr` (#offset struct btrfs_dir_item, location)
            LE64 objectId <- (#peek struct btrfs_disk_key, objectid) location
            return (Just objectId)
    case l of
        [] -> ioError $ mkIOError doesNotExistErrorType "getDefaultSubvolFd" Nothing Nothing
        (objectId : _) -> return objectId

-- | Find the id of the default subvolume. This is a wrapper around
-- 'treeSearch'.
getDefaultSubvol
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> IO SubvolId
getDefaultSubvol path = withFd path ReadOnly getDefaultSubvolFd

setDefaultSubvolFd :: Fd -> ObjectId -> IO ()
setDefaultSubvolFd fd objectId = do
    alloca $ \ptr -> do
        poke ptr objectId
        throwErrnoIfMinus1_ "setDefaultSubvolFd" $
            ioctl fd (#const BTRFS_IOC_DEFAULT_SUBVOL) ptr

-- | Set the default subvolume.
--
-- Note: calls the @BTRFS_IOC_DEFAULT_SUBVOL@ @ioctl@.
setDefaultSubvol
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> SubvolId -- ^ The id of the new default subvolume.
    -> IO ()
setDefaultSubvol path subvolId =
    withFd path ReadOnly $ \fd -> setDefaultSubvolFd fd subvolId

--------------------------------------------------------------------------------

defragFd :: Fd -> IO ()
defragFd fd =
    throwErrnoIfMinus1_ "defragFd" $
        ioctl fd (#const BTRFS_IOC_DEFRAG) nullPtr

-- | Defrag a single file.
--
-- Note: calls the @BTRFS_IOC_DEFRAG@ @ioctl@.
defrag :: FILEPATH -> IO ()
defrag path = withFd path ReadWrite defragFd

-- | Argument to the 'defragRange' operation.
data DefragRangeArgs = DefragRangeArgs
    { draStart :: FileSize
        -- ^ Beginning of the defrag range.
    , draLength :: FileSize
        -- ^ Number of bytes to defrag, use 'maxBound' to say all.
    , draExtentThreshold :: Word32
        -- ^ Any extent of size bigger or equal to this number will be
        -- considered already defragged. Use 0 for the kernel default.
    , draCompress :: CompressionType
        -- ^ Compress the file while defragmenting.
    , draFlush :: Bool
        -- ^ Flush data to disk immediately after defragmenting.
    }
  deriving (Show, Eq)

-- | Defaults for 'defragRange'. Selects the entire file, no compression,
-- and no flushing.
defaultDefragRangeArgs :: DefragRangeArgs
defaultDefragRangeArgs = DefragRangeArgs
    { draStart = 0
    , draLength = maxBound
    , draExtentThreshold = 0
    , draCompress = compressNone
    , draFlush = False
    }

defragRangeFd :: Fd -> DefragRangeArgs -> IO ()
defragRangeFd fd DefragRangeArgs{..} =
    allocaBytesZero (#size struct btrfs_ioctl_defrag_range_args) $ \args -> do
        (#poke struct btrfs_ioctl_defrag_range_args, start        ) args draStart
        (#poke struct btrfs_ioctl_defrag_range_args, len          ) args draLength
        (#poke struct btrfs_ioctl_defrag_range_args, flags        ) args flags
        (#poke struct btrfs_ioctl_defrag_range_args, extent_thresh) args draExtentThreshold
        (#poke struct btrfs_ioctl_defrag_range_args, compress_type) args comp_type
        throwErrnoIfMinus1_ "defragRangeFd" $
            ioctl fd (#const BTRFS_IOC_DEFRAG_RANGE) args
  where
    flags :: Word64
    flags = comp_flags .|. if draFlush then (#const BTRFS_DEFRAG_RANGE_START_IO) else 0
    comp_flags
        | draCompress == compressNone = 0
        | otherwise = (#const BTRFS_DEFRAG_RANGE_COMPRESS)
    CompressionType comp_type = draCompress

-- | Defrag a range within a single file.
--
-- Note: calls the @BTRFS_IOC_DEFRAG_RANGE@ @ioctl@.
defragRange :: FILEPATH -> DefragRangeArgs -> IO ()
defragRange path args =
    withFd path ReadWrite $ \fd ->
        defragRangeFd fd args

--------------------------------------------------------------------------------

-- | Information about a btrfs file system.
data FSInfo = FSInfo
    { fsiDeviceCount :: Word64
        -- ^ The number of devices in the file system.
    , fsiUuid :: UUID
        -- ^ The UUID of the file system.
    , fsiNodeSize :: FileSize
        -- ^ The tree block size in which metadata is stored.
    , fsiSectorSize :: FileSize
        -- ^ The minimum data block allocation unit.
    , fsiCloneAlignment :: FileSize
        -- ^ The size that is used for the alignment constraints of clone
        -- range operations.
    }
  deriving (Show, Eq)

getFSInfoFd :: Fd -> IO FSInfo
getFSInfoFd fd =
    allocaBytes (#size struct btrfs_ioctl_fs_info_args) $ \fsia -> do
        throwErrnoIfMinus1_ "getFSInfoFd" $
            ioctl_fast fd (#const BTRFS_IOC_FS_INFO) fsia
        nd <- (#peek struct btrfs_ioctl_fs_info_args, num_devices) fsia :: IO Word64
        uuid <- (#peek struct btrfs_ioctl_fs_info_args, fsid) fsia :: IO UUID
        ns <- (#peek struct btrfs_ioctl_fs_info_args, nodesize) fsia :: IO Word32
        ss <- (#peek struct btrfs_ioctl_fs_info_args, sectorsize) fsia :: IO Word32
        ca <- (#peek struct btrfs_ioctl_fs_info_args, clone_alignment) fsia :: IO Word32
        return FSInfo
            { fsiDeviceCount = nd
            , fsiUuid = uuid
            , fsiNodeSize = fromIntegral ns
            , fsiSectorSize = fromIntegral ss
            , fsiCloneAlignment = fromIntegral ca
            }

-- | Retrieve information about a btrfs file system.
--
-- Note: calls the @BTRFS_IOC_FS_INFO@ @ioctl@.
getFSInfo
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> IO FSInfo
getFSInfo path =
    withFd path ReadOnly getFSInfoFd

--------------------------------------------------------------------------------

getFSLabelFd :: Fd -> IO FILEPATH
getFSLabelFd fd =
    allocaBytesZero maxLabelSize $ \ptr -> do
        throwErrnoIfMinus1_ "getFSLabelFd" $
            ioctl_fast fd (#const BTRFS_IOC_GET_FSLABEL) ptr
        peekFilePath ptr

-- | Retrieve the label of a btrfs file system.
--
-- Note: calls the @BTRFS_IOC_GET_FSLABEL@ @ioctl@.
getFSLabel
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> IO FILEPATH
getFSLabel path =
    withFd path ReadOnly getFSLabelFd

setFSLabelFd :: Fd -> FILEPATH -> IO ()
setFSLabelFd fd label =
    withFilePathLen label $ \(ptr, len) ->
        allocaBytesZero maxLabelSize $ \buf -> do
            copyArray buf ptr (min len (maxLabelSize - 1))
            throwErrnoIfMinus1_ "setFSLabelFd" $
                ioctl fd (#const BTRFS_IOC_SET_FSLABEL) buf

-- | Set the label of a btrfs file system. Note that a label can be up to
-- 255 /bytes/ long. If the provided label is longer, it will be silently
-- truncated.
--
-- Note: calls the @BTRFS_IOC_SET_FSLABEL@ @ioctl@.
setFSLabel
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> FILEPATH -- ^ The new label.
    -> IO ()
setFSLabel path label =
    withFd path ReadOnly $ \fd ->
        setFSLabelFd fd label

maxLabelSize :: Int
maxLabelSize = (#const BTRFS_LABEL_SIZE)

--------------------------------------------------------------------------------

syncFd :: Fd -> IO ()
syncFd fd =
    throwErrnoIfMinus1_ "syncFd" $
        ioctl fd (#const BTRFS_IOC_SYNC) nullPtr

-- | Sync the file system identified by the supplied path.
-- The 'FilePath' can refer to any file in the file system.
--
-- Note: calls the @BTRFS_IOC_SYNC@ @ioctl@.
sync :: FILEPATH -> IO ()
sync path = withFd path ReadOnly syncFd

startSyncFd :: Fd -> IO ()
startSyncFd fd =
    throwErrnoIfMinus1_ "startSyncFd" $
        ioctl_fast fd (#const BTRFS_IOC_START_SYNC) nullPtr

-- | Initiate a sync for the file system identified by the supplied path.
--
-- Note: calls the @BTRFS_IOC_START_SYNC@ @ioctl@.
startSync :: FILEPATH -> IO ()
startSync path = withFd path ReadOnly startSyncFd

waitSyncFd :: Fd -> IO ()
waitSyncFd fd =
    throwErrnoIfMinus1_ "waitSyncFd" $
        ioctl fd (#const BTRFS_IOC_WAIT_SYNC) nullPtr

-- | Wait until the sync operation completes.
--
-- Note: calls the @BTRFS_IOC_WAIT_SYNC@ @ioctl@.
waitSync :: FILEPATH -> IO ()
waitSync path = withFd path ReadOnly waitSyncFd

--------------------------------------------------------------------------------

resolveLogicalFd :: Fd -> FileSize -> IO ([(InodeNum, FileSize, SubvolId)], Int)
resolveLogicalFd rootFd logical =
    allocaBytes inodesSize $ \inodes ->
    allocaBytesZero (#size struct btrfs_ioctl_logical_ino_args) $ \lia -> do
        (#poke struct btrfs_ioctl_logical_ino_args, logical) lia logical
        (#poke struct btrfs_ioctl_logical_ino_args, size   ) lia (fromIntegral inodesSize :: Word64)
        (#poke struct btrfs_ioctl_logical_ino_args, inodes ) lia inodes
        throwErrnoIfMinus1_ "resolveLogical" $ ioctl rootFd (#const BTRFS_IOC_LOGICAL_INO) lia
        elemMissed <- (#peek struct btrfs_data_container, elem_missed) inodes :: IO Word32
        count      <- (#peek struct btrfs_data_container, elem_cnt   ) inodes :: IO Word32
        let val = (#ptr struct btrfs_data_container, val) inodes :: Ptr Word64
        vals <- peekArray (fromIntegral count) val
        return (extractTriplets vals, fromIntegral elemMissed)
  where
    inodesSize = 64 * 1024 + (#size struct btrfs_data_container)
    extractTriplets (x1 : x2 : x3 : xs) = (x1, x2, x3) : extractTriplets xs
    extractTriplets [] = []
    extractTriplets _ = error "extractTriplets: The length of the list must be a multiple of 3"

-- | Given a physical offset, look for any inodes that this byte belongs
-- to. For each inode, it returns the inode number, the logical offset
-- (i.e. the offset within the inode), and the subvolume id. If a large
-- number of inodes is found, then not all of them will be returned by this
-- function. This is due to a current limitation in the kernel. The integer
-- returned along with list of inodes indicates the number of inodes found
-- but not included in the list.
--
-- Note: calls the @BTRFS_IOC_LOGICAL_INO@ @ioctl@.
resolveLogical
    :: FILEPATH -- ^ The mount point of the volume (or any file in that volume).
    -> FileSize -- ^ The physical byte offset in the underlying block device.
    -> IO ([(InodeNum, FileSize, SubvolId)], Int)
resolveLogical rootPath logical =
    withFd rootPath ReadOnly $ \fd ->
        resolveLogicalFd fd logical

resolveInodeFd :: Fd -> InodeNum -> IO ([FILEPATH], Int)
resolveInodeFd subvolFd inum =
    allocaBytes fspathSize $ \fspath ->
    allocaBytesZero (#size struct btrfs_ioctl_ino_path_args) $ \ipa -> do
        (#poke struct btrfs_ioctl_ino_path_args, inum  ) ipa inum
        (#poke struct btrfs_ioctl_ino_path_args, size  ) ipa (fromIntegral fspathSize :: Word64)
        (#poke struct btrfs_ioctl_ino_path_args, fspath) ipa fspath
        throwErrnoIfMinus1_ "resolveInode" $ ioctl subvolFd (#const BTRFS_IOC_INO_PATHS) ipa
        elemMissed <- (#peek struct btrfs_data_container, elem_missed) fspath :: IO Word32
        count      <- (#peek struct btrfs_data_container, elem_cnt   ) fspath :: IO Word32
        let val = (#ptr struct btrfs_data_container, val) fspath :: Ptr Word64
        vals <- peekArray (fromIntegral count) val
        paths <- mapM (peekFilePath . plusPtr val . fromIntegral) vals
        return (paths, fromIntegral elemMissed)
  where
    fspathSize = 2 * 1024 + (#size struct btrfs_data_container)

-- | Find the file path(s) given an inode number. Returns a list of file paths
-- and an integer indicating the number of paths found but not included in
-- the resulting list. This is because of a limitation in the kernel (it
-- will not return an arbitrarily large list). The paths returned are
-- relative to the root of the subvolume.
--
-- Note: calls the @BTRFS_IOC_INO_PATHS@ @ioctl@.
resolveInode
    :: FILEPATH -- ^ The path to the subvolume (or any file in that subvolume).
    -> InodeNum -- ^ The inode number.
    -> IO ([FILEPATH], Int)
resolveInode subvolPath inum =
    withFd subvolPath ReadOnly $ \subvolFd ->
        resolveInodeFd subvolFd inum

lookupInodeFd :: Fd -> SubvolId -> InodeNum -> IO (SubvolId, FILEPATH)
lookupInodeFd fd treeId inum =
    allocaBytesZero (#size struct btrfs_ioctl_ino_lookup_args) $ \ila -> do
        (#poke struct btrfs_ioctl_ino_lookup_args, treeid  ) ila treeId
        (#poke struct btrfs_ioctl_ino_lookup_args, objectid) ila inum
        throwErrnoIfMinus1_ "lookupInodeFd" $
            ioctl_fast fd (#const BTRFS_IOC_INO_LOOKUP) ila
        treeId' <- (#peek struct btrfs_ioctl_ino_lookup_args, treeid) ila :: IO Word64
        let cName = (#ptr struct btrfs_ioctl_ino_lookup_args, name) ila
        name <- peekFilePath cName
        return (treeId', dropTrailingSlash name)

-- | Find the path of a file given its inode number and the id of the
-- subvolume. If multiple files share the same inode number, only one of
-- them is returned. The id of the subvolume is also returned. This is
-- useful when 0 is given for the 'SubvolId' argument (also see
-- 'getSubvol' for this case).
--
-- Note: calls the @BTRFS_IOC_INO_LOOKUP@ @ioctl@.
lookupInode
    :: FILEPATH -- ^ The path to any file in the volume. The subvolume where
                -- this file resides is ignored unless no 'SubvolId' is
                -- provided (see below).
    -> SubvolId -- ^ The id of the subvolume. Can be 0. In that case, the
                -- subvolume of the 'FilePath' is used (see above).
    -> InodeNum -- ^ The inode number.
    -> IO (SubvolId, FILEPATH)
lookupInode path treeId inum =
    withFd path ReadOnly $ \fd -> lookupInodeFd fd treeId inum

--------------------------------------------------------------------------------

getFileNoCOWFd :: Fd -> IO Bool
getFileNoCOWFd fd =
    alloca $ \flagsPtr -> do
        throwErrnoIfMinus1_ "getFileNoCOWFd" $
            ioctl fd (#const FS_IOC_GETFLAGS) flagsPtr
        flags <- peek flagsPtr :: IO CUInt
        return (flags .&. (#const FS_NOCOW_FL) /= 0)

-- | Determine whether the NOCOW flag is enabled for the specified file.
--
-- Note: calls the @FS_IOC_GETFLAGS@ @ioctl@.
getFileNoCOW :: FILEPATH -> IO Bool
getFileNoCOW path =
    withFd path ReadOnly getFileNoCOWFd

setFileNoCOWFd :: Fd -> Bool -> IO ()
setFileNoCOWFd fd noCOW = do
    alloca $ \flagsPtr -> do
        throwErrnoIfMinus1_ "setFileNoCOWFd" $
            ioctl fd (#const FS_IOC_GETFLAGS) flagsPtr
        if noCOW then
            setFlags flagsPtr ((#const FS_NOCOW_FL) :: CUInt)
        else
            clearFlags flagsPtr ((#const FS_NOCOW_FL) :: CUInt)
        throwErrnoIfMinus1_ "setFileNoCOWFd" $
            ioctl fd (#const FS_IOC_SETFLAGS) flagsPtr

-- | Set or clear the NOCOW flag for the specified file. If the file is not
-- empty, this has no effect and no error will be reported.
--
-- Note: calls the @FS_IOC_GETFLAGS@ and @FS_IOC_GETFLAGS@ @ioctl@s.
setFileNoCOW :: FILEPATH -> Bool -> IO ()
setFileNoCOW path noCOW = do
    withFd path ReadOnly $ \fd ->
        setFileNoCOWFd fd noCOW

--------------------------------------------------------------------------------

data SearchKey = SearchKey
    { skTreeId      :: ObjectId
    , skMinObjectId :: ObjectId
    , skMinType     :: ObjectType
    , skMinOffset   :: Word64
    , skMaxObjectId :: ObjectId
    , skMaxType     :: ObjectType
    , skMaxOffset   :: Word64
    , skMinTransId  :: Word64
    , skMaxTransId  :: Word64
    }
  deriving (Show, Eq)

defaultSearchKey :: SearchKey
defaultSearchKey = SearchKey
    { skTreeId      = 0
    , skMinObjectId = minBound
    , skMinType     = minBound
    , skMinOffset   = minBound
    , skMaxObjectId = maxBound
    , skMaxType     = maxBound
    , skMaxOffset   = maxBound
    , skMinTransId  = minBound
    , skMaxTransId  = maxBound
    }

data SearchHeader = SearchHeader
    { shTransId  :: Word64
    , shObjectId :: ObjectId
    , shOffset   :: Word64
    , shType     :: ObjectType
    , shLen      :: Word32
    }
  deriving (Show, Eq)

treeSearchFd :: Fd -> SearchKey -> Int -> (SearchHeader -> Ptr i -> IO ()) -> IO ()
treeSearchFd fd sk maxItemCount0 callback =
    allocaBytesZero (#size struct btrfs_ioctl_search_args) $ \saPtr -> do
        let skPtr = (#ptr struct btrfs_ioctl_search_args, key) saPtr
        pokeSearchKey skPtr sk
        loopSingleSearch saPtr skPtr maxItemCount0
  where
    loopSingleSearch saPtr skPtr maxItemCount
        | maxItemCount <= 0 = return ()
        | otherwise = do
            let nrItems = fromIntegral (min 4096 maxItemCount) :: Word32
            (#poke struct btrfs_ioctl_search_key, nr_items) skPtr nrItems
            throwErrnoIfMinus1_ "treeSearchFd" $
                ioctl fd (#const BTRFS_IOC_TREE_SEARCH) saPtr
            itemsFound <- (#peek struct btrfs_ioctl_search_key, nr_items) skPtr :: IO Word32
            when (itemsFound > 0) $ do
                let shPtr = (#ptr struct btrfs_ioctl_search_args, buf) saPtr
                lastSh <- loopItems shPtr itemsFound
                case nextKey (shObjectId lastSh, shType lastSh, shOffset lastSh) of
                    Nothing -> return ()
                    Just (objectId, iType, offset) -> do
                        (#poke struct btrfs_ioctl_search_key, min_objectid) skPtr objectId
                        (#poke struct btrfs_ioctl_search_key, min_type    ) skPtr (fromIntegral iType :: Word32)
                        (#poke struct btrfs_ioctl_search_key, min_offset  ) skPtr offset
                        loopSingleSearch saPtr skPtr (maxItemCount - fromIntegral itemsFound)
    -- itemsFound must be at least 1
    loopItems shPtr itemsFound = do
        (sh, itemPtr) <- peekSearchItem shPtr
        callback sh itemPtr
        if itemsFound <= 1 then
            return sh
        else do
            let shPtr' = itemPtr `plusPtr` fromIntegral (shLen sh)
            loopItems shPtr' (itemsFound - 1)
    -- items are indexed by keys which are (objectId, iType, offset)
    -- they are returned in lexicographical order wrt the keys
    nextKey (objectId, iType, offset)
        | offset   < maxBound         = Just (objectId, iType, offset + 1)
        | iType    < skMaxType sk     = Just (objectId, iType + 1, skMinOffset sk)
        | objectId < skMaxObjectId sk = Just (objectId + 1, skMinType sk, skMinOffset sk)
        | otherwise                   = Nothing

treeSearch :: FILEPATH -> SearchKey -> Int -> (SearchHeader -> Ptr i -> IO ()) -> IO ()
treeSearch path sk maxItemCount callback =
    withFd path ReadOnly $ \fd ->
        treeSearchFd fd sk maxItemCount callback

treeSearchListFd :: Fd -> SearchKey -> (SearchHeader -> Ptr i -> IO (Maybe a)) -> IO [a]
treeSearchListFd fd sk unpack = do
    res <- newIORef []
    treeSearchFd fd sk maxBound $ \sh itemPtr -> do
        r <- unpack sh itemPtr
        case r of
            Nothing -> return ()
            Just x -> modifyIORef' res (x :)
    liftM reverse $ readIORef res

treeSearchList :: FILEPATH -> SearchKey -> (SearchHeader -> Ptr i -> IO (Maybe a)) -> IO [a]
treeSearchList path sk unpack =
    withFd path ReadOnly $ \fd ->
        treeSearchListFd fd sk unpack

findFirstItemFd :: Fd -> SearchKey -> (SearchHeader -> Ptr i -> IO a) -> IO a
findFirstItemFd fd sk unpack = do
    res <- newIORef Nothing
    treeSearchFd fd sk 1 $ \sh ptr -> do
        r <- unpack sh ptr
        modifyIORef' res (`mplus` Just r)
    resV <- readIORef res
    case resV of
        Just x -> return x
        Nothing ->
            ioError $ mkIOError doesNotExistErrorType
                                "findFirstItemFd"
                                Nothing Nothing

findFirstItem :: FILEPATH -> SearchKey -> (SearchHeader -> Ptr i -> IO a) -> IO a
findFirstItem path sk unpack =
    withFd path ReadOnly $ \fd ->
        findFirstItemFd fd sk unpack

-- does not initialize nr_items
pokeSearchKey :: Ptr a -> SearchKey -> IO ()
pokeSearchKey ptr sk = do
    (#poke struct btrfs_ioctl_search_key, tree_id     ) ptr (skTreeId      sk)
    (#poke struct btrfs_ioctl_search_key, min_objectid) ptr (skMinObjectId sk)
    (#poke struct btrfs_ioctl_search_key, min_type    ) ptr (fromIntegral (skMinType sk) :: Word32)
    (#poke struct btrfs_ioctl_search_key, min_offset  ) ptr (skMinOffset   sk)
    (#poke struct btrfs_ioctl_search_key, max_objectid) ptr (skMaxObjectId sk)
    (#poke struct btrfs_ioctl_search_key, max_type    ) ptr (fromIntegral (skMaxType sk) :: Word32)
    (#poke struct btrfs_ioctl_search_key, max_offset  ) ptr (skMaxOffset   sk)
    (#poke struct btrfs_ioctl_search_key, min_transid ) ptr (skMinTransId  sk)
    (#poke struct btrfs_ioctl_search_key, max_transid ) ptr (skMaxTransId  sk)

peekSearchItem :: Ptr a -> IO (SearchHeader, Ptr i)
peekSearchItem shPtr = do
    transId  <- (#peek struct btrfs_ioctl_search_header, transid ) shPtr :: IO Word64
    objectId <- (#peek struct btrfs_ioctl_search_header, objectid) shPtr :: IO Word64
    offset   <- (#peek struct btrfs_ioctl_search_header, offset  ) shPtr :: IO Word64
    iType    <- (#peek struct btrfs_ioctl_search_header, type    ) shPtr :: IO Word32
    len      <- (#peek struct btrfs_ioctl_search_header, len     ) shPtr :: IO Word32
    let itemPtr = shPtr `plusPtr` (#size struct btrfs_ioctl_search_header)
    return (SearchHeader transId objectId offset (fromIntegral iType) len, itemPtr)

peekRootRef :: Ptr a -> IO (InodeNum, FILEPATH)
peekRootRef rrPtr = do
    LE64 dirId   <- (#peek struct btrfs_root_ref, dirid   ) rrPtr
    LE16 nameLen <- (#peek struct btrfs_root_ref, name_len) rrPtr
    let cName = rrPtr `plusPtr` (#size struct btrfs_root_ref)
    name <- peekFilePathLen (cName, fromIntegral nameLen)
    return (dirId, name)

--------------------------------------------------------------------------------

withFd :: FILEPATH -> OpenMode -> (Fd -> IO r) -> IO r
withFd path openMode action =
    bracket (openFd path openMode defaultFileFlags {nonBlock = True})
            closeFd action

withSplitPathOpenParent :: String -> Int -> FILEPATH -> (CStringLen -> Fd -> IO r) -> IO r
withSplitPathOpenParent loc maxLen path action =
    unsafeWithFilePathLen name $ \cName@(_, l) -> do
        unless (l <= maxLen) $
            ioError $ flip ioeSetErrorString "the subvolume name is too long"
                    $ mkIOError illegalOperationErrorType loc Nothing (Just (asString name))
        withFd dir ReadOnly $ action cName
  where
    (dir, name) = splitFileName (dropTrailingSlash path)

nothingIf :: Bool -> a -> Maybe a
nothingIf f v = if f then Nothing else Just v
{-# INLINE nothingIf #-}

modifyPtr :: Storable a => Ptr a -> (a -> a) -> IO ()
modifyPtr ptr f = do
    peek ptr >>= (poke ptr . f)

setFlags :: (Storable a, Bits a) => Ptr a -> a -> IO ()
setFlags ptr flags =
    modifyPtr ptr (.|. flags)

clearFlags :: (Storable a, Bits a) => Ptr a -> a -> IO ()
clearFlags ptr flags =
    modifyPtr ptr (.&. complement flags)

allocaBytesZero :: Int -> (Ptr a -> IO b) -> IO b
allocaBytesZero size action =
    allocaBytes size $ \ptr -> do
        memset ptr 0 size
        action ptr

memset :: Ptr a -> Word8 -> Int -> IO ()
memset p b l = do
    _ <- c_memset p (fromIntegral b) (fromIntegral l)
    return ()
{-# INLINE memset #-}

foreign import ccall unsafe "string.h memset"
    c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)
