{-# LANGUAGE CPP #-}

{- |
Module      : System.DiskSpace

Stability   : provisional
Portability : portable
-}

module System.DiskSpace
    ( DiskUsage(..)
    , getDiskUsage
    , getAvailSpace
    ) where

#ifndef CABAL_OS_WINDOWS

import Foreign
import Foreign.C

#include <sys/statvfs.h>

foreign import ccall safe statvfs :: CString -> Ptr a -> IO CInt

type FsBlkCnt = #type fsblkcnt_t

getDiskUsage path =
    withCString path $ \cPath ->
        allocaBytes (#size struct statvfs) $ \stat -> do
            throwErrnoPathIfMinus1_ "getDiskUsage" path $ statvfs cPath stat
            bsize  <- (#peek struct statvfs, f_bsize ) stat :: IO CULong
            frsize <- (#peek struct statvfs, f_frsize) stat :: IO CULong
            blocks <- (#peek struct statvfs, f_blocks) stat :: IO FsBlkCnt
            bfree  <- (#peek struct statvfs, f_bfree ) stat :: IO FsBlkCnt
            bavail <- (#peek struct statvfs, f_bavail) stat :: IO FsBlkCnt
            let frsize' = fromIntegral frsize
            return DiskUsage
                { diskTotal = frsize' * fromIntegral blocks
                , diskFree  = frsize' * fromIntegral bfree
                , diskAvail = frsize' * fromIntegral bavail
                , blockSize = fromIntegral bsize
                }

#else

import System.Win32 (getDiskFreeSpace)

getDiskUsage path = do
    (lpSectorsPerCluster,
     lpBytesPerSector,
     lpNumberOfFreeClusters,
     lpTotalNumberOfClusters)
        <- getDiskFreeSpace (Just path)
    let bs = fromIntegral lpSectorsPerCluster * fromIntegral lpBytesPerSector
        bs' = fromIntegral bs
        to = bs' * fromIntegral lpTotalNumberOfClusters
        av = bs' * fromIntegral lpNumberOfFreeClusters
    return (DiskUsage to av av bs)

#endif

-- | Disk usage information. All fields are in bytes.
data DiskUsage = DiskUsage
    { diskTotal :: Integer -- ^ The total size of the file system.
    , diskFree  :: Integer -- ^ The amount of free space. You probably want to
                           --   use 'diskAvail' instead.
    , diskAvail :: Integer -- ^ The amount of space available to the user.
                           --   Might be less than 'diskFree'. On Windows,
                           --   this is always equal to 'diskFree'.
                           --   This is what most tools report as free
                           --   space (e.g. the unix @df@ tool).
    , blockSize :: Int     -- ^ The optimal block size for I/O in this volume.
                           --   Some operating systems report incorrect values
                           --   for this field.
    }
  deriving (Show, Eq)

-- | Retrieve disk usage information about a volume. The volume is
-- specified with the @FilePath@ argument. The path can refer to the root
-- directory or any other directory inside the volume.
-- Unix systems also accept arbitrary files, but this
-- does not work under Windows and therefore should be avoided if
-- portability is desired.
getDiskUsage :: FilePath -> IO DiskUsage

-- | A convenience function that directly returns the 'diskAvail' field from
-- the result of 'getDiskUsage'. If a large amount of data is to be written
-- in a directory, calling this function for that directory can be used to
-- determine whether the operation will fail because of insufficient disk
-- space.
getAvailSpace :: FilePath -> IO Integer
getAvailSpace = fmap diskAvail . getDiskUsage
