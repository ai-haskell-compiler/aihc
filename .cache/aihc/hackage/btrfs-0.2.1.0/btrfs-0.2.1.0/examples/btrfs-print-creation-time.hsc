import System.Environment (getArgs)
import Control.Monad (forM_)
import Foreign (peekByteOff)
import System.Posix (getFileStatus, fileID)
import Data.Time (UTCTime, utcToLocalZonedTime, zonedTimeToLocalTime)

import System.Linux.Btrfs
import System.Linux.Btrfs.Time

#include <btrfs/ctree.h>

{-
Starting with linux 4.0, btrfs records the creation time of inodes ("otime)
for all newly created inodes. Older kernel wrote 0 (i.e. the unix epoch) in
that field.
-}

main :: IO ()
main = do
    paths <- getArgs
    forM_ paths $ \path -> do
        t <- getOTime path >>= fmap zonedTimeToLocalTime . utcToLocalZonedTime
        let s = show t
        putStrLn $ s ++ replicate (30 - length s) ' ' ++ path

getOTime :: FilePath -> IO UTCTime
getOTime path = do
    subvolId <- getSubvol path
    inum <- fmap (fromIntegral . fileID) $ getFileStatus path
    getOTimeById subvolId inum

getOTimeById :: SubvolId -> InodeNum -> IO UTCTime
getOTimeById subvolId inum = do
    findFirstItem "/" sk $ \_ ptr -> do
        BtrfsTime t <- (#peek struct btrfs_inode_item, otime) ptr
        return t
  where
    sk = defaultSearchKey
        { skTreeId = subvolId
        , skMinObjectId = inum, skMinType = objType, skMinOffset = 0
        , skMaxObjectId = inum, skMaxType = objType, skMaxOffset = 0
        }
    objType = #const BTRFS_INODE_ITEM_KEY
