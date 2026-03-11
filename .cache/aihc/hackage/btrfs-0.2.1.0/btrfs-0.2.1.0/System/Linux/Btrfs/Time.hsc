module System.Linux.Btrfs.Time
    ( BtrfsTime(..)
    ) where

import Data.Time.Format ()
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word.Endian (LE32(..), LE64(..))
import Data.Ratio ((%))

import Foreign.Storable (Storable(..))
import Foreign.C.Types (CInt)

#include <btrfs/ctree.h>

newtype BtrfsTime = BtrfsTime UTCTime

instance Storable BtrfsTime where
    sizeOf _ = (#size struct btrfs_timespec)
    alignment _ = alignment (undefined :: CInt)
    poke _ _ = error "not implemented"
    peek ptr = do
        LE64 sec  <- (#peek struct btrfs_timespec, sec ) ptr
        LE32 nsec <- (#peek struct btrfs_timespec, nsec) ptr
        let frac = toInteger nsec % 1000000000
        return $ BtrfsTime $ posixSecondsToUTCTime $
            fromRational $ toRational sec + frac
