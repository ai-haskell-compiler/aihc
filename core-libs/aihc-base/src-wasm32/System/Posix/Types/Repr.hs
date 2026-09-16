-- | The widths of the POSIX types on wasm32 under WASI.
--
-- A POSIX type is whatever the platform's headers say it is, and the
-- platforms disagree: wasi-libc follows the Linux widths for the file-system types but is
-- an ILP32 platform, so its @blksize_t@ and @nfds_t@ stay at thirty-two bits.
-- Each width therefore has one definition per platform rather than a rule,
-- and 'System.Posix.Types' names these aliases instead of a concrete width.
--
-- The numbers come from the platform's own headers. Nothing checks them
-- automatically: the spec suite compiles a C file of assertions against the
-- headers of the platform it runs on, and no test host is wasm32.
module System.Posix.Types.Repr
  ( CBlkCntRep,
    CBlkSizeRep,
    CCcRep,
    CDevRep,
    CFsBlkCntRep,
    CFsFilCntRep,
    CGidRep,
    CIdRep,
    CInoRep,
    CKeyRep,
    CModeRep,
    CNfdsRep,
    CNlinkRep,
    COffRep,
    CPidRep,
    CRLimRep,
    CSocklenRep,
    CSpeedRep,
    CTcflagRep,
    CUidRep,
  )
where

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64, Word8)

-- | @blkcnt_t@: a signed 64-bit count of blocks.
type CBlkCntRep = Int64

-- | @blksize_t@: a signed 32-bit block size.
type CBlkSizeRep = Int32

-- | @cc_t@: an unsigned byte of terminal control.
type CCcRep = Word8

-- | @dev_t@: an unsigned 64-bit device number.
type CDevRep = Word64

-- | @fsblkcnt_t@: an unsigned 64-bit file-system block count.
type CFsBlkCntRep = Word64

-- | @fsfilcnt_t@: an unsigned 64-bit file-system file count.
type CFsFilCntRep = Word64

-- | @gid_t@: an unsigned 32-bit group id.
type CGidRep = Word32

-- | @id_t@: an unsigned 32-bit user or group id.
type CIdRep = Word32

-- | @ino_t@: an unsigned 64-bit inode number.
type CInoRep = Word64

-- | @key_t@: a signed 32-bit IPC key.
type CKeyRep = Int32

-- | @mode_t@: an unsigned 32-bit file mode.
type CModeRep = Word32

-- | @nfds_t@: an unsigned 32-bit descriptor count.
type CNfdsRep = Word32

-- | @nlink_t@: an unsigned 64-bit link count.
type CNlinkRep = Word64

-- | @off_t@: a signed 64-bit file offset.
type COffRep = Int64

-- | @pid_t@: a signed 32-bit process id.
type CPidRep = Int32

-- | @rlim_t@: an unsigned 64-bit resource limit.
type CRLimRep = Word64

-- | @socklen_t@: an unsigned 32-bit socket address length.
type CSocklenRep = Word32

-- | @speed_t@: an unsigned 32-bit terminal line speed.
type CSpeedRep = Word32

-- | @tcflag_t@: an unsigned 32-bit terminal mode flag set.
type CTcflagRep = Word32

-- | @uid_t@: an unsigned 32-bit user id.
type CUidRep = Word32

-- wasi-libc ships no @termios.h@ and no @sys/resource.h@, so @cc_t@,
-- @speed_t@, @tcflag_t@ and @rlim_t@ have nothing on the platform to follow.
-- They take the Linux widths, which are the common POSIX shape, and the
-- runtime asserts nothing about them because there is nothing to assert
-- them against.
