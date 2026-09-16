{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The POSIX types that the C bindings of the boot libraries use.
--
-- Each type is a newtype over the width its platform gives the C type,
-- which 'System.Posix.Types.Repr' supplies: a POSIX type is whatever the
-- headers say it is, and the platforms disagree about nearly half of them.
--
-- The aliases at the end are the names POSIX itself uses for these types.
-- They are the spellings @unix@ writes in its signatures, so they are type
-- synonyms rather than newtypes of their own, exactly as in GHC.
module System.Posix.Types
  ( -- * The C types
    CBlkCnt (..),
    CBlkSize (..),
    CCc (..),
    CClockId (..),
    CDev (..),
    CFsBlkCnt (..),
    CFsFilCnt (..),
    CGid (..),
    CId (..),
    CIno (..),
    CKey (..),
    CMode (..),
    CNfds (..),
    CNlink (..),
    COff (..),
    CPid (..),
    CRLim (..),
    CSocklen (..),
    CSpeed (..),
    CSsize (..),
    CTcflag (..),
    CUid (..),
    Fd (..),

    -- * The POSIX names for them
    ByteCount,
    ClockTick,
    DeviceID,
    EpochTime,
    FileID,
    FileMode,
    FileOffset,
    GroupID,
    Limit,
    LinkCount,
    ProcessGroupID,
    ProcessID,
    UserID,
  )
where

import Data.Bits (Bits, FiniteBits)
import Foreign.C.Types (CClock, CInt (..), CLong, CSize, CTime)
import Foreign.C.Types.Repr (CSsizeRep)
import Foreign.Storable (Storable)
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Read (Read)
import GHC.Real (Integral (..), Real (..))
import GHC.Show (Show)
import System.Posix.Types.Repr
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

-- | The POSIX @blkcnt_t@: a count of file-system blocks.
newtype CBlkCnt = CBlkCnt CBlkCntRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @blksize_t@: the block size a file system reports.
newtype CBlkSize = CBlkSize CBlkSizeRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @cc_t@: one terminal control character.
newtype CCc = CCc CCcRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @clockid_t@: the C @int@ that names a system clock.
newtype CClockId = CClockId CInt
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @dev_t@: the device a file lives on.
newtype CDev = CDev CDevRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @fsblkcnt_t@: a count of blocks in a file system.
newtype CFsBlkCnt = CFsBlkCnt CFsBlkCntRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @fsfilcnt_t@: a count of files in a file system.
newtype CFsFilCnt = CFsFilCnt CFsFilCntRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @gid_t@: a group id.
newtype CGid = CGid CGidRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @id_t@: a user or group id, whichever a call takes.
newtype CId = CId CIdRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @ino_t@: the inode number of a file.
newtype CIno = CIno CInoRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @key_t@: the key of a System V IPC object.
newtype CKey = CKey CKeyRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @mode_t@: the type and permission bits of a file.
newtype CMode = CMode CModeRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @nfds_t@: a count of descriptors handed to @poll@.
newtype CNfds = CNfds CNfdsRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @nlink_t@: the number of links to a file.
newtype CNlink = CNlink CNlinkRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @off_t@: a byte offset into a file.
newtype COff = COff COffRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @pid_t@: a process or process-group id.
newtype CPid = CPid CPidRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @rlim_t@: one resource limit.
newtype CRLim = CRLim CRLimRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @socklen_t@: the length of a socket address.
newtype CSocklen = CSocklen CSocklenRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @speed_t@: a terminal line speed.
newtype CSpeed = CSpeed CSpeedRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The C @ssize_t@: a byte count that can also carry @-1@ for an error.
newtype CSsize = CSsize CSsizeRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @tcflag_t@: one set of terminal mode flags.
newtype CTcflag = CTcflag CTcflagRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX @uid_t@: a user id.
newtype CUid = CUid CUidRep
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | The POSIX file descriptor: the C @int@ that names an open file.
newtype Fd = Fd CInt
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, Real, Integral, Bits, FiniteBits, Storable)

-- | A number of bytes, as @read@ and @write@ count them.
type ByteCount = CSize

-- | A number of clock ticks, as @times@ counts them.
type ClockTick = CClock

-- | The device a file lives on.
type DeviceID = CDev

-- | A time in seconds since the epoch.
type EpochTime = CTime

-- | The inode number of a file.
type FileID = CIno

-- | The type and permission bits of a file.
type FileMode = CMode

-- | A byte offset into a file.
type FileOffset = COff

-- | A group id.
type GroupID = CGid

-- | A resource limit, as @pathconf@ and @sysconf@ report one.
type Limit = CLong

-- | The number of links to a file.
type LinkCount = CNlink

-- | A process-group id.
type ProcessGroupID = CPid

-- | A process id.
type ProcessID = CPid

-- | A user id.
type UserID = CUid
