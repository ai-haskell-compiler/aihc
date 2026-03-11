{-# LANGUAGE CPP, ForeignFunctionInterface, DeriveDataTypeable #-}

-- | Provides @mmap@: mapping files into memory.
module MMAP where

import           Control.Exception
import           Control.Monad
import           Data.Bits ((.|.))
import           Data.Typeable
import           Foreign.C.Types
import           Foreign.Ptr
import           System.Posix.Types

#include <sys/mman.h>


-- | Exception thrown when @mmap@ fails.
data MmapException = MmapException
  deriving (Eq, Ord, Show, Typeable)

instance Exception MmapException

-- | Exception thrown when @munmap@ fails.
data MunmapException = MunmapException
  { munmapExceptionSize :: CSize
  , munmapExceptionPtr  :: Ptr ()
  } deriving (Eq, Ord, Show, Typeable)

instance Exception MunmapException


-- | @mmap@ - map files or devices into memory.
--
-- The returned memory should be freed with `c_munmap` after use.
--
-- This is the raw C function, and its return value must be checked
-- according to @man mmap@.
--
-- See `mmap` for a variant that turns bad return values into exceptions.
foreign import ccall unsafe "sys/mman.h mmap"
  -- void *mmap(void *addr, size_t length, int prot, int flags,
  --            int fd, off_t offset)
  c_mmap :: Ptr ()      -- ^ void *addr
         -> CSize       -- ^ size_t length
         -> ProtOption  -- ^ int prot
         -> MmapFlags   -- ^ int flags,
         -> Fd          -- ^ int fd
         -> COff        -- ^ off_t offset
         -> IO (Ptr ()) -- void *mmap


-- | @munmap@ - unmap @mmap@'ed memory.
--
-- This is the raw C function, and its return value must be checked
-- according to @man mmap@.
--
-- See `munmap` for a variant that turns bad return values into exceptions.
foreign import ccall unsafe "sys/mman.h munmap"
  -- int munmap(void *addr, size_t length)
  c_munmap :: Ptr ()  -- ^ void *addr
           -> CSize   -- ^ size_t length
           -> IO CInt -- int munmap


-- | Convenience around `c_mmap`, throwing a `MmapException` on a negative return value.
mmap :: Ptr ()
     -> CSize
     -> ProtOption
     -> MmapFlags
     -> Fd
     -> COff
     -> IO (Ptr ())
mmap addr len prot flags fd offset = do
  ptr <- c_mmap addr len prot flags fd offset
  when (ptr == intPtrToPtr (-1)) $ throwIO MmapException
  return ptr


-- | Convenience around `c_munmap`, throwing a `MunmapException` on a negative return value.
munmap :: Ptr ()
       -> CSize
       -> IO ()
munmap addr len = do
  v <- c_munmap addr len
  when (v == -1) . throwIO $ MunmapException len addr


-- | Describes the desired memory protection of the mapping
-- (and  must  not  conflict  with  the open mode of the file).
--
-- Can be combined using the `Monoid` instance `(<>)`.
newtype ProtOption = ProtOption { unProtOption :: CInt }
  deriving (Eq, Show, Ord)

-- | Pages may be executed.
protExec :: ProtOption
protExec = ProtOption #const PROT_EXEC

-- | Pages may be read.
protRead :: ProtOption
protRead = ProtOption #const PROT_READ

-- | Pages may be written.
protWrite :: ProtOption
protWrite = ProtOption #const PROT_WRITE

-- | Pages may not be accessed.
protNone :: ProtOption
protNone = ProtOption #const PROT_NONE

instance Monoid ProtOption where
  mempty = protNone

instance Semigroup ProtOption where
  (<>) (ProtOption a) (ProtOption b) = ProtOption (a .|. b)


-- | Determines whether updates to the mapping are visible
-- to other processes mapping the same region, and whether updates
-- are carried through to the underlying file.
--
-- This behavior is determined by including exactly one of
-- `mapShared` and `mapPrivate`.
newtype MmapSharedFlag = MmapSharedFlag { unMmapSharedFlag :: CInt }
  deriving (Eq, Show, Ord)

-- | Share this mapping. Updates to the mapping are visible to
-- other processes that map the file, and are carried through to
-- the underlying file. The file may not actually be updated
-- until @msync(2)@ or @munmap()@ is called..
mapShared :: MmapSharedFlag
mapShared = MmapSharedFlag #const MAP_SHARED

-- | Create a private copy-on-write mapping. Updates to the mapping
-- are not visible to other processes mapping the same file, and
-- are not carried through to the underlying file. It is unspecified
-- whether changes made to the file after the @mmap()@ call are
-- visible in the mapped region.
mapPrivate :: MmapSharedFlag
mapPrivate = MmapSharedFlag #const MAP_PRIVATE


-- | And `MmapSharedFlag` with one or more `MmapOptionalFlag`s.
newtype MmapOptionalFlag = MmapOptionalFlag { unMmapOptionalFlag :: CInt }
  deriving (Eq, Show, Ord)


#ifdef MAP_32BIT
-- | See @man mmap@ for a description.
map32Bit :: MmapOptionalFlag
map32Bit = MmapOptionalFlag #const MAP_32BIT
#endif

#ifdef MAP_ANONYMOUS
-- | See @man mmap@ for a description.
mapAnonymous :: MmapOptionalFlag
mapAnonymous = MmapOptionalFlag #const MAP_ANONYMOUS
#else
#ifdef MAP_ANON
-- | See @man mmap@ for a description.
mapAnonymous :: MmapOptionalFlag
mapAnonymous = MmapOptionalFlag #const MAP_ANON
#endif
#endif

#ifdef MAP_DENYWRITE
-- | See @man mmap@ for a description.
mapDenywrite :: MmapOptionalFlag
mapDenywrite = MmapOptionalFlag #const MAP_DENYWRITE
#endif

#ifdef MAP_FILE
-- | See @man mmap@ for a description.
mapFile :: MmapOptionalFlag
mapFile = MmapOptionalFlag #const MAP_FILE
#endif

#ifdef MAP_FIXED
-- | See @man mmap@ for a description.
mapFixed :: MmapOptionalFlag
mapFixed = MmapOptionalFlag #const MAP_FIXED
#endif

#ifdef MAP_HUGETLB
-- | See @man mmap@ for a description.
mapHugetlb :: MmapOptionalFlag
mapHugetlb = MmapOptionalFlag #const MAP_HUGETLB
#endif

#ifdef MAP_LOCKED
-- | See @man mmap@ for a description.
mapLocked :: MmapOptionalFlag
mapLocked = MmapOptionalFlag #const MAP_LOCKED
#endif

#ifdef MAP_NONBLOCK
-- | See @man mmap@ for a description.
mapNonblock :: MmapOptionalFlag
mapNonblock = MmapOptionalFlag #const MAP_NONBLOCK
#endif

#ifdef MAP_NORESERVE
-- | See @man mmap@ for a description.
mapNoreserve :: MmapOptionalFlag
mapNoreserve = MmapOptionalFlag #const MAP_NORESERVE
#endif

#ifdef MAP_STACK
-- | See @man mmap@ for a description.
mapStack :: MmapOptionalFlag
mapStack = MmapOptionalFlag #const MAP_STACK
#endif

#ifdef MAP_UNINITIALIZED
-- | See @man mmap@ for a description.
mapUninitialized :: MmapOptionalFlag
mapUninitialized = MmapOptionalFlag #const MAP_UNINITIALIZED
#endif

instance Monoid MmapOptionalFlag where
  mempty = MmapOptionalFlag 0

instance Semigroup MmapOptionalFlag where
  (<>) (MmapOptionalFlag a) (MmapOptionalFlag b) = MmapOptionalFlag (a .|. b)


-- | An `MmapSharedFlag` with one or more `MmapOptionalFlag`s.
newtype MmapFlags = MmapFlags { unMmapFlags :: CInt }
  deriving (Eq, Show, Ord)

-- | Create `MmapFlags` to be passed to `c_mmap` from an `MmapSharedFlag`
-- and one or more `MmapOptionalFlag`s (combine them via `(<>)`,
-- `mempty` for none).
mkMmapFlags :: MmapSharedFlag -> MmapOptionalFlag -> MmapFlags
mkMmapFlags (MmapSharedFlag a) (MmapOptionalFlag b) = MmapFlags (a .|. b)
