module Squeather.Internal.Types where

-- | Errors produced by the underlying SQLite3 C library.
data ErrorFlag
  = SQLITE_ERROR
  | SQLITE_INTERNAL
  | SQLITE_PERM
  | SQLITE_ABORT
  | SQLITE_BUSY
  | SQLITE_LOCKED
  | SQLITE_NOMEM
  | SQLITE_READONLY
  | SQLITE_INTERRUPT
  | SQLITE_IOERR
  | SQLITE_CORRUPT
  | SQLITE_NOTFOUND
  | SQLITE_FULL
  | SQLITE_CANTOPEN
  | SQLITE_PROTOCOL
  | SQLITE_EMPTY
  | SQLITE_SCHEMA
  | SQLITE_TOOBIG
  | SQLITE_CONSTRAINT
  | SQLITE_MISMATCH
  | SQLITE_MISUSE
  | SQLITE_NOLFS
  | SQLITE_AUTH
  | SQLITE_FORMAT
  | SQLITE_RANGE
  | SQLITE_NOTADB
  | SQLITE_NOTICE
  | SQLITE_WARNING
  | SQLITE_ERROR_MISSING_COLLSEQ
  | SQLITE_ERROR_RETRY
  | SQLITE_ERROR_SNAPSHOT
  | SQLITE_IOERR_READ
  | SQLITE_IOERR_SHORT_READ
  | SQLITE_IOERR_WRITE
  | SQLITE_IOERR_FSYNC
  | SQLITE_IOERR_DIR_FSYNC
  | SQLITE_IOERR_TRUNCATE
  | SQLITE_IOERR_FSTAT
  | SQLITE_IOERR_UNLOCK
  | SQLITE_IOERR_RDLOCK
  | SQLITE_IOERR_DELETE
  | SQLITE_IOERR_BLOCKED
  | SQLITE_IOERR_NOMEM
  | SQLITE_IOERR_ACCESS
  | SQLITE_IOERR_CHECKRESERVEDLOCK
  | SQLITE_IOERR_LOCK
  | SQLITE_IOERR_CLOSE
  | SQLITE_IOERR_DIR_CLOSE
  | SQLITE_IOERR_SHMOPEN
  | SQLITE_IOERR_SHMSIZE
  | SQLITE_IOERR_SHMLOCK
  | SQLITE_IOERR_SHMMAP
  | SQLITE_IOERR_SEEK
  | SQLITE_IOERR_DELETE_NOENT
  | SQLITE_IOERR_MMAP
  | SQLITE_IOERR_GETTEMPPATH
  | SQLITE_IOERR_CONVPATH
  | SQLITE_IOERR_VNODE
  | SQLITE_IOERR_AUTH
  | SQLITE_IOERR_BEGIN_ATOMIC
  | SQLITE_IOERR_COMMIT_ATOMIC
  | SQLITE_IOERR_ROLLBACK_ATOMIC
  | SQLITE_LOCKED_SHAREDCACHE
  | SQLITE_LOCKED_VTAB
  | SQLITE_BUSY_RECOVERY
  | SQLITE_BUSY_SNAPSHOT
  | SQLITE_CANTOPEN_NOTEMPDIR
  | SQLITE_CANTOPEN_ISDIR
  | SQLITE_CANTOPEN_FULLPATH
  | SQLITE_CANTOPEN_CONVPATH
  | SQLITE_CANTOPEN_DIRTYWAL
  | SQLITE_CANTOPEN_SYMLINK
  | SQLITE_CORRUPT_VTAB
  | SQLITE_CORRUPT_SEQUENCE
  | SQLITE_READONLY_RECOVERY
  | SQLITE_READONLY_CANTLOCK
  | SQLITE_READONLY_ROLLBACK
  | SQLITE_READONLY_DBMOVED
  | SQLITE_READONLY_CANTINIT
  | SQLITE_READONLY_DIRECTORY
  | SQLITE_ABORT_ROLLBACK
  | SQLITE_CONSTRAINT_CHECK
  | SQLITE_CONSTRAINT_COMMITHOOK
  | SQLITE_CONSTRAINT_FOREIGNKEY
  | SQLITE_CONSTRAINT_FUNCTION
  | SQLITE_CONSTRAINT_NOTNULL
  | SQLITE_CONSTRAINT_PRIMARYKEY
  | SQLITE_CONSTRAINT_TRIGGER
  | SQLITE_CONSTRAINT_UNIQUE
  | SQLITE_CONSTRAINT_VTAB
  | SQLITE_CONSTRAINT_ROWID
  | SQLITE_CONSTRAINT_PINNED
  | SQLITE_NOTICE_RECOVER_WAL
  | SQLITE_NOTICE_RECOVER_ROLLBACK
  | SQLITE_WARNING_AUTOINDEX
  | SQLITE_AUTH_USER
  | SQLITE_OK_LOAD_PERMANENTLY
  | SQLITE_OK_SYMLINK
  deriving (Eq, Ord, Show)

-- | Result from applying 'Squeather.step' to a
-- 'Squeather.Statement'.
data StepResult
  = Row 
  -- ^ A row is ready to be processed
  | Done
  -- ^ There are no more rows
  deriving (Eq, Ord, Show)

-- | Whether to create a new database if it does not already exist.
data Create
  = Create
  | NoCreate
  deriving (Eq, Ord, Show)

--  | Whether to open a database for reading and writing or for
--  reading only.
data WriteMode
  = ReadOnly
  | ReadWrite Create
  deriving (Eq, Ord, Show)

-- | Whether to use multi-thread mode or serialized mode, see
--
-- <https://www.sqlite.org/threadsafe.html>
--
-- It is not possible to use the SQLite single-thread mode.
data ThreadMode = MultiThread | Serialized
  deriving (Eq, Ord, Show)

-- | Whether to use shared cache or private cache mode, see
--
-- <https://www.sqlite.org/sharedcache.html>
data CacheMode = Shared | Private
  deriving (Eq, Ord, Show)

-- | Various options when opening a database.
data OpenFlags = OpenFlags
  { writeMode :: WriteMode
  , uri :: Bool
  -- ^ Filename can be interpreted as a URI if True.

  , memory :: Bool
  -- ^ Database will be opened in-memory.  If you use this flag, the
  -- filename is ignored.

  , threadMode :: ThreadMode

  , cacheMode :: CacheMode

  , noFollow :: Bool
  -- ^ If True, the database filename is not allowed to be a
  -- symbolic link.
  } deriving (Eq, Ord, Show)

