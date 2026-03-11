module Squeather.Internal.Bindings where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (Text)
import Foreign.C.Types (CInt)

import Squeather.Internal.Types

#include "sqlite3.h"

data ParseErrorResult
  = ParseErrorOk
  -- ^ There was no error
  | ParseErrorStep StepResult
  -- ^ A StepResult was found.
  | ParseErrorError ErrorFlag
  -- ^ An error code was found.
  | ParseErrorNotFound
  -- ^ The error code was not found.
  deriving (Eq, Ord, Show)

parseError :: (Integral a, Show a) => a -> ParseErrorResult
parseError n = case n of
  #{const SQLITE_OK} -> ParseErrorOk
  #{const SQLITE_ROW} -> ParseErrorStep Row
  #{const SQLITE_DONE} -> ParseErrorStep Done
  #{const SQLITE_ERROR} -> ParseErrorError SQLITE_ERROR
  #{const SQLITE_INTERNAL} -> ParseErrorError SQLITE_INTERNAL
  #{const SQLITE_PERM} -> ParseErrorError SQLITE_PERM
  #{const SQLITE_ABORT} -> ParseErrorError SQLITE_ABORT
  #{const SQLITE_BUSY} -> ParseErrorError SQLITE_BUSY
  #{const SQLITE_LOCKED} -> ParseErrorError SQLITE_LOCKED
  #{const SQLITE_NOMEM} -> ParseErrorError SQLITE_NOMEM
  #{const SQLITE_READONLY} -> ParseErrorError SQLITE_READONLY
  #{const SQLITE_INTERRUPT} -> ParseErrorError SQLITE_INTERRUPT
  #{const SQLITE_IOERR} -> ParseErrorError SQLITE_IOERR
  #{const SQLITE_CORRUPT} -> ParseErrorError SQLITE_CORRUPT
  #{const SQLITE_NOTFOUND} -> ParseErrorError SQLITE_NOTFOUND
  #{const SQLITE_FULL} -> ParseErrorError SQLITE_FULL
  #{const SQLITE_CANTOPEN} -> ParseErrorError SQLITE_CANTOPEN
  #{const SQLITE_PROTOCOL} -> ParseErrorError SQLITE_PROTOCOL
  #{const SQLITE_EMPTY} -> ParseErrorError SQLITE_EMPTY
  #{const SQLITE_SCHEMA} -> ParseErrorError SQLITE_SCHEMA
  #{const SQLITE_TOOBIG} -> ParseErrorError SQLITE_TOOBIG
  #{const SQLITE_CONSTRAINT} -> ParseErrorError SQLITE_CONSTRAINT
  #{const SQLITE_MISMATCH} -> ParseErrorError SQLITE_MISMATCH
  #{const SQLITE_MISUSE} -> ParseErrorError SQLITE_MISUSE
  #{const SQLITE_NOLFS} -> ParseErrorError SQLITE_NOLFS
  #{const SQLITE_AUTH} -> ParseErrorError SQLITE_AUTH
  #{const SQLITE_FORMAT} -> ParseErrorError SQLITE_FORMAT
  #{const SQLITE_RANGE} -> ParseErrorError SQLITE_RANGE
  #{const SQLITE_NOTADB} -> ParseErrorError SQLITE_NOTADB
  #{const SQLITE_NOTICE} -> ParseErrorError SQLITE_NOTICE
  #{const SQLITE_WARNING} -> ParseErrorError SQLITE_WARNING
  #{const SQLITE_ERROR_MISSING_COLLSEQ} -> ParseErrorError SQLITE_ERROR_MISSING_COLLSEQ
  #{const SQLITE_ERROR_RETRY} -> ParseErrorError SQLITE_ERROR_RETRY
  #{const SQLITE_ERROR_SNAPSHOT} -> ParseErrorError SQLITE_ERROR_SNAPSHOT
  #{const SQLITE_IOERR_READ} -> ParseErrorError SQLITE_IOERR_READ
  #{const SQLITE_IOERR_SHORT_READ} -> ParseErrorError SQLITE_IOERR_SHORT_READ
  #{const SQLITE_IOERR_WRITE} -> ParseErrorError SQLITE_IOERR_WRITE
  #{const SQLITE_IOERR_FSYNC} -> ParseErrorError SQLITE_IOERR_FSYNC
  #{const SQLITE_IOERR_DIR_FSYNC} -> ParseErrorError SQLITE_IOERR_DIR_FSYNC
  #{const SQLITE_IOERR_TRUNCATE} -> ParseErrorError SQLITE_IOERR_TRUNCATE
  #{const SQLITE_IOERR_FSTAT} -> ParseErrorError SQLITE_IOERR_FSTAT
  #{const SQLITE_IOERR_UNLOCK} -> ParseErrorError SQLITE_IOERR_UNLOCK
  #{const SQLITE_IOERR_RDLOCK} -> ParseErrorError SQLITE_IOERR_RDLOCK
  #{const SQLITE_IOERR_DELETE} -> ParseErrorError SQLITE_IOERR_DELETE
  #{const SQLITE_IOERR_BLOCKED} -> ParseErrorError SQLITE_IOERR_BLOCKED
  #{const SQLITE_IOERR_NOMEM} -> ParseErrorError SQLITE_IOERR_NOMEM
  #{const SQLITE_IOERR_ACCESS} -> ParseErrorError SQLITE_IOERR_ACCESS
  #{const SQLITE_IOERR_CHECKRESERVEDLOCK} -> ParseErrorError SQLITE_IOERR_CHECKRESERVEDLOCK
  #{const SQLITE_IOERR_LOCK} -> ParseErrorError SQLITE_IOERR_LOCK
  #{const SQLITE_IOERR_CLOSE} -> ParseErrorError SQLITE_IOERR_CLOSE
  #{const SQLITE_IOERR_DIR_CLOSE} -> ParseErrorError SQLITE_IOERR_DIR_CLOSE
  #{const SQLITE_IOERR_SHMOPEN} -> ParseErrorError SQLITE_IOERR_SHMOPEN
  #{const SQLITE_IOERR_SHMSIZE} -> ParseErrorError SQLITE_IOERR_SHMSIZE
  #{const SQLITE_IOERR_SHMLOCK} -> ParseErrorError SQLITE_IOERR_SHMLOCK
  #{const SQLITE_IOERR_SHMMAP} -> ParseErrorError SQLITE_IOERR_SHMMAP
  #{const SQLITE_IOERR_SEEK} -> ParseErrorError SQLITE_IOERR_SEEK
  #{const SQLITE_IOERR_DELETE_NOENT} -> ParseErrorError SQLITE_IOERR_DELETE_NOENT
  #{const SQLITE_IOERR_MMAP} -> ParseErrorError SQLITE_IOERR_MMAP
  #{const SQLITE_IOERR_GETTEMPPATH} -> ParseErrorError SQLITE_IOERR_GETTEMPPATH
  #{const SQLITE_IOERR_CONVPATH} -> ParseErrorError SQLITE_IOERR_CONVPATH
  #{const SQLITE_IOERR_VNODE} -> ParseErrorError SQLITE_IOERR_VNODE
  #{const SQLITE_IOERR_AUTH} -> ParseErrorError SQLITE_IOERR_AUTH
  #{const SQLITE_IOERR_BEGIN_ATOMIC} -> ParseErrorError SQLITE_IOERR_BEGIN_ATOMIC
  #{const SQLITE_IOERR_COMMIT_ATOMIC} -> ParseErrorError SQLITE_IOERR_COMMIT_ATOMIC
  #{const SQLITE_IOERR_ROLLBACK_ATOMIC} -> ParseErrorError SQLITE_IOERR_ROLLBACK_ATOMIC
  #{const SQLITE_LOCKED_SHAREDCACHE} -> ParseErrorError SQLITE_LOCKED_SHAREDCACHE
  #{const SQLITE_LOCKED_VTAB} -> ParseErrorError SQLITE_LOCKED_VTAB
  #{const SQLITE_BUSY_RECOVERY} -> ParseErrorError SQLITE_BUSY_RECOVERY
  #{const SQLITE_BUSY_SNAPSHOT} -> ParseErrorError SQLITE_BUSY_SNAPSHOT
  #{const SQLITE_CANTOPEN_NOTEMPDIR} -> ParseErrorError SQLITE_CANTOPEN_NOTEMPDIR
  #{const SQLITE_CANTOPEN_ISDIR} -> ParseErrorError SQLITE_CANTOPEN_ISDIR
  #{const SQLITE_CANTOPEN_FULLPATH} -> ParseErrorError SQLITE_CANTOPEN_FULLPATH
  #{const SQLITE_CANTOPEN_CONVPATH} -> ParseErrorError SQLITE_CANTOPEN_CONVPATH
  #{const SQLITE_CANTOPEN_DIRTYWAL} -> ParseErrorError SQLITE_CANTOPEN_DIRTYWAL
  #{const SQLITE_CANTOPEN_SYMLINK} -> ParseErrorError SQLITE_CANTOPEN_SYMLINK
  #{const SQLITE_CORRUPT_VTAB} -> ParseErrorError SQLITE_CORRUPT_VTAB
  #{const SQLITE_CORRUPT_SEQUENCE} -> ParseErrorError SQLITE_CORRUPT_SEQUENCE
  #{const SQLITE_READONLY_RECOVERY} -> ParseErrorError SQLITE_READONLY_RECOVERY
  #{const SQLITE_READONLY_CANTLOCK} -> ParseErrorError SQLITE_READONLY_CANTLOCK
  #{const SQLITE_READONLY_ROLLBACK} -> ParseErrorError SQLITE_READONLY_ROLLBACK
  #{const SQLITE_READONLY_DBMOVED} -> ParseErrorError SQLITE_READONLY_DBMOVED
  #{const SQLITE_READONLY_CANTINIT} -> ParseErrorError SQLITE_READONLY_CANTINIT
  #{const SQLITE_READONLY_DIRECTORY} -> ParseErrorError SQLITE_READONLY_DIRECTORY
  #{const SQLITE_ABORT_ROLLBACK} -> ParseErrorError SQLITE_ABORT_ROLLBACK
  #{const SQLITE_CONSTRAINT_CHECK} -> ParseErrorError SQLITE_CONSTRAINT_CHECK
  #{const SQLITE_CONSTRAINT_COMMITHOOK} -> ParseErrorError SQLITE_CONSTRAINT_COMMITHOOK
  #{const SQLITE_CONSTRAINT_FOREIGNKEY} -> ParseErrorError SQLITE_CONSTRAINT_FOREIGNKEY
  #{const SQLITE_CONSTRAINT_FUNCTION} -> ParseErrorError SQLITE_CONSTRAINT_FUNCTION
  #{const SQLITE_CONSTRAINT_NOTNULL} -> ParseErrorError SQLITE_CONSTRAINT_NOTNULL
  #{const SQLITE_CONSTRAINT_PRIMARYKEY} -> ParseErrorError SQLITE_CONSTRAINT_PRIMARYKEY
  #{const SQLITE_CONSTRAINT_TRIGGER} -> ParseErrorError SQLITE_CONSTRAINT_TRIGGER
  #{const SQLITE_CONSTRAINT_UNIQUE} -> ParseErrorError SQLITE_CONSTRAINT_UNIQUE
  #{const SQLITE_CONSTRAINT_VTAB} -> ParseErrorError SQLITE_CONSTRAINT_VTAB
  #{const SQLITE_CONSTRAINT_ROWID} -> ParseErrorError SQLITE_CONSTRAINT_ROWID
  #{const SQLITE_CONSTRAINT_PINNED} -> ParseErrorError SQLITE_CONSTRAINT_PINNED
  #{const SQLITE_NOTICE_RECOVER_WAL} -> ParseErrorError SQLITE_NOTICE_RECOVER_WAL
  #{const SQLITE_NOTICE_RECOVER_ROLLBACK} -> ParseErrorError SQLITE_NOTICE_RECOVER_ROLLBACK
  #{const SQLITE_WARNING_AUTOINDEX} -> ParseErrorError SQLITE_WARNING_AUTOINDEX
  #{const SQLITE_AUTH_USER} -> ParseErrorError SQLITE_AUTH_USER

  -- The next two are strange because they are not really errors.
  -- For the time being I'll just leave them in this case statement
  -- because they shouldn't arise when using this library.
  #{const SQLITE_OK_LOAD_PERMANENTLY} -> ParseErrorError SQLITE_OK_LOAD_PERMANENTLY
  #{const SQLITE_OK_SYMLINK} -> ParseErrorError SQLITE_OK_SYMLINK
  _ -> ParseErrorNotFound

c'SQLITE_VERSION :: String
c'SQLITE_VERSION = #const_str SQLITE_VERSION

c'SQLITE_VERSION_NUMBER :: Integral a => a
c'SQLITE_VERSION_NUMBER = #const SQLITE_VERSION_NUMBER

c'SQLITE_SOURCE_ID :: String
c'SQLITE_SOURCE_ID = #const_str SQLITE_SOURCE_ID

c'SQLITE_STATIC :: Integral a => a
c'SQLITE_STATIC = #{const SQLITE_STATIC}

c'SQLITE_TRANSIENT :: Integral a => a
c'SQLITE_TRANSIENT = #{const SQLITE_TRANSIENT}

-- | Various types of SQL data; used both when obtaining
-- query results and when providing named parameters.
data SQLData
  = SQLNull
  | SQLText Text
  | SQLFloat Double
  | SQLInteger Int64
  | SQLBlob ByteString
  deriving (Eq, Ord, Show)

flagsToInt :: OpenFlags -> CInt
flagsToInt (OpenFlags fWrite furi fmemory fthread fcache fnoFollow)
  = 0
  .|. writeModeToInt fWrite
  .|. iff furi #{const SQLITE_OPEN_URI}
  .|. iff fmemory #{const SQLITE_OPEN_MEMORY}
  .|. threadModeToInt fthread
  .|. cacheModeToInt fcache
  .|. iff fnoFollow #{const SQLITE_OPEN_NOFOLLOW}
  where
    iff b n = if b then n else 0

cacheModeToInt :: CacheMode -> CInt
cacheModeToInt c = case c of
  Shared -> #{const SQLITE_OPEN_SHAREDCACHE}
  Private -> #{const SQLITE_OPEN_PRIVATECACHE}

threadModeToInt :: ThreadMode -> CInt
threadModeToInt t = case t of
  MultiThread -> #{const SQLITE_OPEN_NOMUTEX}
  Serialized -> #{const SQLITE_OPEN_FULLMUTEX}

writeModeToInt :: WriteMode -> CInt
writeModeToInt w = case w of
  ReadOnly -> #{const SQLITE_OPEN_READONLY}
  ReadWrite c -> #{const SQLITE_OPEN_READWRITE} .|. case c of
    Create -> #{const SQLITE_OPEN_CREATE}
    NoCreate -> 0

-- | Returns a 'SQLData' that indicates only the data type
-- corresponding to the given constant.  Any fields in the 'SQLData' will
-- be 'undefined'.
convertCColumnType :: Integral a => a -> Maybe SQLData
convertCColumnType x = case x of
  #{const SQLITE_INTEGER} -> Just $ SQLInteger undefined
  #{const SQLITE_FLOAT} -> Just $ SQLFloat undefined
  #{const SQLITE_BLOB} -> Just $ SQLBlob undefined
  #{const SQLITE_NULL} -> Just SQLNull
  #{const SQLITE_TEXT} -> Just $ SQLText undefined
  _ -> Nothing
