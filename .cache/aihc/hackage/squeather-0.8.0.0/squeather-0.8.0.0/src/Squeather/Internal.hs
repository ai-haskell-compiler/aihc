{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}

module Squeather.Internal where

import qualified Control.Exception as Exception
import Control.Exception (throwIO)
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Foreign
import Foreign.C.Types (CInt(CInt), CChar, CUChar)
import Foreign (Ptr, FunPtr, ForeignPtr)

import Squeather.Internal.Bindings (SQLData(SQLNull, SQLText, SQLFloat, SQLInteger, SQLBlob))
import qualified Squeather.Internal.Bindings as Bindings
import Squeather.Internal.Types (ErrorFlag, StepResult, OpenFlags)
import qualified Squeather.Internal.Types as Types

-- | SQLite3 database handle
data C'sqlite3

-- | SQLite3 statement handle
data C'sqlite3_stmt

-- | Void
data C'void

-- | Database handle.  To create a database handle, use 'open'.
-- The resources behind the handle are automatically destroyed when
-- there are no remaining references to the 'Database', so Squeather
-- provides no @close@ function.
data Database = Database
  { dbPointer :: ForeignPtr C'sqlite3
  , dbFilename :: Text
  -- ^ Used only for error messages.
  } deriving (Eq, Ord, Show)

-- | Statement handle.  To create a statement handle, use 'prepare'.
-- The resources behind the Statement are automatically destroyed
-- when there are no remaining references to the 'Statement', so
-- Squeather provides no @finalize@ function.
data Statement = Statement
  { stmtPointer :: ForeignPtr C'sqlite3_stmt
  , stmtSql :: Text
  -- ^ SQL used to make the statement
  , stmtDb :: Database
  -- ^ Database used to make this statement
  } deriving (Eq, Ord, Show)

-- | Errors produced by the Squeather library (as opposed to being
-- caused directly by the underlying SQLite3 C library.)
data SqueatherErrorFlag
  = ParameterNotFound
  -- ^ Named parameter for SQL statement not found
  | ExecFailed
  -- ^ The 'exec' function found an error string
  | IntConversion
  -- ^ Failed to convert an 'Int' to a 'CInt' or vice-versa because
  -- the values were out of range.
  | UnknownColumnType CInt
  -- ^ 'sqlite3_column_type' returned a type Squeather didn't
  -- identify.
  | UnknownSqliteError CInt
  -- ^ SQLite returned an error code that is uknown to Squeather.
  | IncompleteBackup
  -- ^ A backup was started, but it did not finish running.
  | Bug
  -- ^ These failures should never happen and indicate a bug in
  -- Squeather.
  | ColumnNameNull Int
  -- ^ The call to 'sqlite3_column_name' returned a null pointer.

  deriving (Eq, Ord, Show)

-- | Exceptions.  Squeather indicates all errors (even those arising
-- from possible bugs) by throwing exceptions of this type.
data Error = Error
  { errorContext :: Text
  -- ^ Gives a context where this error occured, such as a SELECT
  -- query or a filename passed to @open@.
  , errorFlag :: Either ErrorFlag SqueatherErrorFlag
  -- ^ Either the error flag returned by SQLite library, or the flag
  -- produced by this library.
  , errorText :: Text
  -- ^ The text description of the error, as returned by SQLite or
  -- as created by Squeather.
  , errorFilename :: Text
  -- ^ The filename of the database giving rise to the error.
  } deriving (Eq, Ord, Show)

instance Exception.Exception Error

-- | <https://www.sqlite.org/c3ref/extended_result_codes.html>
foreign import ccall unsafe "sqlite3_extended_result_codes" sqlite3_extended_result_codes
  :: Ptr C'sqlite3
  -> Int
  -- ^ On or off
  -> IO CInt

-- | <https://www.sqlite.org/c3ref/open.html>
foreign import ccall unsafe "sqlite3_open_v2" sqlite3_open_v2
  :: Ptr CChar
  -- ^ Database filename, UTF-8
  -> Ptr (Ptr C'sqlite3)
  -- ^ OUT: SQLite db handle
  -> CInt
  -- ^ Flags
  -> Ptr CChar
  -- ^ VFS module to use
  -> IO CInt

-- | <https://www.sqlite.org/c3ref/errcode.html>
foreign import ccall unsafe "sqlite3_errmsg" sqlite3_errmsg
  :: Ptr C'sqlite3
  -> IO (Ptr CChar)

-- | Reads a UTF-8 text.
readUtf8 :: Ptr CChar -> IO Text
readUtf8 cstr = do
  bs <- ByteString.packCString cstr
  return . Encoding.decodeUtf8 $ bs

-- | Writes a UTF-8 text for foreign function use.
writeUtf8 :: Text -> (Ptr CChar -> IO a) -> IO a
writeUtf8 txt cback = do
  let bs = Encoding.encodeUtf8 txt
  ByteString.useAsCString bs cback

-- | Like 'writeUtf8' but instead returns a CStringLen.
writeUtf8Len :: Text -> ((Ptr CChar, Int) -> IO a) -> IO a
writeUtf8Len txt cback = do
  let bs = Encoding.encodeUtf8 txt
  ByteString.useAsCStringLen bs cback


-- | Checks SQLite return code.  Throws an exception if the code is
-- an error.  Otherwise, returns successfully.  Do not use this
-- function if checking the return code from a function such as
-- @sqlite3_step@; instead, use 'checkStepError'.
checkError
  :: Database
  -> Text
  -- ^ Context
  -> CInt
  -> IO ()
checkError (Database dbFp dbFn) ctx err = case Bindings.parseError err of
  Bindings.ParseErrorOk -> return ()
  Bindings.ParseErrorStep _ -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Right Bug
    , errorText = "Squeather.checkError: returned StepResult - should never happen"
    , errorFilename = dbFn
    }
  Bindings.ParseErrorError flg -> Foreign.withForeignPtr dbFp $ \db -> do
    ptrMsg <- sqlite3_errmsg db
    errMsg <- readUtf8 ptrMsg
    Exception.throwIO $ Error ctx (Left flg) errMsg dbFn
  Bindings.ParseErrorNotFound -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Right $ UnknownSqliteError err
    , errorText = Text.pack $ "Squeather.checkError: returned unknown error code " ++ show err
    , errorFilename = dbFn
    }

-- | Like 'checkError' but for use only when using
-- @sqlite3_initialize@.
checkInitError
  :: Text
  -- ^ Database filename
  -> CInt
  -> IO ()
checkInitError fn err = case Bindings.parseError err of
  Bindings.ParseErrorOk -> return ()
  Bindings.ParseErrorStep _ -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Right Bug
    , errorText = "Squeather.checkInitError: returned StepResult - should never happen"
    , errorFilename = fn
    }
  Bindings.ParseErrorError res -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Left res
    , errorText = Text.pack
      $ "Squeather.checkInitError: returned error code " ++ show res
    , errorFilename = fn
    }
  Bindings.ParseErrorNotFound -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Right $ UnknownSqliteError err
    , errorText = Text.pack
      $ "Squeather.checkInitError: returned unknown error code " ++ show err
    , errorFilename = fn
    }
  where
    ctx = "when initializing SQLite library"

-- | Like 'checkError' but for use when using @sqlite3_step@.
checkStepError
  :: Database
  -> Text
  -- ^ Context
  -> CInt
  -> IO StepResult
checkStepError (Database dbFp dbName) ctx err = case Bindings.parseError err of
  Bindings.ParseErrorOk -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Right Bug
    , errorText = "Squeather.checkStepError: returned SQLITE_OK - should never happen"
    , errorFilename = dbName
    }
  Bindings.ParseErrorStep r -> return r
  Bindings.ParseErrorError flag -> Foreign.withForeignPtr dbFp $ \db -> do
    ptrMsg <- sqlite3_errmsg db
    errMsg <- readUtf8 ptrMsg
    Exception.throwIO $ Error ctx (Left flag) errMsg dbName
  Bindings.ParseErrorNotFound -> Exception.throwIO $ Error
    { errorContext = ctx
    , errorFlag = Right $ UnknownSqliteError err
    , errorText = Text.pack $ "Squeather.checkStepError: returned unknown error code " ++ show err
    , errorFilename = dbName
    }

-- | Opens a new 'Database'.  The 'openFlags' are used.
open
  :: Text
  -- ^ Database filename
  -> IO Database
open = openWithFlags openFlags

-- | Opens a new 'Database', with settings specified with
-- 'openFlags'.
openWithFlags
  :: OpenFlags
  -> Text
  -- ^ Database filename
  -> IO Database
openWithFlags flags fn
  = writeUtf8 fn $ \fnUtf8 ->
  Foreign.alloca $ \ptrIn ->
  Foreign.poke ptrIn Foreign.nullPtr >>
  let acq = sqlite3_open_v2 fnUtf8 ptrIn (Bindings.flagsToInt flags) Foreign.nullPtr
      rel _ = Foreign.peek ptrIn >>= sqlite3_close_v2
      use code = do
        sqlite3 <- Foreign.peek ptrIn
        fp <- Foreign.newForeignPtr p_squeather_close_v2 sqlite3
        return (fp, code)
  in do
      sqlite3_initialize >>= checkInitError fn
      (fp, code) <- Exception.bracketOnError acq rel use
      let db = Database fp fn
      checkError db "opening database" code
      Foreign.withForeignPtr fp $ \ptrDb ->
        sqlite3_extended_result_codes ptrDb 1
          >>= checkError db "setting extended result codes"
      return db


-- | <https://www.sqlite.org/c3ref/prepare.html>
foreign import ccall unsafe "sqlite3_prepare_v2" sqlite3_prepare_v2
  :: Ptr C'sqlite3
  -- ^ Database handle
  -> Ptr CChar
  -- ^ SQL Statement, UTF-8
  -> CInt
  -- ^ Length of SQL statement in bytes
  -> Ptr (Ptr C'sqlite3_stmt)
  -- ^ OUT Statement handle
  -> Ptr (Ptr CChar)
  -- ^ OUT unused portion of input statement
  -> IO CInt

-- | Prepares a statement.  The corresponding C SQLite function
-- allows you to pass in a multi-statement SQL text, and retrieve
-- the unused portion for later use.  Squeather does not allow this.
-- Squeather will prepare only the first statement.
prepare
  :: Database
  -- ^ Database handle
  -> Text
  -- ^ SQL Statement, UTF-8
  -> IO Statement
prepare db@(Database dbFp dbFn) sql
  = writeUtf8Len sql $ \(sqlUtf8, sqlLen) ->
  Foreign.alloca $ \ptrIn ->
  Foreign.withForeignPtr dbFp $ \dbPtr -> do
    Foreign.poke ptrIn Foreign.nullPtr
    sqlLenCInt <- intToCInt sql dbFn sqlLen
    let acq = sqlite3_prepare_v2 dbPtr sqlUtf8 sqlLenCInt ptrIn Foreign.nullPtr
        rel _ = Foreign.peek ptrIn >>= sqlite3_finalize
        use code = do
          ptrStmt <- Foreign.peek ptrIn
          fp <- Foreign.newForeignPtr p_squeather_finalize ptrStmt
          checkError db sql code
          return $ Statement fp sql db
    Exception.bracketOnError acq rel use

-- | <https://www.sqlite.org/c3ref/bind_parameter_index.html>
foreign import ccall unsafe "sqlite3_bind_parameter_index" sqlite3_bind_parameter_index
  :: Ptr C'sqlite3_stmt
  -- ^ Statement
  -> Ptr CChar
  -- ^ Parameter name
  -> IO CInt
  -- ^ The index of the parameter.  Returns 0 if no matching
  -- parameter is found.

-- | Gets the index of the parameter that has the given name.
-- Throws an 'Error' with 'ParameterNotFound' if the given parameter
-- name does not exist for this statement.
getParameterIndex
  :: Statement
  -> Text
  -- ^ Look up the parameter with this name.
  -> IO CInt
getParameterIndex (Statement stFp stSql (Database _ dbFn)) param
  = writeUtf8 param $ \paramUtf8 ->
    Foreign.withForeignPtr stFp $ \stPtr -> do
      idx <- sqlite3_bind_parameter_index stPtr paramUtf8
      if idx == 0
        then throwIO $ Error stSql (Right ParameterNotFound)
                ("parameter not found: " <> param) dbFn
        else return idx

-- | <https://www.sqlite.org/c3ref/bind_blob.html>
foreign import ccall safe "sqlite3_bind_blob" sqlite3_bind_blob
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> Ptr a
  -- ^ Blob
  -> CInt
  -- ^ Length
  -> FunPtr (Ptr a -> IO ())
  -- ^ Callback to dispose of the blob.  Use @SQLITE_STATIC@ if the
  -- blob is in static, unmanaged space and does not need to be
  -- freed.  Use @SQLITE_TRANSIENT@ to have SQLite make its own
  -- private copy of the data immediately.
  -> IO CInt

bindBlob
  :: Statement
  -> Text
  -- ^ Parameter name
  -> ByteString
  -- ^ Blob
  -> IO ()
bindBlob st@(Statement stFp sSql db) paramName blob
  = ByteString.useAsCStringLen blob $ \(ptrBlob, blobLen) -> 
  Foreign.withForeignPtr stFp $ \sPtr -> do
    idx <- getParameterIndex st paramName
    let transient = Foreign.castPtrToFunPtr . Foreign.intPtrToPtr
          $ Bindings.c'SQLITE_TRANSIENT
    blobLenCInt <- intToCInt sSql (dbFilename db) blobLen
    rslt <- sqlite3_bind_blob sPtr idx ptrBlob blobLenCInt transient
    checkError db sSql rslt

-- | <https://www.sqlite.org/c3ref/bind_blob.html>
foreign import ccall unsafe "sqlite3_bind_double" sqlite3_bind_double
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> Double
  -- ^ Double to bind
  -> IO CInt

bindDouble
  :: Statement
  -> Text
  -- ^ Parameter name
  -> Double
  -> IO ()
bindDouble st@(Statement stFp sSql db) paramName dbl =
  Foreign.withForeignPtr stFp $ \sPtr -> do
    idx <- getParameterIndex st paramName
    rslt <- sqlite3_bind_double sPtr idx dbl
    checkError db sSql rslt

-- | <https://www.sqlite.org/c3ref/bind_blob.html>
foreign import ccall unsafe "sqlite3_bind_int64" sqlite3_bind_int64
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> Int64
  -> IO CInt

bindInt64
  :: Statement
  -> Text
  -- ^ Parameter name
  -> Int64
  -> IO ()
bindInt64 st@(Statement stFp sSql db) paramName int64 =
  Foreign.withForeignPtr stFp $ \sPtr -> do
    idx <- getParameterIndex st paramName
    rslt <- sqlite3_bind_int64 sPtr idx int64
    checkError db sSql rslt

-- | <https://www.sqlite.org/c3ref/bind_blob.html>
foreign import ccall unsafe "sqlite3_bind_null" sqlite3_bind_null
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> IO CInt

bindNull
  :: Statement
  -> Text
  -- ^ Parameter name
  -> IO ()
bindNull st@(Statement stFp sSql db) paramName =
  Foreign.withForeignPtr stFp $ \sPtr -> do
    idx <- getParameterIndex st paramName
    rslt <- sqlite3_bind_null sPtr idx
    checkError db sSql rslt

-- | <https://www.sqlite.org/c3ref/bind_blob.html>
foreign import ccall unsafe "sqlite3_bind_text" sqlite3_bind_text
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> Ptr CChar
  -- ^ UTF-8 text
  -> CInt
  -- ^ Length
  -> FunPtr (Ptr a -> IO ())
  -- ^ Callback to dispose of the string.  Use @SQLITE_STATIC@ if the
  -- string is in static, unmanaged space and does not need to be
  -- freed.  Use @SQLITE_TRANSIENT@ to have SQLite make its own
  -- private copy of the data immediately.
  -> IO CInt

bindText
  :: Statement
  -> Text
  -- ^ Parameter name
  -> Text
  -- ^ Text to bind
  -> IO ()
bindText st@(Statement stFp sSql db) paramName txt
  = writeUtf8Len txt $ \(ptrTxt, txtLen) ->
  Foreign.withForeignPtr stFp $ \sPtr -> do
    idx <- getParameterIndex st paramName
    let transient = Foreign.castPtrToFunPtr . Foreign.intPtrToPtr
          $ Bindings.c'SQLITE_TRANSIENT
    txtLenCInt <- intToCInt sSql (dbFilename db) txtLen
    rslt <- sqlite3_bind_text sPtr idx ptrTxt txtLenCInt transient
    checkError db sSql rslt

-- | Binds a parameter with given SQL data to the given 'Statement'.
bindSqlData
  :: Statement
  -> Text
  -- ^ Parameter name
  -> SQLData
  -> IO ()
bindSqlData st name sqld = case sqld of
  SQLNull -> bindNull st name
  SQLText txt -> bindText st name txt
  SQLFloat dbl -> bindDouble st name dbl
  SQLInteger i64 -> bindInt64 st name i64
  SQLBlob blob -> bindBlob st name blob

-- | <https://www.sqlite.org/c3ref/step.html>
foreign import ccall unsafe "sqlite3_step" sqlite3_step
  :: Ptr C'sqlite3_stmt
  -> IO CInt

-- | Evaluate a prepared statement.  Returns 'Types.Row' if the
-- 'Statement' has returned a row of data.  In that case, use
-- 'column' or 'columns' to get individual columns or all columns,
-- respectively.  Returns 'Types.Done' if there is no data to retrieve.
-- In that case, 'step' should not be called again without first
-- calling 'reset'.
step :: Statement -> IO StepResult
step (Statement stFp sSql db) =
  Foreign.withForeignPtr stFp $ \sPtr -> do
    rslt <- sqlite3_step sPtr
    checkStepError db sSql rslt

-- | The number of columns returned by the prepared statement.  Can
-- be zero.  However, just because this routine returns a positive
-- number does not mean that data will be returned.  A @SELECT@
-- statement will always return a postive column count, but a
-- particular query might return no rows.
--
-- <https://www.sqlite.org/c3ref/column_count.html>
foreign import ccall unsafe "sqlite3_column_count" sqlite3_column_count
  :: Ptr C'sqlite3_stmt
  -> IO CInt

-- | <https://www.sqlite.org/c3ref/column_blob.html>
foreign import ccall unsafe "sqlite3_column_bytes" sqlite3_column_bytes
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Column index
  -> IO CInt
  -- ^ Number of bytes in the column

-- | <https://www.sqlite.org/c3ref/column_blob.html>
foreign import ccall unsafe "sqlite3_column_type" sqlite3_column_type
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> IO CInt

-- | <https://www.sqlite.org/c3ref/column_blob.html>
foreign import ccall unsafe "sqlite3_column_blob" sqlite3_column_blob
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> IO (Ptr a)
  -- ^ Pointer to result

-- | <https://www.sqlite.org/c3ref/column_blob.html>
foreign import ccall unsafe "sqlite3_column_double" sqlite3_column_double
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> IO Double

-- | <https://www.sqlite.org/c3ref/column_blob.html>
foreign import ccall unsafe "sqlite3_column_int64" sqlite3_column_int64
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> IO Int64

-- | <https://www.sqlite.org/c3ref/column_blob.html>
foreign import ccall unsafe "sqlite3_column_text" sqlite3_column_text
  :: Ptr C'sqlite3_stmt
  -> CInt
  -- ^ Index
  -> IO (Ptr CUChar)

-- | Retrieves a column with a given index from the 'Statement'.
-- Assumes that 'step' was already called and that it returned
-- 'Row'.
column
  :: Statement
  -> Int
  -- ^ Index
  -> IO SQLData
column (Statement stFp sSql db) intIdx =
  Foreign.withForeignPtr stFp $ \stPtr -> do
    idx <- intToCInt sSql (dbFilename db) intIdx
    colTypeNum <- sqlite3_column_type stPtr idx
    colType <- case Bindings.convertCColumnType colTypeNum of
      Just n -> return n
      Nothing -> Exception.throwIO $ Error
        { errorContext = sSql
        , errorFlag = Right $ UnknownColumnType colTypeNum
        , errorText = "Unknown column type found"
        , errorFilename = dbFilename db
        }
    case colType of
      SQLNull -> return SQLNull
      SQLFloat _ -> fmap SQLFloat $ sqlite3_column_double stPtr idx
      SQLBlob _ -> do
        resPtr <- sqlite3_column_blob stPtr idx
        resLen <- sqlite3_column_bytes stPtr idx
        resLenInt <- intFromCInt sSql (dbFilename db) resLen
        bs <- ByteString.packCStringLen (resPtr, resLenInt)
        return $ SQLBlob bs
      SQLInteger _ -> fmap SQLInteger $ sqlite3_column_int64 stPtr idx
      SQLText _ -> do
        resPtr <- sqlite3_column_text stPtr idx
        resLen <- sqlite3_column_bytes stPtr idx
        resLenInt <- intFromCInt sSql (dbFilename db) resLen
        bs <- ByteString.packCStringLen (Foreign.castPtr resPtr, resLenInt)
        return . SQLText . Encoding.decodeUtf8 $ bs

-- | The number of columns that a given 'Statement' will return.
-- Works regardless of whether 'step' has been applied or not;
-- however, just because this returns a positive value does not mean
-- that 'step' will ever actually return a 'Row'.
columnCount :: Statement -> IO Int
columnCount (Statement stFp sSql db)
  = Foreign.withForeignPtr stFp $ \stPtr ->
  sqlite3_column_count stPtr >>= intFromCInt sSql (dbFilename db)

-- | Return all available columns, in order, from a 'Statement' on
-- which 'step' returned 'Row'.  You should already have applied
-- 'step'.
columns :: Statement -> IO [SQLData]
columns st = do
  nCols <- columnCount st
  mapM (column st) [0 .. nCols - 1]

-- | Retrieves all remaining rows from a 'Statement'.  Applies
-- 'step' for you for as many times as needed.
allRows :: Statement -> IO [[SQLData]]
allRows st = do
  r <- step st
  case r of
    Types.Done -> return []
    Types.Row -> do
      cols <- columns st
      rest <- allRows st
      return $ cols : rest

-- | Bind multiple named parameters to a 'Statement'.  
bindParams
  :: Statement
  -> [(Text, SQLData)]
  -> IO ()
bindParams st = mapM_ (uncurry (bindSqlData st))

-- | Execute a query without any parameters.  Executes only one
-- query - there is no need to terminate it with a semicolon,
-- although you can.  If you use a semicolon-separated list of
-- queries, only the first query will be run.  There is no way to
-- use SQL parameters; for that you will need 'executeNamed'.
execute
  :: Database
  -> Text
  -- ^ SQL text
  -> IO [[SQLData]]
  -- ^ All SQL data from the query.
execute db sql = prepare db sql >>= allRows

-- | Execute a query with named parameters.  Executes only one
-- query - there is no need to terminate it with a semicolon,
-- although you can.  If you use a semicolon-separated list of
-- queries, only the first query will be run.
executeNamed
  :: Database
  -> Text
  -- ^ SQL text
  -> [(Text, SQLData)]
  -- ^ Pairs, where each 'Text' is a named parameter and each
  -- 'SQLData' is the corresponding data to bind to that parameter.
  -- [This page](https://www.sqlite.org/c3ref/bind_blob.html)
  -- describes the different parameter syntax that is allowed.
  -- Squeather makes no effort to support the plain @?@ syntax.  Note
  -- that the leading mark (@?@, @:@, @\@@, or @\$@) is part of the
  -- parameter name and must appear as part of the 'Text'.
  -> IO [[SQLData]]
  -- ^ All SQL data from the query.
executeNamed db sql params = prepare db sql >>= use
  where
    use stmt = do
      bindParams stmt params
      allRows stmt

-- | Like 'executeNamed' but also returns the names of the columns
-- in addition to the SQL results.
executeNamedWithColumns
  :: Database
  -> Text
  -- ^ SQL text
  -> [(Text, SQLData)]
  -- ^ Pairs, where each 'Text' is a named parameter and each
  -- 'SQLData' is the corresponding data to bind to that parameter.
  -- [This page](https://www.sqlite.org/c3ref/bind_blob.html)
  -- describes the different parameter syntax that is allowed.
  -- Squeather makes no effort to support the plain @?@ syntax.  Note
  -- that the leading mark (@?@, @:@, @\@@, or @\$@) is part of the
  -- parameter name and must appear as part of the 'Text'.
  -> IO ([Text], [[SQLData]])
  -- ^ The column names, and all SQL data from the query.
executeNamedWithColumns db sql params = prepare db sql >>= use
  where
    use stmt = do
      bindParams stmt params
      rows <- allRows stmt
      names <- columnNames stmt
      return (names, rows)


-- | <https://www.sqlite.org/c3ref/reset.html>
foreign import ccall unsafe "sqlite3_reset" sqlite3_reset
  :: Ptr C'sqlite3_stmt
  -> IO CInt

-- | Resets a 'Statement' so it may be re-executed.  Does not clear
-- bindings.  In SQLite, 'sqlite3_reset' returns an error code if
-- the most recent step statement returned an error.  'reset' does
-- not do this.  It does not check the error code returned by
-- 'sqlite3_reset'.
reset :: Statement -> IO ()
reset (Statement stFp _ _) = Foreign.withForeignPtr stFp $ \stPtr ->
  sqlite3_reset stPtr >> return ()

-- | <https://www.sqlite.org/c3ref/clear_bindings.html>
foreign import ccall unsafe "sqlite3_clear_bindings" sqlite3_clear_bindings
  :: Ptr C'sqlite3_stmt
  -> IO CInt

-- | Clears all bindings on the 'Statement'.
clearBindings :: Statement -> IO ()
clearBindings (Statement stFp _ db)
  -- Checks the error code, but in SQLite version 3.31.1,
  -- sqlite3_clear_bindings will only ever return SQLITE_OK
  = Foreign.withForeignPtr stFp $ \stPtr ->
  sqlite3_clear_bindings stPtr >>= checkError db "clearing bindings"

foreign import ccall unsafe "sqlite3_finalize" sqlite3_finalize
  :: Ptr C'sqlite3_stmt
  -> IO CInt

foreign import ccall unsafe "&squeather_finalize" p_squeather_finalize
  :: FunPtr (Ptr C'sqlite3_stmt -> IO ())

-- | <https://www.sqlite.org/c3ref/close.html>
foreign import ccall unsafe "sqlite3_close_v2" sqlite3_close_v2
  :: Ptr C'sqlite3
  -> IO CInt

foreign import ccall unsafe "&squeather_close_v2" p_squeather_close_v2
  :: FunPtr (Ptr C'sqlite3 -> IO ())

-- | The type of the callback from 'sqlite3_exec'.  This callback is
-- invoked for every row of data.
type ExecCallback a
  = Ptr a
  -- ^ The fourth argument of 'sqlite3_exec' is passed through here.
  -> CInt
  -- ^ The number of columns in the result
  -> Ptr (Ptr CChar)
  -- ^ An array of pointers to strings obtained as if from
  -- @sqlite3_column_text@
  -> Ptr (Ptr CChar)
  -- ^ An array of pointers to strings where each entry represents
  -- the name of the corresponding result column as obtained from
  -- @sqlite3_column_name@
  -> IO CInt
  -- ^ The function should return zero if successful.  If it returns
  -- non-zero, then 'SQLITE_ABORT' will be thrown without involking
  -- the callback again and without running any more SQL statements.

-- | <https://www.sqlite.org/c3ref/exec.html>
foreign import ccall "sqlite3_exec" sqlite3_exec
  :: Ptr C'sqlite3
  -> Ptr CChar
  -- ^ SQL
  -> FunPtr (ExecCallback a)
  -- ^ Callback.  Pass 'Foreign.nullFunPtr' if you do not with to use
  -- a callback.
  -> Ptr a
  -- ^ Passed to callback for every row
  -> Ptr (Ptr CChar)
  -- ^ If there is a failure, the error message is written here.  If
  -- there is no failure, 'Foreign.nullPtr' is written here.
  -> IO CInt

-- | <https://www.sqlite.org/c3ref/free.html>
foreign import ccall unsafe "sqlite3_free" sqlite3_free
  :: Ptr a
  -> IO ()

-- | Evaluate one or more SQL statements.  There is no way to obtain
-- the results; for that you will need 'execute' or 'executeNamed'.
-- There is no way to use SQL parameters; for that you will need
-- 'executeNamed'.
exec
  :: Database
  -> Text
  -- ^ SQL to be evaluated.  Multiple, semicolon-separated
  -- statements will be executed.
  -> IO ()
exec db@(Database dbFp dbFn) sqlTxt =
  writeUtf8 sqlTxt $ \ptrSql ->
  Foreign.withForeignPtr dbFp $ \dbPtr ->
  Foreign.alloca $ \strErr -> do
    Foreign.poke strErr Foreign.nullPtr
    let cleanup = Foreign.peek strErr >>= sqlite3_free
        runExec = do
          code <- sqlite3_exec dbPtr ptrSql Foreign.nullFunPtr Foreign.nullPtr strErr
          errVal <- Foreign.peek strErr
          when (errVal /= Foreign.nullPtr) $ do
            errTxt <- readUtf8 errVal
            Exception.throwIO $ Error sqlTxt (Right ExecFailed) errTxt dbFn
          checkError db sqlTxt code
    Exception.finally runExec cleanup

-- | <https://www.sqlite.org/c3ref/last_insert_rowid.html>
foreign import ccall unsafe "sqlite3_last_insert_rowid" sqlite3_last_insert_rowid
  :: Ptr C'sqlite3
  -> IO Int64

-- | Get the rowid of the most recent successful INSERT.
lastInsertRowId :: Database -> IO Int64
lastInsertRowId (Database dbFp _) =
  Foreign.withForeignPtr dbFp sqlite3_last_insert_rowid

-- | Convert from an Int to a CInt.  Makes sure the conversion fits
-- in the space allotted.  Throws an exception if it doesn't fit.
intToCInt
  :: Text
  -- ^ Context.  For error messages only.
  -> Text
  -- ^ Database filename.  For error messages only.
  -> Int
  -> IO CInt
intToCInt ctx fn i
  | iConv > fromIntegral (maxBound :: CInt)
      = throw . Text.pack $ "number too big to convert to CInt: " ++ show i
  | iConv < fromIntegral (minBound :: CInt)
      = throw . Text.pack $ "number too small to convert to CInt: " ++ show i
  | otherwise = return $ fromIntegral i
  where
    iConv = fromIntegral i :: Integer
    throw str = Exception.throwIO exc
      where
        exc = Error { errorContext = ctx
                    , errorFlag = Right IntConversion
                    , errorText = str
                    , errorFilename = fn
                    }

-- | Convert from an CInt to a Int.  Makes sure the conversion fits
-- in the space allotted.  Throws an exception if it doesn't fit.
intFromCInt
  :: Text
  -- ^ Context.  For error messages only.
  -> Text
  -- ^ Database filename.  For error messages only.
  -> CInt
  -> IO Int
intFromCInt ctx fn i
  | iConv > fromIntegral (maxBound :: Int)
    = throw . Text.pack $ "number too big to convert to Int: " ++ show i
  | iConv < fromIntegral (minBound :: Int)
    = throw . Text.pack $ "number too small to convert to Int: " ++ show i
  | otherwise = return $ fromIntegral i
  where
    iConv = fromIntegral i :: Integer
    throw str = Exception.throwIO exc
      where
        exc = Error { errorContext = ctx
                    , errorFlag = Right IntConversion
                    , errorText = str
                    , errorFilename = fn
                    }


-- | Returns a string which is the version number for SQLite used to
-- build this library.  SQLite is embedded into the library, so the
-- only way to change the SQLite version is to recompile the
-- library.
sqliteVersion :: String
sqliteVersion = Bindings.c'SQLITE_VERSION

-- | Default settings for 'OpenFlags', where the 'Types.writeMode'
-- is 'Types.ReadWrite' 'Types.Create', 'Types.threadMode' is
-- 'Types.Serialized', 'Types.cacheMode' is 'Types.Private', and all
-- other flags are set to False.
openFlags :: OpenFlags
openFlags = Types.OpenFlags
  { Types.writeMode = Types.ReadWrite Types.Create
  , Types.uri = False
  , Types.memory = False
  , Types.threadMode = Types.Serialized
  , Types.cacheMode = Types.Private
  , Types.noFollow = False
  }

-- Backup API

data C'sqlite3_backup

-- | <https://www.sqlite.org/c3ref/backup_finish.html>
foreign import ccall unsafe "sqlite3_backup_init" sqlite3_backup_init
  :: Ptr C'sqlite3
  -- ^ Destination database handle
  -> Ptr CChar
  -- ^ Destination database name - @main@ for the main database,
  -- @temp@ for the temporary database, or the name specified after
  -- the @AS@ keyword in an @ATTACH@ statement for an attached
  -- database.
  -> Ptr C'sqlite3
  -- ^ Source database handle
  -> Ptr CChar
  -- ^ Source database name
  -> IO (Ptr C'sqlite3_backup)
  -- ^ Returns pointer to backup object

-- | <https://www.sqlite.org/c3ref/backup_finish.html>
foreign import ccall unsafe "sqlite3_backup_step" sqlite3_backup_step
  :: Ptr C'sqlite3_backup
  -> CInt
  -- ^ Number of pages.  If negative, copy all remaining source
  -- pages.
  -> IO CInt
  -- ^ Returns error code

-- | <https://www.sqlite.org/c3ref/backup_finish.html>
foreign import ccall unsafe "sqlite3_backup_finish" sqlite3_backup_finish
  :: Ptr C'sqlite3_backup
  -> IO CInt
  -- ^ Returns error code

-- | <https://www.sqlite.org/c3ref/backup_finish.html>
foreign import ccall unsafe "sqlite3_backup_remaining" sqlite3_backup_remaining
  :: Ptr C'sqlite3_backup
  -> IO CInt
  -- ^ Returns number of pages remaining to be backed up

-- | <https://www.sqlite.org/c3ref/backup_finish.html>
foreign import ccall unsafe "sqlite3_backup_pagecount" sqlite3_backup_pagecount
  :: Ptr C'sqlite3_backup
  -> IO CInt
  -- ^ Returns number of pages in source database

-- | Backup source
data Source = Source
  { sourceConnection :: Database
  , sourceName :: Text
  -- ^ The name for the source database.  Use @main@ for the
  -- main database, @temp@ for the temporary database, or the name
  -- specified after the @AS@ keyword in an @ATTACH@ statement for
  -- an attached database.
  } deriving (Eq, Ord, Show)

-- | Backup destination
data Destination = Destination
  { destConnection :: Database
  , destName :: Text
  -- ^ The name for the destination database.  Use @main@ for the
  -- main database, @temp@ for the temporary database, or the name
  -- specified after the @AS@ keyword in an @ATTACH@ statement for
  -- an attached database.
  } deriving (Eq, Ord, Show)

-- | Use the SQLite backup API to copy the content of one database
-- to another.  Can be used to safely copy databases while they are
-- in use, or to copy in-memory databases to or from persistent
-- files.
backup :: Source -> Destination -> IO ()
backup src dest =
  Foreign.withForeignPtr (dbPointer . destConnection $ dest) $ \ptrDestDb ->
  Foreign.withForeignPtr (dbPointer . sourceConnection $ src) $ \ptrSrcDb ->
  writeUtf8 (sourceName src) $ \ptrSrcName ->
  writeUtf8 (destName dest) $ \ptrDestName ->
  let
    acq = sqlite3_backup_init ptrDestDb ptrDestName ptrSrcDb ptrSrcName
    rel = sqlite3_backup_finish
    use bkpPtr = do
      code <- sqlite3_backup_step bkpPtr (-1)
      case Bindings.parseError code of
        Bindings.ParseErrorStep Types.Done -> return ()
        Bindings.ParseErrorOk -> Exception.throwIO $ Error
          { errorContext = ctx
          , errorFlag = Right IncompleteBackup
          , errorText = "Squeather.backup: backup did not complete"
          , errorFilename = ctx
          }
        Bindings.ParseErrorStep Types.Row -> Exception.throwIO $ Error
          { errorContext = ctx
          , errorFlag = Right Bug
          , errorText = "Squeather.backup: returned Row StepResult - should never happen"
          , errorFilename = ctx
          }
        Bindings.ParseErrorError flg -> Exception.throwIO $ Error
          { errorContext = ctx
          , errorFlag = Left flg
          , errorText = "Squeather.backup: error during backup"
          , errorFilename = ctx
          }
        Bindings.ParseErrorNotFound -> Exception.throwIO $ Error
          { errorContext = ctx
          , errorFlag = Right $ UnknownSqliteError code
          , errorText = "Squeather.backup: error during backup - code not found"
          , errorFilename = ctx
          }
    ctx = "during backup from " <> dbFilename (sourceConnection src) <> " to "
      <> dbFilename (destConnection dest)

  in Exception.bracket acq rel use

-- | <https://www.sqlite.org/c3ref/changes.html>
foreign import ccall unsafe "sqlite3_changes" sqlite3_changes
  :: Ptr C'sqlite3
  -> IO CInt

-- | Count the number of rows modified by the most recent @INSERT@,
-- @UPDATE@, or @DELETE@ statement.
changes :: Database -> IO Int
changes (Database dbFp dbName) =
  Foreign.withForeignPtr dbFp $ \dbPtr ->
  sqlite3_changes dbPtr >>= intFromCInt "changes" dbName

-- Column names

foreign import ccall unsafe "sqlite3_column_name" sqlite3_column_name
  :: Ptr C'sqlite3_stmt
  -> CInt
  -> IO (Ptr CChar)

-- | Gets the name of a column.  The name is the value of the @AS@
-- clause if it exists, or is an undefined string otherwise.
columnName
  :: Statement
  -> Int
  -- ^ Index.  The leftmost column is @0@.
  -> IO Text
columnName (Statement stFp stSql db) idx =
  Foreign.withForeignPtr stFp $ \stPtr -> do
    cIntIdx <- intToCInt ("getting column name in " <> stSql) (dbFilename db) idx
    ptrStr <- sqlite3_column_name stPtr cIntIdx
    if ptrStr == Foreign.nullPtr
      then throwIO $ Error
            { errorContext = stSql
            , errorFlag = Right (ColumnNameNull idx)
            , errorText = Text.pack $ "null pointer returned when getting column name for index " <> show idx
            , errorFilename = dbFilename db
            }
      else readUtf8 ptrStr

-- | Gets all column names, in order.
columnNames :: Statement -> IO [Text]
columnNames stmt = do
  i <- columnCount stmt
  mapM (columnName stmt) [0 .. (i - 1)]

-- | Returns zero if mutexing code was omitted.
foreign import ccall unsafe "sqlite3_threadsafe" sqlite3_threadsafe
  :: IO CInt

-- | Initialize the SQLite library, see
--
-- <https://www.sqlite.org/c3ref/initialize.html>
foreign import ccall unsafe "sqlite3_initialize" sqlite3_initialize
  :: IO CInt
