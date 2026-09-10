{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Discarding a result would read better as void, but Data.Functor imports
-- Prelude, which imports this module.
{-# HLINT ignore "Use void" #-}

-- | C error numbers and their IO error forms. The runtime reports POSIX
-- error numbers, so the constants use the values that Linux and macOS
-- share. WASI numbers its errors differently, so a comparison against one
-- of the constants is only meaningful on a POSIX target; this is why the
-- retrying operations below are the ones that read @eINTR@.
--
-- @errno@ itself is a macro over a thread-local location, so 'getErrno'
-- and 'resetErrno' reach it through a runtime shim rather than naming it
-- in a foreign import.
module Foreign.C.Error
  ( Errno (..),
    eOK,
    ePERM,
    eNOENT,
    eSRCH,
    eINTR,
    eIO,
    eNXIO,
    e2BIG,
    eNOEXEC,
    eBADF,
    eCHILD,
    eNOMEM,
    eACCES,
    eFAULT,
    eBUSY,
    eEXIST,
    eXDEV,
    eNODEV,
    eNOTDIR,
    eISDIR,
    eINVAL,
    eNFILE,
    eMFILE,
    eNOTTY,
    eFBIG,
    eNOSPC,
    eSPIPE,
    eROFS,
    eMLINK,
    ePIPE,
    eDOM,
    eRANGE,
    isValidErrno,
    getErrno,
    resetErrno,
    errnoToIOError,
    throwErrno,
    throwErrnoIf,
    throwErrnoIf_,
    throwErrnoIfRetry,
    throwErrnoIfRetry_,
    throwErrnoIfMinus1,
    throwErrnoIfMinus1_,
    throwErrnoIfMinus1Retry,
    throwErrnoIfMinus1Retry_,
    throwErrnoIfNull,
    throwErrnoIfNullRetry,
    throwErrnoPath,
    throwErrnoPathIf,
    throwErrnoPathIf_,
    throwErrnoPathIfNull,
    throwErrnoPathIfMinus1,
    throwErrnoPathIfMinus1_,
  )
where

import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Foreign.C.Types (CInt)
import GHC.Base (Applicative (..), Monad (..), String, (++))
import GHC.IO (FilePath, IO)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (Handle, IOError, IOErrorType (..), IOException (..), ioError)
import GHC.Num (Num (..))
import GHC.Ptr (Ptr, nullPtr)
import GHC.Real (fromIntegral)
import GHC.Show (Show (..))

newtype Errno = Errno CInt
  deriving newtype (Eq)

eOK, ePERM, eNOENT, eSRCH, eINTR, eIO, eNXIO, e2BIG, eNOEXEC, eBADF, eCHILD, eNOMEM, eACCES, eFAULT, eBUSY, eEXIST, eXDEV, eNODEV, eNOTDIR, eISDIR, eINVAL, eNFILE, eMFILE, eNOTTY, eFBIG, eNOSPC, eSPIPE, eROFS, eMLINK, ePIPE, eDOM, eRANGE :: Errno
eOK = Errno 0
ePERM = Errno 1
eNOENT = Errno 2
eSRCH = Errno 3
eINTR = Errno 4
eIO = Errno 5
eNXIO = Errno 6
e2BIG = Errno 7
eNOEXEC = Errno 8
eBADF = Errno 9
eCHILD = Errno 10
eNOMEM = Errno 12
eACCES = Errno 13
eFAULT = Errno 14
eBUSY = Errno 16
eEXIST = Errno 17
eXDEV = Errno 18
eNODEV = Errno 19
eNOTDIR = Errno 20
eISDIR = Errno 21
eINVAL = Errno 22
eNFILE = Errno 23
eMFILE = Errno 24
eNOTTY = Errno 25
eFBIG = Errno 27
eNOSPC = Errno 28
eSPIPE = Errno 29
eROFS = Errno 30
eMLINK = Errno 31
ePIPE = Errno 32
eDOM = Errno 33
eRANGE = Errno 34

isValidErrno :: Errno -> Bool
isValidErrno (Errno value) = value /= negate 1

-- | The IO error for an error number. The description is the C library
-- text for the error number.
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError location errno@(Errno code) handle name =
  case errnoDetails errno of
    (errorType, description) -> IOError handle errorType location description (Just code) name

errnoDetails :: Errno -> (IOErrorType, String)
errnoDetails errno@(Errno code)
  | errno == ePERM = (PermissionDenied, "Operation not permitted")
  | errno == eNOENT = (NoSuchThing, "No such file or directory")
  | errno == eSRCH = (NoSuchThing, "No such process")
  | errno == eINTR = (Interrupted, "Interrupted system call")
  | errno == eIO = (HardwareFault, "Input/output error")
  | errno == eNXIO = (NoSuchThing, "Device not configured")
  | errno == e2BIG = (ResourceExhausted, "Argument list too long")
  | errno == eNOEXEC = (InvalidArgument, "Exec format error")
  | errno == eBADF = (InvalidArgument, "Bad file descriptor")
  | errno == eCHILD = (NoSuchThing, "No child processes")
  | errno == eNOMEM = (ResourceExhausted, "Cannot allocate memory")
  | errno == eACCES = (PermissionDenied, "Permission denied")
  | errno == eFAULT = (OtherError, "Bad address")
  | errno == eBUSY = (ResourceBusy, "Resource busy")
  | errno == eEXIST = (AlreadyExists, "File exists")
  | errno == eXDEV = (UnsupportedOperation, "Cross-device link")
  | errno == eNODEV = (UnsupportedOperation, "Operation not supported by device")
  | errno == eNOTDIR = (InappropriateType, "Not a directory")
  | errno == eISDIR = (InappropriateType, "Is a directory")
  | errno == eINVAL = (InvalidArgument, "Invalid argument")
  | errno == eNFILE = (ResourceExhausted, "Too many open files in system")
  | errno == eMFILE = (ResourceExhausted, "Too many open files")
  | errno == eNOTTY = (IllegalOperation, "Inappropriate ioctl for device")
  | errno == eFBIG = (PermissionDenied, "File too large")
  | errno == eNOSPC = (ResourceExhausted, "No space left on device")
  | errno == eSPIPE = (UnsupportedOperation, "Illegal seek")
  | errno == eROFS = (PermissionDenied, "Read-only file system")
  | errno == eMLINK = (ResourceExhausted, "Too many links")
  | errno == ePIPE = (ResourceVanished, "Broken pipe")
  | errno == eDOM = (InvalidArgument, "Numerical argument out of domain")
  | errno == eRANGE = (UnsupportedOperation, "Result too large")
  | otherwise = (OtherError, "errno " ++ show (fromIntegral code :: Int))
  where
    otherwise = True

foreign import ccall unsafe "aihc_errno_get"
  errnoGet :: IO Int

foreign import ccall unsafe "aihc_errno_set"
  errnoSet :: Int -> IO Int

-- | The current value of @errno@.
getErrno :: IO Errno
getErrno = errnoGet >>= \value -> pure (Errno (fromIntegral value))

-- | Set @errno@ back to zero.
resetErrno :: IO ()
resetErrno = errnoSet 0 >> pure ()

-- | Throw the IO error that the current @errno@ denotes.
throwErrno :: String -> IO a
throwErrno location =
  getErrno >>= \errno -> ioError (errnoToIOError location errno Nothing Nothing)

-- | Run an action and throw when its result denotes an error.
throwErrnoIf :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf failed location action =
  action >>= \result ->
    if failed result then throwErrno location else pure result

-- | 'throwErrnoIf' discarding the result.
throwErrnoIf_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ failed location action =
  throwErrnoIf failed location action >> pure ()

-- | 'throwErrnoIf' that runs the action again when it was interrupted.
throwErrnoIfRetry :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry failed location action = retry
  where
    retry =
      action >>= \result ->
        if failed result
          then
            getErrno >>= \errno ->
              if errno == eINTR then retry else throwErrno location
          else pure result

-- | 'throwErrnoIfRetry' discarding the result.
throwErrnoIfRetry_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ failed location action =
  throwErrnoIfRetry failed location action >> pure ()

-- | Throw when the action returns @-1@, the usual C failure result.
throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1 = throwErrnoIf (\result -> result == negate 1)

-- | 'throwErrnoIfMinus1' discarding the result.
throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_ = throwErrnoIf_ (\result -> result == negate 1)

-- | 'throwErrnoIfMinus1' that runs the action again when it was interrupted.
throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry = throwErrnoIfRetry (\result -> result == negate 1)

-- | 'throwErrnoIfMinus1Retry' discarding the result.
throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_ = throwErrnoIfRetry_ (\result -> result == negate 1)

-- | Throw when the action returns a null pointer.
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull = throwErrnoIf (== nullPtr)

-- | 'throwErrnoIfNull' that runs the action again when it was interrupted.
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry = throwErrnoIfRetry (== nullPtr)

-- | 'throwErrno' naming the file the operation was working on.
throwErrnoPath :: String -> FilePath -> IO a
throwErrnoPath location path =
  getErrno >>= \errno -> ioError (errnoToIOError location errno Nothing (Just path))

-- | 'throwErrnoIf' naming the file the operation was working on.
throwErrnoPathIf :: (a -> Bool) -> String -> FilePath -> IO a -> IO a
throwErrnoPathIf failed location path action =
  action >>= \result ->
    if failed result then throwErrnoPath location path else pure result

-- | 'throwErrnoPathIf' discarding the result.
throwErrnoPathIf_ :: (a -> Bool) -> String -> FilePath -> IO a -> IO ()
throwErrnoPathIf_ failed location path action =
  throwErrnoPathIf failed location path action >> pure ()

-- | 'throwErrnoPathIf' for an action that returns a null pointer.
throwErrnoPathIfNull :: String -> FilePath -> IO (Ptr a) -> IO (Ptr a)
throwErrnoPathIfNull = throwErrnoPathIf (== nullPtr)

-- | 'throwErrnoPathIf' for an action that returns @-1@.
throwErrnoPathIfMinus1 :: (Eq a, Num a) => String -> FilePath -> IO a -> IO a
throwErrnoPathIfMinus1 = throwErrnoPathIf (\result -> result == negate 1)

-- | 'throwErrnoPathIfMinus1' discarding the result.
throwErrnoPathIfMinus1_ :: (Eq a, Num a) => String -> FilePath -> IO a -> IO ()
throwErrnoPathIfMinus1_ = throwErrnoPathIf_ (\result -> result == negate 1)
