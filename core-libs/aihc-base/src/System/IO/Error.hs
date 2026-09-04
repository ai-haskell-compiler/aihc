-- | IO errors and their construction and inspection.
module System.IO.Error
  ( IOError,
    userError,
    mkIOError,
    annotateIOError,
    isAlreadyExistsError,
    isDoesNotExistError,
    isAlreadyInUseError,
    isFullError,
    isEOFError,
    isIllegalOperation,
    isPermissionError,
    isUserError,
    isResourceVanishedError,
    ioeGetErrorType,
    ioeGetLocation,
    ioeGetErrorString,
    ioeGetHandle,
    ioeGetFileName,
    ioeSetErrorType,
    ioeSetErrorString,
    ioeSetLocation,
    ioeSetHandle,
    ioeSetFileName,
    IOErrorType,
    alreadyExistsErrorType,
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType,
    permissionErrorType,
    userErrorType,
    resourceVanishedErrorType,
    isAlreadyExistsErrorType,
    isDoesNotExistErrorType,
    isAlreadyInUseErrorType,
    isFullErrorType,
    isEOFErrorType,
    isIllegalOperationErrorType,
    isPermissionErrorType,
    isUserErrorType,
    isResourceVanishedErrorType,
    ioError,
    catchIOError,
    tryIOError,
    modifyIOError,
  )
where

import Control.Exception.Base (catch, throwIO, try)
import GHC.IO.Exception (IOError, IOErrorType (..), IOException (..), ioError, userError)
import GHC.IO.Handle.Types (Handle)
import Prelude (Bool (..), Either, FilePath, IO, Maybe (..), String, (.), (==), (>>=))

tryIOError :: IO a -> IO (Either IOError a)
tryIOError = try

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> IOError
mkIOError errorType location handle path =
  IOError
    { ioe_type = errorType,
      ioe_location = location,
      ioe_description = "",
      ioe_errno = Nothing,
      ioe_handle = handle,
      ioe_filename = path
    }

isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError, isFullError, isEOFError, isIllegalOperation, isPermissionError, isUserError, isResourceVanishedError :: IOError -> Bool
isAlreadyExistsError = isAlreadyExistsErrorType . ioeGetErrorType
isDoesNotExistError = isDoesNotExistErrorType . ioeGetErrorType
isAlreadyInUseError = isAlreadyInUseErrorType . ioeGetErrorType
isFullError = isFullErrorType . ioeGetErrorType
isEOFError = isEOFErrorType . ioeGetErrorType
isIllegalOperation = isIllegalOperationErrorType . ioeGetErrorType
isPermissionError = isPermissionErrorType . ioeGetErrorType
isUserError = isUserErrorType . ioeGetErrorType
isResourceVanishedError = isResourceVanishedErrorType . ioeGetErrorType

alreadyExistsErrorType, doesNotExistErrorType, alreadyInUseErrorType, fullErrorType, eofErrorType, illegalOperationErrorType, permissionErrorType, userErrorType, resourceVanishedErrorType :: IOErrorType
alreadyExistsErrorType = AlreadyExists
doesNotExistErrorType = NoSuchThing
alreadyInUseErrorType = ResourceBusy
fullErrorType = ResourceExhausted
eofErrorType = EOF
illegalOperationErrorType = IllegalOperation
permissionErrorType = PermissionDenied
userErrorType = UserError
resourceVanishedErrorType = ResourceVanished

isAlreadyExistsErrorType, isDoesNotExistErrorType, isAlreadyInUseErrorType, isFullErrorType, isEOFErrorType, isIllegalOperationErrorType, isPermissionErrorType, isUserErrorType, isResourceVanishedErrorType :: IOErrorType -> Bool
isAlreadyExistsErrorType = (== AlreadyExists)
isDoesNotExistErrorType = (== NoSuchThing)
isAlreadyInUseErrorType = (== ResourceBusy)
isFullErrorType = (== ResourceExhausted)
isEOFErrorType = (== EOF)
isIllegalOperationErrorType = (== IllegalOperation)
isPermissionErrorType = (== PermissionDenied)
isUserErrorType = (== UserError)
isResourceVanishedErrorType = (== ResourceVanished)

ioeGetErrorType :: IOError -> IOErrorType
ioeGetErrorType = ioe_type

ioeGetErrorString :: IOError -> String
ioeGetErrorString exception =
  case isUserErrorType (ioe_type exception) of
    True -> ioe_description exception
    False -> ioe_description exception

ioeGetLocation :: IOError -> String
ioeGetLocation = ioe_location

ioeGetHandle :: IOError -> Maybe Handle
ioeGetHandle = ioe_handle

ioeGetFileName :: IOError -> Maybe FilePath
ioeGetFileName = ioe_filename

ioeSetErrorType :: IOError -> IOErrorType -> IOError
ioeSetErrorType exception errorType = exception {ioe_type = errorType}

ioeSetErrorString :: IOError -> String -> IOError
ioeSetErrorString exception description = exception {ioe_description = description}

ioeSetLocation :: IOError -> String -> IOError
ioeSetLocation exception location = exception {ioe_location = location}

ioeSetHandle :: IOError -> Handle -> IOError
ioeSetHandle exception handle = exception {ioe_handle = Just handle}

ioeSetFileName :: IOError -> FilePath -> IOError
ioeSetFileName exception path = exception {ioe_filename = Just path}

modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError function action = catch action (ioError . function)

annotateIOError :: IOError -> String -> Maybe Handle -> Maybe FilePath -> IOError
annotateIOError exception location handle path =
  exception
    { ioe_handle = orElse handle (ioe_handle exception),
      ioe_location = location,
      ioe_filename = orElse path (ioe_filename exception)
    }

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just value) _ = Just value
orElse Nothing fallback = fallback

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch
