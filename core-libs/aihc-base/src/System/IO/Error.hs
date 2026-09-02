module System.IO.Error
  ( IOError,
    IOException,
    IOErrorType (..),
    ioError,
    ioeSetErrorString,
    mkIOError,
    userError,
    alreadyExistsErrorType,
    doesNotExistErrorType,
    alreadyInUseErrorType,
    fullErrorType,
    eofErrorType,
    illegalOperationErrorType,
    permissionErrorType,
    userErrorType,
  )
where

import GHC.IO.Exception (IOError, IOErrorType (..), IOException, ioError, ioeSetErrorString, mkIOError, userError)

alreadyExistsErrorType :: IOErrorType
alreadyExistsErrorType = AlreadyExists

doesNotExistErrorType :: IOErrorType
doesNotExistErrorType = NoSuchThing

alreadyInUseErrorType :: IOErrorType
alreadyInUseErrorType = ResourceBusy

fullErrorType :: IOErrorType
fullErrorType = ResourceExhausted

eofErrorType :: IOErrorType
eofErrorType = EOF

illegalOperationErrorType :: IOErrorType
illegalOperationErrorType = IllegalOperation

permissionErrorType :: IOErrorType
permissionErrorType = PermissionDenied

userErrorType :: IOErrorType
userErrorType = UserError
