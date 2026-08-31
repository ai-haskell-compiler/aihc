module System.IO.Error
  ( IOError,
    IOException,
    IOErrorType (..),
    ioError,
    ioeSetErrorString,
    mkIOError,
    userError,
  )
where

import GHC.IO.Exception (IOError, IOErrorType (..), IOException, ioError, ioeSetErrorString, mkIOError, userError)
