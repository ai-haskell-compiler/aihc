module System.IO.Error
  ( IOError,
    IOException,
    IOErrorType (..),
    ioError,
    userError,
  )
where

import GHC.IO.Exception (IOError, IOErrorType (..), IOException, ioError, userError)
