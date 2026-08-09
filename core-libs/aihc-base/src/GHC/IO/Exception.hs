-- | The minimal synchronous IO error model used by the first 'Handle' layer.
module GHC.IO.Exception
  ( IOError,
    IOException (..),
    IOErrorType (..),
    ioError,
    ioErrorFromErrno,
    illegalOperationError,
    userError,
  )
where

import GHC.Base (Maybe, Monad (..), String)
import GHC.IO (IO)
import GHC.IO.Runtime (raiseIOErrorRaw)
import GHC.Int (Int)

data IOErrorType
  = AlreadyExists
  | NoSuchThing
  | ResourceBusy
  | ResourceExhausted
  | EOF
  | IllegalOperation
  | PermissionDenied
  | InvalidArgument
  | UserError
  | OtherError

newtype IOException = IOError Int

type IOError = IOException

-- | Raise an uncaught IO error. Catching typed exceptions is deliberately
-- outside the initial IO milestone; native execution terminates after the
-- runtime reports the encoded error number.
ioError :: IOException -> IO a
ioError (IOError exceptionCode) = do
  raiseIOErrorRaw exceptionCode
  ioError (IOError exceptionCode)

ioErrorFromErrno :: String -> Maybe String -> Int -> IOException
ioErrorFromErrno _ _ = IOError

illegalOperationError :: String -> String -> IOException
illegalOperationError _ _ = IOError 9

userError :: String -> IOException
userError _ = IOError 0
