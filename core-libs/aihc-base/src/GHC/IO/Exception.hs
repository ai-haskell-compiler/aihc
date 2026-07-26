{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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

import GHC.IO (IO (..))
import GHC.Int (Int (..))
import Prelude hiding (Int)

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

foreign import ccall unsafe "aihc_io_raise_error"
  raiseIOErrorRaw :: Int# -> Int#

raiseIOError :: Int -> IO Int
raiseIOError (I# exceptionCode) =
  IO
    ( \state ->
        case raiseIOErrorRaw exceptionCode of
          result -> (# state, I# result #)
    )

-- | Raise an uncaught IO error. Catching typed exceptions is deliberately
-- outside the initial IO milestone; native execution terminates after the
-- runtime reports the encoded error number.
ioError :: IOException -> IO a
ioError (IOError exceptionCode) = do
  raiseIOError exceptionCode
  ioError (IOError exceptionCode)

ioErrorFromErrno :: String -> Maybe String -> Int -> IOException
ioErrorFromErrno _ _ = IOError

illegalOperationError :: String -> String -> IOException
illegalOperationError _ _ = IOError 9

userError :: String -> IOException
userError _ = IOError 0
