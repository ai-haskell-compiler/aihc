{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.IO (IO (..))
import GHC.Prim (catch#, raise#)
import GHC.Ptr (Ptr (..))
import System.IO (hPutBuf, stdout)

data DemoException = DemoException

failingThunk :: DemoException
failingThunk = raise# DemoException

forceFailure :: IO ()
forceFailure =
  IO
    ( \state ->
        case failingThunk of
          DemoException -> (# state, () #)
    )

throwIO :: DemoException -> IO a
throwIO exception = IO (\_state -> raise# exception)

catchIO :: IO a -> (DemoException -> IO a) -> IO a
catchIO (IO action) handler =
  IO
    ( \state ->
        catch#
          action
          ( \exception ->
              case handler exception of
                IO handlerAction -> handlerAction
          )
          state
    )

main :: IO ()
main =
  catchIO
    (catchIO forceFailure throwIO)
    ( \exception ->
        case exception of
          DemoException -> hPutBuf stdout (Ptr "outer handler caught rethrow\n"# :: Ptr ()) 29
    )
