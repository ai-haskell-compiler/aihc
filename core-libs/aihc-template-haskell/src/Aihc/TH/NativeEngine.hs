-- | The main loop of a native splice program.
--
-- With @--unsafe-disable-th-sandbox@, the compiler runs the splices of a
-- module in a native program, not in a WebAssembly instance. The program
-- has no sandbox. The compiler makes the main module of the program. That
-- module gives the closures of the splices to 'serveSplices'.
--
-- The program and the compiler use the same lines as the WebAssembly
-- engine. The C functions of @cbits/aihc_native_engine.c@ read and write
-- these lines.
module Aihc.TH.NativeEngine
  ( serveSplices,
  )
where

import Control.Exception (ErrorCall (..), SomeException, toException, try)
import Foreign.C.String (withCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)
import System.IO (hFlush, stdout)

-- | Move the protocol away from the standard streams.
foreign import ccall unsafe "aihc_native_engine_start" engineStart :: IO ()

-- | Reply to the command of the last splice, and give the index of the
-- next splice. The result is negative at the end of the input.
foreign import ccall unsafe "aihc_native_engine_next" engineNext :: IO Int

-- | Tell the compiler that the splice raised an exception.
foreign import ccall unsafe "aihc_native_engine_fail" engineFail :: Ptr CChar -> Int -> IO ()

-- | Run the splices that the compiler asks for, by their index in the
-- list, until the end of the input.
serveSplices :: [IO ()] -> IO ()
serveSplices closures = engineStart >> loop
  where
    loop = do
      index <- engineNext
      if index < 0
        then pure ()
        else do
          result <- case drop index closures of
            closure : _ -> try closure
            [] -> pure (Left (toException (ErrorCall ("unknown closure " <> show index))))
          -- The standard output of a splice goes to the standard error of
          -- the compiler. Write it before the reply.
          hFlush stdout
          case result of
            Left exception -> withCStringLen (show (exception :: SomeException)) (uncurry engineFail)
            Right () -> pure ()
          loop
