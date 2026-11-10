{-# LANGUAGE MagicHash #-}

-- | The program of an image that runs closures for an engine. See
-- "Aihc.Wasm.Engine".
--
-- The program asks the driver for the address of a closure, runs the
-- closure, and asks again. The driver gives the null address when it has
-- no more closures. A closure is a static value of the type @IO ()@ in a
-- shared module that the driver loaded.
--
-- The driver does not call into the program. The program calls the driver,
-- and the driver gives its answer as the result of that call.
module Main (main) where

import Control.Exception (SomeException, try)
import Foreign.C.String (withCStringLen)
import Foreign.C.Types (CChar)
import Foreign.Ptr (nullPtr)
import GHC.Exts (unsafeCoerce#)
import GHC.Ptr (Ptr (..))

-- | The address of the next closure. The call returns when the driver has
-- a closure or has no more commands.
foreign import ccall unsafe "aihc_engine_next" engineNext :: IO (Ptr ())

-- | Tell the driver that the closure raised an exception.
foreign import ccall unsafe "aihc_engine_fail" engineFail :: Ptr CChar -> Int -> IO ()

main :: IO ()
main = do
  closure <- engineNext
  if closure == nullPtr
    then pure ()
    else do
      result <- try (closureAction closure)
      case result of
        Left exception -> withCStringLen (show (exception :: SomeException)) (uncurry engineFail)
        Right () -> pure ()
      main

-- | The closure at an address. A static closure does not move, so its
-- address is the value itself.
closureAction :: Ptr () -> IO ()
closureAction (Ptr address) = unsafeCoerce# address
