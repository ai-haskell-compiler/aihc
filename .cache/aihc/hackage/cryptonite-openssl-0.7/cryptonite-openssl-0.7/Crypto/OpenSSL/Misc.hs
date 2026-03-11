{-# LANGUAGE DeriveDataTypeable #-}
module Crypto.OpenSSL.Misc
    ( OpenSSLError(..)
    , OpenSSLGcmError(..)
    , check
    , checkCtx
    , doIO
    ) where

import           Control.Exception
import           Data.Typeable
import           Foreign.C.Types (CInt)
import           System.IO.Unsafe (unsafePerformIO)
import           Basement.Compat.CallStack

data OpenSSLError = OpenSSLError Int
    deriving (Show,Read,Eq,Typeable)

instance Exception OpenSSLError

newtype OpenSSLGcmError = OpenSSLGcmError String
    deriving (Show,Read,Eq,Typeable)

instance Exception OpenSSLGcmError

check :: HasCallStack => IO CInt -> IO ()
check f = do
    r <- f
    if r == 0
        then throwIO $ OpenSSLError (fromIntegral r)
        else return ()
{-# INLINE check #-}

checkCtx :: Exception e => (String -> e) -> String -> IO CInt -> IO ()
checkCtx exnConstr n f = do
    r <- f
    if (r /= 1) then throwIO $ exnConstr n else return ()

doIO :: IO a -> a
doIO = unsafePerformIO
