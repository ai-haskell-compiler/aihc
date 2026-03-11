{-# LANGUAGE DataKinds #-}

module HelloWorld (helloWorld) where

import Prelude as P

import Control.Monad.Catch
import Control.Monad.State
import Data.Word

import AsmAArch64

msg :: String
msg = "Hello World!\n"

-- | syscalls
sysExit, sysWrite :: Word16
sysWrite = 64
sysExit = 93

helloWorld :: MonadCatch m => StateT CodeState m ()
helloWorld = do

    start <- label
    exportSymbol "_start" start
    mov x0 1
    helloString <- ascii msg
    adr x1 helloString
    mov x2 $ fromIntegral $ P.length msg
    mov x8 sysWrite
    svc 0

    mov x0 0
    mov x8 sysExit
    svc 0
