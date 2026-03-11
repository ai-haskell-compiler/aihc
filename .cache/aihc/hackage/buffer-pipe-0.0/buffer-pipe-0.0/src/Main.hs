module Main where

import qualified Foreign.Marshal.Alloc as Alloc
import qualified System.IO as IO
import Control.Monad (when)


chunkSize :: Int
chunkSize = 100*1024*1024

main :: IO ()
main = Alloc.allocaBytes chunkSize $ \ptr ->
   let go = do
          readSize <- IO.hGetBuf IO.stdin ptr chunkSize
          IO.hPutBuf IO.stdout ptr readSize
          when (readSize >= chunkSize) go
   in  go
