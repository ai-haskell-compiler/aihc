{-# LANGUAGE DeriveGeneric #-}

module Main where

-- import Foreign.CStorable -- c-storable-deriving
-- import Foreign.Storable
-- import System.IO.Posix.MMap.Internal (c_mmap, c_munmap) -- bytestring-mmap
import           Data.ByteString.Internal (fromForeignPtr)
import           Foreign.ForeignPtr
import           Foreign.Storable
import           GHC.Generics
import           System.Posix.Files
import           System.Posix.SharedMem

import           SharedMemory


data Person = Person
  { name :: String
  , age :: Int
  } deriving (Eq, Ord, Show, Generic)

-- instance CStorable Person


main :: IO ()
main = do
  let shmemPath = "/asdf"
  let size = 200

  (fptr, fd) <- openSharedMemory shmemPath
                                 size
                                 ShmOpenFlags{ shmReadWrite = True
                                             , shmCreate = True
                                             , shmExclusive = False
                                             , shmTrunc = True
                                             }
                                 ownerModes

  withForeignPtr fptr $ \ptr8 -> do
    x <- peekByteOff ptr8 0
    print (x :: Char)

  withForeignPtr fptr $ \ptr8 -> do
    pokeByteOff ptr8 0 'h'
    y <- peekByteOff ptr8 0
    print (y :: Char)

  let bs = fromForeignPtr (castForeignPtr fptr) 0 (fromIntegral size)

  print fd
  print fptr
  print bs
