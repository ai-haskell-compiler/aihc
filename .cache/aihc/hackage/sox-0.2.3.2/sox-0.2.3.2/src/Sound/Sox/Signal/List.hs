{- |
Functions for reading and writing data
using the endianess of the machine.
This is the way Sox handles raw data.
This module is more or less provided for completeness,
since it is based on lists,
which means that it is too slow
to process real world data.
For serious applications use Data.StorableVector.Lazy.
-}
module Sound.Sox.Signal.List (
   writeFile, put,
   withReadFile, getContents,
   ReadException, IOReadException,
   ) where

import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Exception.Asynchronous as Async

import Control.Monad.Trans.Class (lift, )

import Foreign.Storable (Storable (..), )

import Foreign (Ptr, alloca, )
import System.IO (withBinaryFile, IOMode(WriteMode,ReadMode), Handle, hPutBuf, hGetBuf, )
import Control.Exception.Extensible (SomeException, try, )
import Control.Monad (liftM)

import System.IO.Unsafe (unsafeInterleaveIO, )

import Prelude hiding (writeFile, readFile, getContents, )


writeFile :: Storable a => FilePath -> [a] -> IO ()
writeFile fileName signal =
   withBinaryFile fileName WriteMode (flip put signal)

put :: Storable a => Handle -> [a] -> IO ()
put h signal =
   alloca $
      \p -> mapM_ (putFrame h p) signal

putFrame :: Storable a => Handle -> Ptr a -> a -> IO ()
putFrame h p n =
   poke p n >> hPutBuf h p (sizeOf n)



data ReadException =
   BrokenFrame
  deriving (Show, Eq, Enum)

type IOReadException =
   Either ReadException SomeException

withReadFile :: Storable a =>
   FilePath ->
   (Async.Exceptional IOReadException [a] -> IO b) ->
   IO b
withReadFile fileName act =
   withBinaryFile fileName ReadMode $ \sig ->
      getContents sig >>= act

getContents :: Storable a =>
   Handle -> IO (Async.Exceptional IOReadException [a])
getContents h =
   alloca $
      \p ->
--         Async.eatNothingT $
         liftM (\(Async.Exceptional (Just e) a) -> Async.Exceptional e a) $
         Async.manySynchronousT
            unsafeInterleaveIO
            (:) [] (getFrame h p)

getFrame :: Storable a =>
   Handle -> Ptr a ->
   Sync.ExceptionalT (Maybe IOReadException) IO a
getFrame h p =
   do let getSize :: Storable a => a -> Ptr a -> Int
          getSize dummy _ = sizeOf dummy
          size = getSize undefined p
      cnt <-
         Sync.mapExceptionT (Just . Right) $ Sync.fromEitherT $
         try $ hGetBuf h p size
      Sync.assertT Nothing (cnt > 0)
      Sync.assertT (Just $ Left BrokenFrame) (cnt == size)
      lift $ peek p
