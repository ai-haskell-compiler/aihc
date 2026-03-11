{- |
Files with text content.
-}
module System.IO.Exception.TextFile where

import System.IO.Exception.File (EIO, close, )
import qualified Control.Monad.Exception.Synchronous  as Sync
import qualified Control.Monad.Exception.Asynchronous as Async
import Control.Monad.Exception.Synchronous (bracketT, )
import System.IO.Straight (SIO, ioToExceptionalSIO, unsafeInterleaveSIO, )
import System.IO (Handle, IOMode, )
import qualified System.IO as IO

import System.IO.Error (isEOFError, )
import Control.Exception (IOException)

import Prelude hiding (getChar)


open :: FilePath -> IOMode -> EIO Handle
open name mode =
   ioToExceptionalSIO $ IO.openFile name mode

with ::
   FilePath -> IOMode -> (Handle -> EIO r) -> EIO r
with name mode =
   bracketT (open name mode) close

getChar :: Handle -> EIO Char
getChar h =
   ioToExceptionalSIO $ IO.hGetChar h

getContentsSynchronous :: Handle -> EIO String
getContentsSynchronous h =
   Sync.manyT
      -- candidate for toMaybe from utility-ht
      (\e -> if isEOFError e then Nothing else Just e)
      (:) [] (getChar h)

{- |
This calls 'unsafeInterleaveIO'.
Maybe we should also attach 'unsafe' to this function name?
We should use the LazyIO type and manyT here.
-}
getContentsAsynchronous :: Handle -> SIO (Async.Exceptional IOException String)
getContentsAsynchronous h =
   Async.manySynchronousT unsafeInterleaveSIO (:) [] (getChar h)

putChar :: Handle -> Char -> EIO ()
putChar h c =
   ioToExceptionalSIO $ IO.hPutChar h c
