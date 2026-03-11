-- | Implementation.
--
-- Module      : Network.Socket.Splice.Internal
-- Copyright   : (c) Cetin Sert 2012
-- License     : BSD3
-- Maintainer  : fusion@corsis.eu
-- Stability   : stable
-- Portability : works on all operating systems


#ifdef LINUX_SPLICE
#include <fcntl.h>
#endif
{-# LANGUAGE CPP, ForeignFunctionInterface #-}


module Network.Socket.Splice.Internal (

  -- * Cross-platform API for Socket to Socket Data Transfer Loops

  {- | 'splice' is the cross-platform API for continous, uni-directional
       data transfer between two network sockets.

       'splice' and its implementation primitives 'hSplice' and 'fdSplice' are
       /infinite/ loops that are intended to be used with
       'Control.Concurrent.forkIO' and exception handlers. 'splice' is a
       terminal operation; it cannot be interleaved by other IO operations on
       its sockets or handles.

       [Initiate bi-directional continuous data transfer between two sockets:]
    
       > void . forkIO . tryWith handler $! splice void 1024 (sourceSocket, _) (targetSocket, _)
       > void . forkIO . tryWith handler $! splice void 1024 (targetSocket, _) (sourceSocket, _)

       where @handler@ is an IO operation that would do the necessary clean up –
       such as ensuring the sockets are closed and any resources that may be
       associated with the sockets are properly disposed of.

       [Notes]

         * 'System.IO.Splice.Linux.c_splice', the Linux-only system call, is not
           a terminal infinite loop and can be safely interleaved by other IO
           operations on sockets or socket handles.
  -}

    splice
  , ChunkSize
  , zeroCopy

  -- * Combinators for Exception Handling
  , tryWith
  , try_

  -- * Implementation Primitives
  {- | Infinite loops used in the cross-platform implementation of 'splice'. -}

  , hSplice
#ifdef LINUX_SPLICE
  , fdSplice
#endif

  ) where


import Data.Word
import Foreign.Ptr

import System.IO
import Network.Socket
import Control.Monad
import Control.Exception
import Foreign.Marshal.Alloc

#ifdef LINUX_SPLICE
import Data.Int
import Data.Bits
import System.Posix.IO
import Unsafe.Coerce
import Foreign.C.Error
import System.Posix.Types
import System.Posix.Internals
import qualified System.IO.Splice.Linux as L
#else
import Data.Maybe
#endif


----------------------------------------------------------------------------------------------------


-- | Indicates whether 'splice' uses zero-copy system calls or the portable user
--   space Haskell implementation.
zeroCopy :: Bool -- ^ @True@ if 'splice' uses zero-copy system calls; otherwise, false.
zeroCopy =
#ifdef LINUX_SPLICE
  True
#else
  False
#endif


-- | The numeric type to recommend chunk sizes for moving data between sockets
--   used by both zero-copy and portable implementations of 'splice'.
type ChunkSize =
#ifdef LINUX_SPLICE
  L.ChunkSize
#else
  Int
#endif


----------------------------------------------------------------------------------------------SPLICE


-- | Pipes data from one socket to another in an /infinite loop/.
--
--   'splice' currently has two implementations:
--
--   [on GNU\/Linux using 'fdSplice' ≅]
--
--   > splice len (sIn, _       ) (sOut, _        ) = ... fdSplice ...
--
--   [on all other operating systems using 'hSplice' ≅]
--
--   > splice len (_  , Just hIn) (_   , Just hOut) = ... hSplice  ...
--
--   [Notes]
--
--     * 'fdSplice' and 'fdSplice' implementation of 'splice' are only available
--        on GNU\/Linux.
--
--     * 'hSplice' is always available and the 'hSplice' implementation of
--       'splice' can be forced on GNU\/Linux by defining the @portable@ flag at
--       compile time.
--
--     * 'hSplice' implementation requires handles in 'NoBuffering' mode.
--
--     * 'splice' is a terminal loop on two sockets and once entered its sockets
--        and handles cannot be interleaved by other IO operations.
--
splice
  :: ChunkSize               -- ^ maximal chunk size.
  -> (Socket, Maybe Handle)  -- ^ source socket and possibly its opened handle.
  -> (Socket, Maybe Handle)  -- ^ target socket and possibly its opened handle.
  -> IO ()                   -- ^ infinite loop.
#ifdef LINUX_SPLICE
splice len (sIn, _  ) (sOut, _   ) = do
#else
splice len (_  , hIn) (_   , hOut) = do
#endif
#ifdef LINUX_SPLICE
  let s = Fd $! fdSocket sIn
  let t = Fd $! fdSocket sOut
  fdSplice len s t
#else
  let s = fromJust hIn
  let t = fromJust hOut
  hSplice (fromIntegral len) s t
#endif


--------------------------------------------------------------------------------------------FDSPLICE


#ifdef LINUX_SPLICE

{- | GNU\/Linux @splice()@ system call loop.

       1. creates a pipe in kernel address space

       2. uses it until the loop terminates by exception

       3. closes the pipe and returns
-}
fdSplice :: ChunkSize -> Fd -> Fd -> IO ()
fdSplice len s@(Fd fdIn) t@(Fd fdOut) = do

  (r,w) <- createPipe
  let n = nullPtr  
  let u = unsafeCoerce :: (#type ssize_t) -> (#type size_t)
  let check = throwErrnoIfMinus1 "Network.Socket.Splice.splice"
  let flags = L.sPLICE_F_MOVE .|. L.sPLICE_F_MORE
  let setNonBlockingMode v = do setNonBlockingFD fdIn  v
                                setNonBlockingFD fdOut v
  setNonBlockingMode False

  finally
    (forever $! do 
       bytes   <- check $! L.c_splice s n w n    len    flags
       if bytes > 0
         then              L.c_splice r n t n (u bytes) flags
         else              throwRecv0)
    (do closeFd r
        closeFd w
        try_ $! setNonBlockingMode True)

#endif


---------------------------------------------------------------------------------------------HSPLICE


{- | The portable Haskell loop.

       1. allocates a /single/ memory buffer in user address space

       2. uses it until the loop terminates by exception

       3. frees the buffer and returns

   [Notes]

     * the socket handles are required to be in 'NoBuffering' mode.
-} 
hSplice :: Int -> Handle -> Handle -> IO ()
hSplice len s t = do

  a <- mallocBytes len :: IO (Ptr Word8)

  finally
    (forever $! do
       bytes   <- hGetBufSome s a len
       if bytes > 0
         then     hPutBuf     t a bytes
         else     throwRecv0)
    (free a)


------------------------------------------------------------------------------------------EXCEPTIONS


-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
--   is expected and can be handled easily. Unlike 'finally' exceptions are
--   /NOT/ rethrown once handled.
tryWith
  :: (SomeException -> IO a) -- ^ exception handler.
  -> IO a                    -- ^ action to run which can throw /any/ exception.
  -> IO a                    -- ^ new action where all exceptions are handled by the single handler.
tryWith h a = try a >>= \r -> case r of Left x -> h x; Right y -> return y


-- | Similar to 'Control.Exception.Base.try' but used when an obvious exception
--   is expected which can be safely ignored.
try_
  :: IO ()                   -- ^ action to run which can throw /any/ exception.
  -> IO ()                   -- ^ new action where exceptions are silenced.
try_ a = (try a :: IO (Either SomeException ())) >> return ()


throwRecv0 :: a
throwRecv0 = error "Network.Socket.Splice.splice ended"