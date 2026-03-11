{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- |
-- Module      :  System.IO.MMap
-- Copyright   :  (c) Gracjan Polak 2009
-- License     :  BSD-style
--
-- Stability   :  experimental
-- Portability :  portable
--
-- This library provides a wrapper to mmap(2) or MapViewOfFile,
-- allowing files or devices to be lazily loaded into memory as strict
-- or lazy ByteStrings, ForeignPtrs or plain Ptrs, using the virtual
-- memory subsystem to do on-demand loading.  Modifications are also
-- supported.


module System.IO.MMap
(
     -- $mmap_intro

     -- * Mapping mode
     Mode(..),

     -- * Memory mapped files strict interface
     mmapFilePtr,
     mmapWithFilePtr,
     mmapFileForeignPtr,
     mmapFileByteString,

     munmapFilePtr,

     -- * Memory mapped files lazy interface
     mmapFileForeignPtrLazy,
     mmapFileByteStringLazy
)
where

import System.IO ()
import Foreign.Ptr (Ptr,FunPtr,nullPtr,plusPtr,castPtr)
import Foreign.C.Types (CInt(..),CLLong(..),CSize(..))
import Foreign.C.String (CString,withCString)
import Foreign.ForeignPtr (ForeignPtr,withForeignPtr,finalizeForeignPtr,newForeignPtr,newForeignPtrEnv)
import Foreign.C.Error
import System.IO.Unsafe  (unsafePerformIO)
import qualified Data.ByteString.Internal as BS (fromForeignPtr)
import Data.Int (Int64)
import Control.Monad  (when)
import qualified Control.Exception as E (bracketOnError, bracket, finally)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as BSL  (ByteString,fromChunks)
import Prelude hiding (length)

--import Debug.Trace

-- TODO:
--    - support native characters (Unicode) in FilePath
--    - support externally given HANDLEs and FDs
--    - support data commit
--    - support memory region resize

-- $mmap_intro
--
-- This module is an interface to @mmap(2)@ system call under POSIX
-- (Unix, Linux, Mac OS X) and @CreateFileMapping@, @MapViewOfFile@ under
-- Windows.
--
-- We can consider mmap as lazy IO pushed into the virtual memory
-- subsystem.
--
-- It is only safe to mmap a file if you know you are the sole
-- user. Otherwise referential transparency may be or may be not
-- compromised. Sadly semantics differ much between operating systems.
--
-- In case of IO errors all function use 'throwErrno' or 'throwErrnoPath'.
--
-- In case of 'ForeignPtr' or 'BS.ByteString' functions the storage
-- manager is used to free the mapped memory. When the garbage
-- collector notices there are no further references to the mapped
-- memory, a call to @munmap@ is made. It is not necessary to do this
-- yourself. In tight memory situations it may be profitable to use
-- 'System.Mem.performGC' or 'finalizeForeignPtr' to force an unmap
-- action. You can also use 'mmapWithFilePtr' that uses scope based
-- resource allocation.
--
-- To free resources returned as Ptr use 'munmapFilePtr'.
--
-- For modes 'ReadOnly', 'ReadWrite' and 'WriteCopy' file must exist
-- before mapping it into memory. It also needs to have correct
-- permissions for reading and/or writing (depending on mode). In
-- 'ReadWriteEx' the file will be created with default permissions if
-- it does not exist.
--
-- If mode is 'ReadWrite', 'ReadWriteEx' or 'WriteCopy' the returned
-- memory region may be written to with 'Foreign.Storable.poke' and
-- friends. In 'WriteCopy' mode changes will not be written to disk.
-- It is an error to modify mapped memory in 'ReadOnly' mode. If is
-- undefined if and how changes from external changes affect your
-- mmapped regions, they may reflect in your memory or may not and
-- this note applies equally to all modes.
--
-- Range specified may be 'Nothing', in this case whole file will be
-- mapped. Otherwise range should be 'Just (offset,size)' where
-- offsets is the beginning byte of file region to map and size tells
-- mapping length. There are no alignment requirements. Returned Ptr or
-- ForeignPtr will be aligned to page size boundary and you'll be
-- given offset to your data. Both @offset@ and @size@ must be
-- nonnegative.  Sum @offset + size@ should not be greater than file
-- length, except in 'ReadWriteEx' mode when file will be extended to
-- cover whole range. We do allow @size@ to be 0 and we do mmap files
-- of 0 length. If your offset is 0 you are guaranteed to receive page
-- aligned pointer back. You are required to give explicit range in
-- case of 'ReadWriteEx' even if the file exists.
--
-- File extension in 'ReadWriteEx' mode seems to use sparse files
-- whenever supported by oprating system and therefore returns
-- immediatelly as postpones real block allocation for later.
--
-- For more details about mmap and its consequences see:
--
-- * <http://opengroup.org/onlinepubs/009695399/functions/mmap.html>
--
-- * <http://www.gnu.org/software/libc/manual/html_node/Memory_002dmapped-I_002fO.html>
--
-- * <http://msdn2.microsoft.com/en-us/library/aa366781(VS.85).aspx>
--
-- Questions and Answers
--
-- * Q: What happens if somebody writes to my mmapped file? A:
-- Undefined. System is free to not synchronize write system call and
-- mmap so nothing is sure. So this might be reflected in your memory
-- or not.  This applies even in 'WriteCopy' mode.
--
-- * Q: What happens if I map 'ReadWrite' and change memory? A: After
-- some time in will be written to disk. It is unspecified when this
-- happens.
--
-- * Q: What if somebody removes my file? A: Undefined. File with
-- mmapped region is treated by system as open file. Removing such
-- file works the same way as removing open file and different systems
-- have different ideas what to do in such case.
--
-- * Q: Why can't I open my file for writting after mmaping it? A:
-- File needs to be unmapped first. Either make sure you don't
-- reference memory mapped regions and force garbage collection (this
-- is hard to do) or better yet use mmaping with explicit memory
-- management.
--
-- * Q: Can I map region after end of file? A: You need to use
-- 'ReadWriteEx' mode.
--


-- | Mode of mapping. Four cases are supported.
data Mode = ReadOnly     -- ^ file is mapped read-only, file must
                         -- exist
          | ReadWrite    -- ^ file is mapped read-write, file must
                         -- exist
          | WriteCopy    -- ^ file is mapped read-write, but changes
                         -- aren't propagated to disk, file must exist
          | ReadWriteEx  -- ^ file is mapped read-write, if file does
                         -- not exist it will be created with default
                         -- permissions, region parameter specifies
                         -- size, if file size is lower it will be
                         -- extended with zeros
    deriving (Eq,Ord,Enum,Show,Read)

sanitizeFileRegion :: (Integral a,Bounded a) => String -> ForeignPtr () -> Mode -> Maybe (Int64,a) -> IO (Int64,a)
sanitizeFileRegion filepath handle' ReadWriteEx (Just region@(offset,length)) =
    withForeignPtr handle' $ \handle -> do
        longsize <- c_system_io_file_size handle
        let needsize = fromIntegral (offset + fromIntegral length)
        when (longsize < needsize)
                 ((throwErrnoPathIfMinus1 "extend file size" filepath $
                   c_system_io_extend_file_size handle needsize) >> return ())
        return region
sanitizeFileRegion _filepath _handle ReadWriteEx _
    = error "sanitizeRegion given ReadWriteEx with no region, please check earlier for this"
sanitizeFileRegion filepath handle' mode region = withForeignPtr handle' $ \handle -> do
    longsize <- c_system_io_file_size handle >>= \x -> return (fromIntegral x)
    let Just (_,sizetype) = region
    (offset,size) <- case region of
        Just (offset,size) -> do
            when (size<0) $
                 ioError (errnoToIOError "mmap negative size reguested" eINVAL Nothing (Just filepath))
            when (offset<0) $
                 ioError (errnoToIOError "mmap negative offset reguested" eINVAL Nothing (Just filepath))
            when (mode/=ReadWriteEx && (longsize<offset || longsize<(offset + fromIntegral size))) $
                 ioError (errnoToIOError "mmap offset and size beyond end of file" eINVAL Nothing (Just filepath))
            return (offset,size)
        Nothing -> do
            when (longsize > fromIntegral (maxBound `asTypeOf` sizetype)) $
                 ioError (errnoToIOError "mmap requested size is greater then maxBound" eINVAL Nothing (Just filepath))
            return (0,fromIntegral longsize)
    return (offset,size)

checkModeRegion :: FilePath -> Mode -> Maybe a -> IO ()
checkModeRegion filepath ReadWriteEx Nothing =
    ioError (errnoToIOError "mmap ReadWriteEx must have explicit region" eINVAL Nothing (Just filepath))
checkModeRegion _ _ _ = return ()

-- | The 'mmapFilePtr' function maps a file or device into memory,
-- returning a tuple @(ptr,rawsize,offset,size)@ where:
--
-- * @ptr@ is pointer to mmapped region
--
-- * @rawsize@ is length (in bytes) of mapped data, rawsize might be
-- greater than size because of alignment
--
-- * @offset@ tell where your data lives: @plusPtr ptr offset@
--
-- * @size@ your data length (in bytes)
--
-- If 'mmapFilePtr' fails for some reason, a 'throwErrno' is used.
--
-- Use @munmapFilePtr ptr rawsize@ to unmap memory.
--
-- Memory mapped files will behave as if they were read lazily
-- pages from the file will be loaded into memory on demand.
--

mmapFilePtr :: FilePath                     -- ^ name of file to mmap
            -> Mode                         -- ^ access mode
            -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
            -> IO (Ptr a,Int,Int,Int)       -- ^ (ptr,rawsize,offset,size)
mmapFilePtr filepath mode offsetsize = do
    checkModeRegion filepath mode offsetsize
    E.bracket (mmapFileOpen filepath mode)
            (finalizeForeignPtr) mmap
    where
        mmap handle' = do
            (offset,size) <- sanitizeFileRegion filepath handle' mode offsetsize
            let align     = offset `mod` fromIntegral c_system_io_granularity
            let offsetraw = offset - align
            let sizeraw   = size + fromIntegral align
            ptr <- withForeignPtr handle' $ \handle ->
                   c_system_io_mmap_mmap handle (fromIntegral $ fromEnum mode)
                                             (fromIntegral offsetraw) (fromIntegral sizeraw)
            when (ptr == nullPtr) $
                  throwErrnoPath ("mmap of '" ++ filepath ++ "' failed") filepath
            return (castPtr ptr,sizeraw,fromIntegral align,size)

-- | Memory map region of file using autounmap semantics. See
-- 'mmapFilePtr' for description of parameters.  The @action@ will be
-- executed with tuple @(ptr,size)@ as single argument. This is the
-- pointer to mapped data already adjusted and size of requested
-- region. Return value is that of action.
mmapWithFilePtr :: FilePath                        -- ^ name of file to mmap
                -> Mode                            -- ^ access mode
                -> Maybe (Int64,Int)               -- ^ range to map, maps whole file if Nothing
                -> ((Ptr (),Int) -> IO a)          -- ^ action to run
                -> IO a                            -- ^ result of action
mmapWithFilePtr filepath mode offsetsize action = do
    checkModeRegion filepath mode offsetsize
    (ptr,rawsize,offset,size) <- mmapFilePtr filepath mode offsetsize
    result <- action (ptr `plusPtr` offset,size) `E.finally` munmapFilePtr ptr rawsize
    return result

-- | Maps region of file and returns it as 'ForeignPtr'. See 'mmapFilePtr' for details.
mmapFileForeignPtr :: FilePath                     -- ^ name of file to map
                   -> Mode                         -- ^ access mode
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO (ForeignPtr a,Int,Int)    -- ^ foreign pointer to beginning of raw region,
                                                   -- offset to your data and size of your data
mmapFileForeignPtr filepath mode range = do
    checkModeRegion filepath mode range
    (rawptr,rawsize,offset,size) <- mmapFilePtr filepath mode range
    let rawsizeptr = castIntToPtr rawsize
    foreignptr <- newForeignPtrEnv c_system_io_mmap_munmap_funptr rawsizeptr rawptr
    return (foreignptr,offset,size)

-- | Maps region of file and returns it as 'BS.ByteString'.  File is
-- mapped in in 'ReadOnly' mode. See 'mmapFilePtr' for details.
mmapFileByteString :: FilePath                     -- ^ name of file to map
                   -> Maybe (Int64,Int)            -- ^ range to map, maps whole file if Nothing
                   -> IO BS.ByteString             -- ^ bytestring with file contents
mmapFileByteString filepath range = do
    (foreignptr,offset,size) <- mmapFileForeignPtr filepath ReadOnly range
    let bytestring = BS.fromForeignPtr foreignptr offset size
    return bytestring

-- | The 'mmapFileForeignPtrLazy' function maps a file or device into
-- memory, returning a list of tuples with the same meaning as in
-- function 'mmapFileForeignPtr'.
--
-- Chunks are really mapped into memory at the first inspection of a
-- chunk. They are kept in memory while they are referenced, garbage
-- collector takes care of the later.
--
mmapFileForeignPtrLazy :: FilePath                    -- ^ name of file to mmap
                       -> Mode                        -- ^ access mode
                       -> Maybe (Int64,Int64)         -- ^ range to map, maps whole file if Nothing
                       -> IO [(ForeignPtr a,Int,Int)] -- ^ (ptr,offset,size)
mmapFileForeignPtrLazy filepath mode offsetsize = do
    checkModeRegion filepath mode offsetsize
    E.bracketOnError (mmapFileOpen filepath mode)
                       (finalizeForeignPtr) mmap
    where
        mmap handle = do
            (offset,size) <- sanitizeFileRegion filepath handle mode offsetsize
            return $ map (mmapFileForeignPtrLazyChunk filepath mode handle) (chunks offset size)


{-# NOINLINE mmapFileForeignPtrLazyChunk #-}
mmapFileForeignPtrLazyChunk :: FilePath
                            -> Mode
                            -> ForeignPtr ()
                            -> (Int64, Int)
                            -> (ForeignPtr a, Int, Int)
mmapFileForeignPtrLazyChunk filepath mode handle' (offset,size) = unsafePerformIO $
    withForeignPtr handle' $ \handle -> do
        let align     = offset `mod` fromIntegral c_system_io_granularity
            offsetraw = offset - align
            sizeraw   = size + fromIntegral align
        ptr <- c_system_io_mmap_mmap handle (fromIntegral $ fromEnum mode)
                 (fromIntegral offsetraw) (fromIntegral sizeraw)
        when (ptr == nullPtr) $
            throwErrnoPath ("lazy mmap of '" ++ filepath ++
                            "' chunk(" ++ show offset ++ "," ++ show size ++") failed") filepath
        let rawsizeptr = castIntToPtr sizeraw
        foreignptr <- newForeignPtrEnv c_system_io_mmap_munmap_funptr rawsizeptr ptr
        return (foreignptr,fromIntegral offset,size)

chunks :: Int64 -> Int64 -> [(Int64,Int)]
chunks _offset 0 = []
chunks offset size | size <= fromIntegral chunkSize = [(offset,fromIntegral size)]
                   | otherwise = let offset2 = ((offset + chunkSizeLong * 2 - 1) `div` chunkSizeLong) * chunkSizeLong
                                     size2   = offset2 - offset
                                     chunkSizeLong = fromIntegral chunkSize
                                 in (offset,fromIntegral size2) : chunks offset2 (size-size2)

-- | Maps region of file and returns it as 'BSL.ByteString'. File is
-- mapped in in 'ReadOnly' mode. See 'mmapFileForeignPtrLazy' for
-- details.
mmapFileByteStringLazy :: FilePath                     -- ^ name of file to map
                       -> Maybe (Int64,Int64)          -- ^ range to map, maps whole file if Nothing
                       -> IO BSL.ByteString            -- ^ bytestring with file content
mmapFileByteStringLazy filepath offsetsize = do
    list <- mmapFileForeignPtrLazy filepath ReadOnly offsetsize
    return (BSL.fromChunks (map turn list))
    where
        turn (foreignptr,offset,size) = BS.fromForeignPtr foreignptr offset size

-- | Unmaps memory region. As parameters use values marked as ptr and
-- rawsize in description of 'mmapFilePtr'.
munmapFilePtr :: Ptr a  -- ^ pointer
              -> Int    -- ^ rawsize
              -> IO ()
munmapFilePtr ptr rawsize = c_system_io_mmap_munmap (castIntToPtr rawsize) ptr

chunkSize :: Int
chunkSize = (128*1024 `div` fromIntegral c_system_io_granularity) * fromIntegral c_system_io_granularity

mmapFileOpen :: FilePath -> Mode -> IO (ForeignPtr ())
mmapFileOpen filepath' mode = do
    ptr <- withCString filepath' $ \filepath ->
        c_system_io_mmap_file_open filepath (fromIntegral $ fromEnum mode)
    when (ptr == nullPtr) $
        throwErrnoPath ("opening of '" ++ filepath' ++ "' failed") filepath'
    handle <- newForeignPtr c_system_io_mmap_file_close ptr
    return handle

--castPtrToInt :: Ptr a -> Int
--castPtrToInt ptr = ptr `minusPtr` nullPtr

castIntToPtr :: Int -> Ptr a
castIntToPtr int = nullPtr `plusPtr` int


-- | Should open file given as CString in mode given as CInt
foreign import ccall unsafe "HsMmap.h system_io_mmap_file_open"
    c_system_io_mmap_file_open :: CString       -- ^ file path, system encoding
                               -> CInt          -- ^ mode as 0, 1, 2, fromEnum
                               -> IO (Ptr ())   -- ^ file handle returned, nullPtr on error (and errno set)
-- | Used in finalizers, to close handle
foreign import ccall unsafe "HsMmap.h &system_io_mmap_file_close"
    c_system_io_mmap_file_close :: FunPtr(Ptr () -> IO ())

-- | Mmemory maps file from handle, using mode, starting offset and size
foreign import ccall unsafe "HsMmap.h system_io_mmap_mmap"
    c_system_io_mmap_mmap :: Ptr ()     -- ^ handle from c_system_io_mmap_file_open
                          -> CInt       -- ^ mode
                          -> CLLong     -- ^ starting offset, must be nonegative
                          -> CSize      -- ^ length, must be greater than zero
                          -> IO (Ptr a) -- ^ starting pointer to byte data, nullPtr on error (plus errno set)
-- | Used in finalizers
foreign import ccall unsafe "HsMmap.h &system_io_mmap_munmap"
    c_system_io_mmap_munmap_funptr :: FunPtr(Ptr () -> Ptr a -> IO ())
-- | Unmap region of memory. Size must be the same as returned by
-- mmap. If size is zero, does nothing (treats pointer as invalid)
foreign import ccall unsafe "HsMmap.h system_io_mmap_munmap"
    c_system_io_mmap_munmap :: Ptr () -> Ptr a -> IO ()
-- | Get file size in system specific manner
foreign import ccall unsafe "HsMmap.h system_io_mmap_file_size"
    c_system_io_file_size :: Ptr () -> IO CLLong
-- | Set file size in system specific manner. It is guaranteed to be called
-- only with new size being at least current size.
foreign import ccall unsafe "HsMmap.h system_io_mmap_extend_file_size"
    c_system_io_extend_file_size :: Ptr () -> CLLong -> IO CInt
-- | Memory mapping granularity.
foreign import ccall unsafe "HsMmap.h system_io_mmap_granularity"
    c_system_io_granularity :: CInt
