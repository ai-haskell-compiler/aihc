{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Main where

import System.IO.MMap
import Data.ByteString.Char8 as BSC
import Data.ByteString.Unsafe as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Ptr
import System.Mem
import Control.Concurrent
import Test.HUnit
import System.Directory
import Foreign.C.Types (CInt(..),CLLong)
import Control.Monad
import System.IO
import Data.Int
import Control.Exception as E
#ifdef WINDOWS
import qualified System.Win32.File as W
#endif

ignoreExceptions doit = (doit >> return ()) `E.catch` (\(e :: IOError) -> return ())

foreign import ccall unsafe "HsMmap.h system_io_mmap_counters"
    c_system_io_counters :: IO CInt

removeFileDelayed :: FilePath -> IO ()
#ifdef WINDOWS
removeFileDelayed filepath = do
  h <- W.createFile filepath
                    W.dELETE
                    W.fILE_SHARE_NONE
                    Nothing
                    W.oPEN_ALWAYS
                    W.fILE_FLAG_DELETE_ON_CLOSE
                    Nothing
  W.closeHandle h
#else
removeFileDelayed filepath = removeFile filepath
#endif

content = BSC.pack "Memory mapping of files for POSIX and Windows"
contentLazy = BSL.fromChunks [content]

test_normal_readonly = do
    BSC.writeFile "test_normal.bin" content
    bs <- mmapFileByteString "test_normal.bin" Nothing
    bs @?= content

test_normal_readonly_many_times = do
    BSC.writeFile "test_normal.bin" content
    bs1 <- mmapFileByteString "test_normal.bin" Nothing
    bs2 <- mmapFileByteString "test_normal.bin" Nothing
    bs3 <- mmapFileByteString "test_normal.bin" Nothing
    BSC.concat [bs1,bs2,bs3] @?= BSC.concat [content, content, content]

test_normal_readonly_lazy = do
    let filename = "test_normalQ.bin"
    BSC.writeFile filename content
    bs <- mmapFileByteStringLazy filename Nothing
    bs @?= contentLazy

test_normal_readonly_zero_length = do
    BSC.writeFile "test_zerolength.bin" BSC.empty
    bs <- mmapFileByteString "test_zerolength.bin" Nothing
    bs @?= BSC.empty

test_non_existing_readonly = do
    ignoreExceptions $ removeFile "test_notexists.bin"
    ignoreExceptions $ do
        mmapFileByteString "test_notexists.bin" Nothing
        assertFailure "Should throw exception"

test_no_permission_readonly = do
    let filename = "test_nopermission.bin"
    ignoreExceptions $ setPermissions filename (setOwnerReadable True .
                                                setOwnerWritable True .
                                                setOwnerExecutable True .
                                                setOwnerSearchable True $ emptyPermissions)
    BSC.writeFile filename content
    setPermissions filename (emptyPermissions)
    permissions <- getPermissions filename
    -- no way to clear read flag under Windows, skip the test
    if not (readable permissions)
        then ignoreExceptions $ do
                 mmapFileByteString filename Nothing
                 assertFailure "Should throw exception"
        else return ()

test_normal_negative_offset_readonly = do
    ignoreExceptions $ removeFile "test_normal1.bin"
    BSC.writeFile "test_normal1.bin" content
    ignoreExceptions $ do
        mmapFileByteString "test_normal1.bin" (Just (-20,5))
        assertFailure "Should throw exception"

test_normal_negative_size_readonly = do
    ignoreExceptions $ removeFile "test_normal2.bin"
    BSC.writeFile "test_normal2.bin" content
    ignoreExceptions $ do
        mmapFileByteString "test_normal2.bin" (Just (0,-5))
        assertFailure "Should throw exception"

test_normal_offset_size_readonly = do
    let filename = "test_normal5.bin"
    BSC.writeFile filename content
    bs <- mmapFileByteString filename (Just (5,5))
    let exp = BSC.take 5 (BSC.drop 5 content)
    bs @?= exp

test_normal_offset_size_zero_readonly = do
    let filename = "test_normal6.bin"
    BSC.writeFile filename content
    bs <- mmapFileByteString filename (Just (5,0))
    let exp = BSC.empty
    bs @?= exp

test_normal_offset_size_zero_readonly_lazy = do
    let filename = "test_normal6x.bin"
    BSC.writeFile filename content
    bs <- mmapFileByteStringLazy filename (Just (5,0))
    let exp = BSL.empty
    bs @?= exp

test_normal_offset_beyond_eof_readonly = do
    let filename = "test_normal9.bin"
    BSC.writeFile filename content
    ignoreExceptions $ do
        mmapFileByteString filename (Just (1000,5))
        assertFailure "Should throw exception"

test_normal_offset_beyond_eof_readonly_lazy = do
    -- although lazy, should throw exception
    let filename = "test_normal9.bin"
    BSC.writeFile filename content
    ignoreExceptions $ do
        mmapFileByteStringLazy filename (Just (1000,5))
        assertFailure "Should throw exception"

test_normal_offset_plus_size_beyond_eof_readonly = do
    let filename = "test_normal7.bin"
    BSC.writeFile filename content
    ignoreExceptions $ do
        mmapFileByteString filename (Just (4,5000))
        assertFailure "Should throw exception"

test_normal_offset_plus_size_beyond_eof_readonly_lazy = do
    let filename = "test_normal7.bin"
    BSC.writeFile filename content
    ignoreExceptions $ do
        mmapFileByteStringLazy filename (Just (4,5000))
        assertFailure "Should throw exception"

test_normal_offset_plus_size_beyond_eof_readwriteex = do
    let filename = "test_normal8.bin"
    BSC.writeFile filename content
    mmapWithFilePtr filename ReadWriteEx (Just (4,5000)) $ \(ptr,size) -> do
        size @?= 5000
        bs <- BSC.packCStringLen (castPtr ptr,size)
        bs @?= BSC.take 5000 (BSC.drop 4 (content `BSC.append` BSC.replicate 10000 '\0'))

test_delete_while_mmapped = do
    let filename = "test_normalU.bin"
    BSC.writeFile filename content
    mmapWithFilePtr filename ReadOnly Nothing $ \(ptr,size) -> do
        removeFileDelayed filename
        bs <- BSC.packCStringLen (castPtr ptr,size)
        bs @?= content
    v <- doesFileExist filename
    False @=? v

test_readwriteex_lazy_make_a_touch = do
    let filename = "test_normal8.bin"
    BSC.writeFile filename content
    let threegb = 3*1000*1000*1000
    ignore <- mmapFileForeignPtrLazy filename ReadWriteEx (Just (4,threegb))
    let size = sum (Prelude.map (\(_,_,s) -> s) ignore)
    size @?= fromIntegral threegb

test_readwriteex_lazy_make_dont_touch = do
    let filename = "test_normal86.bin"
    BSC.writeFile filename content
    let threegb = 3*1000
    mmapFileForeignPtrLazy filename ReadWriteEx (Just (0,threegb))
    System.Mem.performGC
    threadDelay 1000
    size <- withFile filename ReadMode hFileSize
    size @?= fromIntegral threegb

test_create_offset_plus_size_readwriteex = do
    let filename = "test_normal9.bin"
    ignoreExceptions $ removeFile filename
    mmapWithFilePtr filename ReadWriteEx (Just (4,5000)) $ \(ptr,size) -> do
        size @?= 5000
        bs <- BSC.packCStringLen (castPtr ptr,size)
        bs @?= BSC.replicate 5000 '\0'

test_create_readwriteex_no_way = do
    let filename = "zonk/test_normal9.bin"
    ignoreExceptions $ mmapWithFilePtr filename ReadWriteEx (Just (4,5000)) $ \(ptr,size) -> do
        assertFailure "Should throw exception"

test_create_nothing_readwriteex_should_throw = do
    let filename = "test_normalA.bin"
    ignoreExceptions $ removeFile filename
    ignoreExceptions $ mmapWithFilePtr filename ReadWriteEx Nothing $ \(ptr,size) -> do
        size @?= 5000
        bs <- BSC.packCStringLen (castPtr ptr,size)
        bs @?= BSC.replicate 5000 '\0'
        assertFailure "Should throw exception"
    x <- doesFileExist filename
    x @?= False

test_change_two_places = do
    let filename = "test_normalAB.bin"
    BSC.writeFile filename content
    mmapWithFilePtr filename ReadWrite Nothing $ \(ptr1,size1) ->
        do
          -- this should change one common memory
          let v1 = 0x41414141::Int32
          poke (castPtr ptr1) v1
          v2 <- peek (castPtr ptr1)
          v2 @?= v1
          bs2 <- mmapFileByteString filename Nothing
          size1 @?= BSC.length bs2
          bs1 <- BSC.packCStringLen (castPtr ptr1,size1)
          bs1 @?= bs2

test_change_read_write = do
    let filename = "test_normalAC.bin"
    BSC.writeFile filename content
    mmapWithFilePtr filename ReadWrite Nothing $ \(ptr1,size1) ->
        do
          poke (castPtr ptr1) (0x41414141::Int32)
    bs3 <- BSC.readFile filename
    bs3 @?= BSC.pack "\x41\x41\x41\x41" `BSC.append` BSC.drop 4 content

test_writecopy = do
    let filename = "test_normalAD.bin"
    BSC.writeFile filename content
    mmapWithFilePtr filename WriteCopy Nothing $ \(ptr1,size1) ->
        do
          poke (castPtr ptr1) (0x41414141::Int32)
    -- change should NOT be reflected in file on disk
    bs3 <- BSC.readFile filename
    bs3 @?= content

test_counters_zero = do
    System.Mem.performGC
    threadDelay 1000
    counters <- c_system_io_counters
    return (counters @?= 0)

alltests = [ "Normal read only mmap" ~:
             test_normal_readonly
           , "Normal read only mmap lazy" ~:
             test_normal_readonly_lazy
           , "Zero length file mmap" ~:
             test_normal_readonly_zero_length
           , "File does not exist" ~:
             test_non_existing_readonly
           , "No permission to read file" ~:
             test_no_permission_readonly
           , "Signal error when negative offset given" ~:
             test_normal_negative_offset_readonly
           , "Signal error when negative size given" ~:
             test_normal_negative_size_readonly
           , "Test if we can cut part of file" ~:
             test_normal_offset_size_readonly
           , "Test if we can cut zero length part of file" ~:
             test_normal_offset_size_zero_readonly
           , "Test if we can cut zero length part of file lazy" ~:
             test_normal_offset_size_zero_readonly_lazy
           , "Should throw error if mmaping readonly beyond end of file" ~:
             test_normal_offset_beyond_eof_readonly
           , "Should throw error if mmaping readonly beyond end of file lazy" ~:
             test_normal_offset_beyond_eof_readonly_lazy
           , "Should throw error if mmaping readonly with size beyond end of file" ~:
             test_normal_offset_plus_size_beyond_eof_readonly
           , "Should throw error if mmaping readonly with size beyond end of file lazy" ~:
             test_normal_offset_plus_size_beyond_eof_readonly_lazy
           , "Should ReadWriteEx mmap existing file and resize" ~:
             test_normal_offset_plus_size_beyond_eof_readwriteex
           , "Should ReadWriteEx mmap new file and resize" ~:
             test_create_offset_plus_size_readwriteex
           , "ReadWriteEx must have range specified" ~:
             test_create_nothing_readwriteex_should_throw
           , "Report error in file creation" ~:
             test_create_readwriteex_no_way
           , "ReadWriteEx in lazy mode should set file size even if not touching" ~:
             test_readwriteex_lazy_make_dont_touch
           , "Remove file while mmaped" ~:
             test_delete_while_mmapped
           , "MMap byte string many times" ~:
             test_normal_readonly_many_times
           , "Mmap common memory" ~:
             test_change_two_places
           , "Mmap read write memory" ~:
             test_change_read_write
           , "Mmap WriteCopy mode" ~:
             test_writecopy

           --, "ReadWriteEx in lazy should extend file beyond 3GB when mapped in" ~:
           --  Test_readwriteex_lazy_make_a_touch
           -- insert tests above this line
           , "Counters should be zero" ~:
             test_counters_zero
           ]

main = do
    runTestTT (test alltests)

{-
main = do
    BSC.writeFile "test.bin" content
    bs <- mmapFileByteString "test.bin" Nothing
    BSC.putStrLn bs
    print (bs == content)
    bs2 <- mmapFileByteString "test.bin" (Just (5,5))
    print (bs2 == BSC.take 5 (BSC.drop 5 content))

    -- create 5 gigabyte file
    let l = 1024*1024*1024*5
    (f,s) <- mmapFileForeignPtr "test.bin" ReadWrite (Just (l,5))
    withForeignPtr f $ \f -> poke (castPtr f) (64::Word8)

    E.catch (do
              bs3 <- mmapFileByteString "test.bin" Nothing
              print (fromIntegral l==BSC.length bs3 + 5 ))
          (\E -> print True -- exception here is also ok
          )
    bs4 <- mmapFileByteStringLazy "test.bin" Nothing
    print (BSL.fromChunks [content] == BSL.take (fromIntegral $ BSC.length content) bs4)
    bs5 <- mmapFileByteStringLazy "test.bin" (Just (5,5))
    print (BSC.take 5 (BSC.drop 5 content) == BSC.concat (BSL.toChunks bs5))

    System.Mem.performGC
    threadDelay 10000

-}
