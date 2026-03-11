{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}

module Main where

import Test.Tasty.TH
import Test.Tasty.HUnit

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CC
import           Data.Conduit ((.|))
import           Control.Monad.IO.Class (liftIO)
import           Control.Exception (throwIO)
import           System.IO.Error (isDoesNotExistError, catchIOError)
import           System.Directory (doesFileExist, removeFile, createDirectory, removeDirectory)
import           System.IO (hPutStrLn)

import System.IO.SafeWrite
import Data.Conduit.SafeWrite

main :: IO ()
main = do
    removeFileIfExists outname
    $(defaultMainGenerator)

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists fp = removeFile fp `catchIOError` ignoreDoesNotExistError
    where
        ignoreDoesNotExistError e
                | isDoesNotExistError e = return ()
                | otherwise = throwIO e

outname :: FilePath
outname = "testing-output.txt"

case_create_output = do
    withOutputFile outname $ flip hPutStrLn "Hello World"
    (doesFileExist outname) >>= assertBool "Output file was not created"
    removeFile outname

case_not_create_on_exception = do
    (withOutputFile outname $ \h -> do
        hPutStrLn h "Hello World"
        throwIO $ userError "Something bad happened") `catchIOError` \_ -> return ()
    (not <$> doesFileExist outname) >>= assertBool "Output file was created despite exception being raised"

case_no_intermediate_output = do
    withOutputFile outname $ \h -> do
        hPutStrLn h "Hello World"
        partial <- doesFileExist outname
        assertBool "Partial file should not exist before internal action ends" (not partial)
    (doesFileExist outname) >>= assertBool "Output file was not created"
    removeFile outname


case_in_subdirectory = do
    let subdir = "subdirectory_for_testing"
        ofname = subdir ++ "/file.txt"

    createDirectory subdir
    withOutputFile ofname $ flip hPutStrLn "Hello World"
    (doesFileExist ofname) >>= assertBool "Output file was not created"
    removeFile ofname
    removeDirectory subdir

case_conduit_create_output = do
    C.runConduitRes $
        C.yield "Hello World" .| safeSinkFile outname
    (doesFileExist outname) >>= assertBool "Output file was not created"
    removeFile outname

case_conduit_create_output_pass = do
        C.runConduitRes $
            C.yield "Hello World"
                .| atomicConduitUseFile outname writeout
                .| CC.sinkNull
        (doesFileExist outname) >>= assertBool "Output file was not created"
        removeFile outname
    where
        writeout h = C.awaitForever $ \line -> do
            C.yield line
            liftIO $ B8.hPutStrLn h (B.take 8 line)

case_conduit_not_create_on_exception = do
    (C.runConduitRes $
        (do
            C.yield "Hello World"
            liftIO . throwIO $ userError "Something bad happened"
            ) .| safeSinkFile outname
        ) `catchIOError` \_ -> return ()
    (not <$> doesFileExist outname) >>= assertBool "Output file was created despite exception being raised"


case_conduit_no_intermediate_output = do
    C.runConduitRes $
        (do
            C.yield "Hello World"
            liftIO $ do
                partial <- doesFileExist outname
                assertBool "Partial file should not exist before conduit upstream ends" (not partial)
        ) .| safeSinkFile outname
    (doesFileExist outname) >>= assertBool "Output file was not created at the end of conduit processing"
    removeFile outname

case_conduit_in_subdirectory = do
    let subdir = "subdirectory_for_testing"
        ofname = subdir ++ "/file.txt"

    createDirectory subdir
    C.runConduitRes $
        C.yield "Hello World" .| safeSinkFile ofname
    (doesFileExist ofname) >>= assertBool "Output file was not created"
    removeFile ofname
    removeDirectory subdir

