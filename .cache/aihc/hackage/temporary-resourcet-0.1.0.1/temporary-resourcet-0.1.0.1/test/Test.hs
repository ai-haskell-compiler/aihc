module Main where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Monad                (liftM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           System.Directory             (doesDirectoryExist,
                                               doesFileExist)
import           System.IO.Temp

main :: IO ()
main = defaultMain $ testGroup "temporary-resourcet"
    [ testGroup "createTempDirectory"
        [ testCase "release" $ test_createTempDirectory_release @? "Directory still exists"
        , testCase "runResourceT" $ test_createTempDirectory_runResourceT @? "Directory still exists"
        ]
    , testGroup "openBinaryTempFile"
        [ testCase "release" $ test_openBinaryTempFile_release @? "File still exists"
        , testCase "runResourceT" $ test_openBinaryTempFile_runResourceT @? "File still exists"
        ]
    , testGroup "openTempFile"
        [ testCase "release" $ test_openTempFile_release @? "File still exists"
        , testCase "runResourceT" $ test_openTempFile_runResourceT @? "File still exists"
        ]
    ]

test_createTempDirectory_release :: IO Bool
test_createTempDirectory_release =
    runResourceT $ do
        (key, path) <- createTempDirectory Nothing "test-temporary-resourcet-"
        release key
        liftIO $ liftM not $ doesDirectoryExist path

test_createTempDirectory_runResourceT :: IO Bool
test_createTempDirectory_runResourceT = do
    path <- runResourceT $ do
        (_, path) <- createTempDirectory Nothing "test-temporary-resourcet-"
        return path
    liftM not $ doesDirectoryExist path

test_openBinaryTempFile_release :: IO Bool
test_openBinaryTempFile_release =
    runResourceT $ do
        (key, path, _) <- openBinaryTempFile Nothing "test-temporary-resourcet-"
        release key
        liftIO $ liftM not $ doesFileExist path

test_openBinaryTempFile_runResourceT :: IO Bool
test_openBinaryTempFile_runResourceT = do
    path <- runResourceT $ do
        (_, path, _) <- openBinaryTempFile Nothing "test-temporary-resourcet-"
        return path
    liftM not $ doesFileExist path

test_openTempFile_release :: IO Bool
test_openTempFile_release =
    runResourceT $ do
        (key, path, _) <- openTempFile Nothing "test-temporary-resourcet-"
        release key
        liftIO $ liftM not $ doesFileExist path

test_openTempFile_runResourceT :: IO Bool
test_openTempFile_runResourceT = do
    path <- runResourceT $ do
        (_, path, _) <- openTempFile Nothing "test-temporary-resourcet-"
        return path
    liftM not $ doesFileExist path
