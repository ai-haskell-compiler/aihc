{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Concurrent         (threadDelay)
import Control.Concurrent.Async   (forConcurrently_)
import Control.Exception          (bracket)
import Data.IORef
import Data.Proxy                 (Proxy (..))
import Data.Singletons.Bool       (reflectBool, SBoolI)
import System.FilePath            ((</>))
import System.IO
       (Handle, IOMode (ReadWriteMode), hClose, openFile)
import System.IO.Temp             (withSystemTempDirectory)
import Test.Tasty                 (TestTree, defaultMain, testGroup)
import Test.Tasty.ExpectedFailure (ignoreTest)
import Test.Tasty.HUnit           (testCase, (@=?))

import Lukko

import qualified Lukko.NoOp as NoOp

#ifdef HAS_OFD_LOCKING
import qualified Lukko.OFD as OFD
#endif

#ifdef HAS_FLOCK
import qualified Lukko.FLock as FLock
#endif

main :: IO ()
main = defaultMain $ testGroup "lukko" $
    [ testGroup "Lukko default" $ testSuite (Proxy :: Proxy FileLockingSupported) fileLockingSupported fdLock fdUnlock
    , testGroup "Lukko.NoOp" $ testSuite (Proxy :: Proxy NoOp.FileLockingSupported) NoOp.fileLockingSupported NoOp.fdLock NoOp.fdUnlock
    ]
#ifdef HAS_OFD_LOCKING
    ++ [ testGroup "Lukko.OFD" $ testSuite (Proxy :: Proxy OFD.FileLockingSupported) OFD.fileLockingSupported OFD.fdLock OFD.fdUnlock ]
#endif
#ifdef HAS_FLOCK
    ++ [ testGroup "Lukko.FLock" $ testSuite (Proxy :: Proxy FLock.FileLockingSupported) FLock.fileLockingSupported FLock.fdLock FLock.fdUnlock ]
#endif

testSuite
    :: forall supported. SBoolI supported
    => Proxy supported
    -> Bool
    -> (FD -> LockMode -> IO ())
    -> (FD -> IO ())
    -> [TestTree]
testSuite suppP supp implLock implUnlock =
    [ modify $ testCase "concurrent threads" $ do
        let n = 10 :: Int
        ref <- newIORef 0

        withSystemTempDirectory "handle-lock-tests" $ \tmpDir -> do
            -- print tmpDir
            forConcurrently_ [1 :: Int .. n] $ \_ ->
                withLock (tmpDir </> "lock") $ do
                    val <- readIORef ref
                    threadDelay 10000 -- 10ms
                    writeIORef ref (succ val)

        val <- readIORef ref
        val @=? n
    , testCase "FileLockingSupported and fileLockingSupported agree" $
          reflectBool suppP @=? supp
    ]
  where
    modify | supp      = id
           | otherwise = ignoreTest
    withLock = genWithLock implLock implUnlock

genWithLock
    :: (FD -> LockMode -> IO ())
    -> (FD -> IO ())
    -> FilePath
    -> IO a
    -> IO a
genWithLock implLock implUnlock fp action =
    bracket takeLock releaseLock (const action)
  where
    takeLock = do
        fd <- fdOpen fp
        implLock fd ExclusiveLock
        return fd

    releaseLock :: FD -> IO ()
    releaseLock fd = do
        implUnlock fd
        fdClose fd
