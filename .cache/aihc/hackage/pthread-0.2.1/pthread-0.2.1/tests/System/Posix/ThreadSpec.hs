{-# LANGUAGE ScopedTypeVariables #-}

module System.Posix.ThreadSpec (spec) where

import Control.Monad (forM_, replicateM, replicateM_)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)
import qualified System.Posix.Thread as Posix
import Test.Hspec

spec :: Spec
spec = do
    describe "create" $ do
      it "creates 1 threads successfully" $ do
        tid <- Posix.create_ (return ())
        Posix.detach tid
      it "creates 1000 threads successfully" $ do
        replicateM_ 1000 $ do
          tid <- Posix.create_ (return ())
          Posix.detach tid
    describe "join" $ do
      it "Joins 1000 threads successfully" $ do
        alloca $ \(ptr :: Ptr Int) -> do
          tids <- replicateM 1000 $ Posix.create (return ptr)
          forM_ tids $ \tid -> Posix.join tid `shouldReturn` ptr
    describe "exit" $ do
      it "Exits 1000 threads successfully" $ do
        replicateM_ 1000 $ do
          tid <- Posix.create_ Posix.exit_
          Posix.detach tid
