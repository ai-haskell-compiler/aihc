{-# LANGUAGE OverloadedStrings #-}

module TokenSpec where

import Control.Concurrent
import Crypto.Token
import Test.Hspec

----------------------------------------------------------------

spec :: Spec
spec = do
    describe "crypto token" $ do
        it "encrypt & decrypt ByteString" $ do
            mgr <- spawnTokenManager $ defaultConfig{interval = 1, tokenLifetime = 3}
            tf <- encryptToken mgr "Foo"
            threadDelay 1100000
            decryptToken mgr tf `shouldReturn` Just "Foo"
            tb <- encryptToken mgr "Bar"
            threadDelay 1100000
            decryptToken mgr tf `shouldReturn` Just "Foo"
            decryptToken mgr tb `shouldReturn` Just "Bar"
            threadDelay 1100000
            decryptToken mgr tf `shouldReturn` Nothing
            decryptToken mgr tb `shouldReturn` Just "Bar"
            threadDelay 1100000
            decryptToken mgr tf `shouldReturn` Nothing
            decryptToken mgr tb `shouldReturn` Nothing
