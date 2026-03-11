{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Data.Default (def)
import Database.PostgreSQL.Simple
import Network.Wai.Session.PostgreSQL
import Test.Hspec

import qualified Data.ByteString as B

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Network.Wai.Session.PostgreSQL" $ it "handles sessions" $ do
    conn <- dbconnect
    conn' <- fromSimpleConnection conn
    store <- dbStore conn' testSettings
    purger conn' testSettings

    -- new session
    ((lookupSess1, insertSess1), mknewsessid) <- store Nothing
    sessid <- mknewsessid

    -- insert
    insertSess1 ("foo" :: B.ByteString) ("foo" :: B.ByteString)
    lookupSess1 "foo" `shouldReturn` Just "foo"

    -- update
    insertSess1 ("foo" :: B.ByteString) ("bar" :: B.ByteString)
    lookupSess1 "foo" `shouldReturn` Just "bar"

    -- non-existing key
    lookupSess1 "bar" `shouldReturn` Nothing

    -- existing session
    ((lookupSess2, insertSess2), mknewsessid) <- store $ Just sessid
    newsessid <- mknewsessid

    lookupSess2 "foo" `shouldReturn` Just "bar"

    newsessid `shouldBe` sessid

    -- invalid session
    let invalidsessid = "foobar"
    ((lookupSess3, insertSess3), mknewsessid) <- store $ Just invalidsessid
    newsessid2 <- mknewsessid

    newsessid2 `shouldNotBe` newsessid
    newsessid2 `shouldNotBe` invalidsessid

    lookupSess3 "foo" `shouldReturn` Nothing

    -- re-accessing session
    ((lookupSess4, insertSess4), mknewsessid) <- store $ Just sessid
    lookupSess4 "foo" `shouldReturn` Just "bar"

    -- purged session
    threadDelay 2000000
    ((lookupSess5, insertSess5), mknewsessid) <- store $ Just sessid
    lookupSess5 "foo" `shouldReturn` Nothing

dbconnect :: IO Connection
dbconnect = do
    let connectInfo = ConnectInfo {
          connectHost = "localhost"
        , connectPort = 5432
        , connectUser = "demo"
        , connectPassword = "omed"
        , connectDatabase = "demodb" }
    connectPostgreSQL $ postgreSQLConnectionString connectInfo

testSettings :: StoreSettings
testSettings = def { storeSettingsSessionTimeout=1 }
