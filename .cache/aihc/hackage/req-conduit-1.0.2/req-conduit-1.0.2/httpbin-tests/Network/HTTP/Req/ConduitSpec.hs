{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Req.ConduitSpec (spec) where

import Control.Exception (throwIO)
import Control.Monad
import Data.ByteString qualified as B
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Binary qualified as CB
import Data.Conduit.List qualified as CL
import Data.Int (Int64)
import Network.HTTP.Req
import Network.HTTP.Req.Conduit
import System.IO (Handle)
import System.IO.Temp
import Test.Hspec

spec :: Spec
spec = do
  describe "streaming 10 M request" $
    it "works" $ do
      let size :: Int64
          size = 10 * 1024 * 1024
          src = CL.replicate (10 * 1024) (B.replicate 1024 0)
      void
        ( req
            POST
            (httpbin /: "post")
            (ReqBodySource size src)
            ignoreResponse
            mempty
        ) ::
        IO ()

  describe "streaming 10 M response" $
    it "works" $ do
      let tempi :: (Handle -> IO ()) -> IO ()
          tempi f = withSystemTempFile "req-conduit" (const f)
      tempi $ \h -> do
        let size :: Int
            size = 10 * 1024 * 1024
        reqBr GET (httpbin /: "stream-bytes" /~ size) NoReqBody mempty $ \r ->
          runConduitRes $
            responseBodySource r .| CB.sinkHandle h

----------------------------------------------------------------------------
-- Instances

instance MonadHttp IO where
  handleHttpException = throwIO

----------------------------------------------------------------------------
-- Helpers

-- | 'Url' representing <https://httpbin.org>.
httpbin :: Url 'Https
httpbin = https "httpbin.org"
