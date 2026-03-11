{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Main where

import           Control.Applicative
import           Control.Monad

import           Test.Hspec
import           Test.SmallCheck
import           Test.SmallCheck.Series
import           Test.Hspec.SmallCheck

import           Data.Monoid         ((<>))
import           Data.Function
import           Data.List
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Vector         (Vector)
import qualified Data.Vector         as V
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Encode.Pretty

import           Network.HTTP.Conduit

import           GitHub.Types.Events



main :: IO ()
main = do
    httpManager <- newManager tlsManagerSettings
    hspec $ spec httpManager


spec :: Manager -> Spec
spec httpManager = do

    describe "GitHub.Types.Event" $ do
        it "should parse all events from the public timeline" $ flip shouldReturn True $ do
          req <- parseUrl $ "https://api.github.com/events"
          body <- httpLbs (req { requestHeaders = ("User-Agent", "github-types-haskell-test") : requestHeaders req }) httpManager

          case eitherDecode (responseBody body) :: Either String [Value] of
              Left e -> fail $ "Failed to parse response as list of JSON values: " <> e
              Right x -> do
                  forM_ x $ \v -> do
                      case fromJSON v :: Result Event of
                          Error s -> fail $ "Failed to parse event " <> s <> ", payload: " <> show (encodePretty v)
                          _ -> return ()

                  return True
