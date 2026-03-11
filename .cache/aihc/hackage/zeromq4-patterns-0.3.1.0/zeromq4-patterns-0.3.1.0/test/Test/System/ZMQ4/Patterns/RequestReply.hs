{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
module Test.System.ZMQ4.Patterns.RequestReply (tests) where

import Control.Concurrent.Async

import Control.Monad (replicateM)

import Data.Binary

import GHC.Generics (Generic)

import System.ZMQ4.Patterns.RequestReply

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

tests :: Test
tests = testGroup "System.ZMQ4.PAtterns.RequestReply" [
    testProperty "server client" prop_server_client
  ]

data Request = ReqA | ReqB deriving (Eq, Generic, Show)

instance Binary Request

instance Arbitrary Request where
    arbitrary = elements [ReqA, ReqB]

data Response = RepA | RepB deriving (Eq, Generic, Show)

instance Binary Response

instance RequestReply Request Response

reply :: Request -> IO Response
reply ReqA = return RepA
reply ReqB = return RepB

newtype TestSetup = TestSetup [Request] deriving (Show)

instance Arbitrary TestSetup where
    arbitrary = TestSetup <$> replicateM 10 arbitrary

prop_server_client :: TestSetup -> Property
prop_server_client (TestSetup reqs) = within (10*1000*1000) $ ioProperty $
    withAsync (responder addr reply) $ \_ ->
        checkAll reqs

  where
    checkAll :: [Request] -> IO Property
    checkAll [] = return (property True)
    checkAll (x:xs) = do
        y <- request addr x
        let flag = case x of ReqA -> (y == RepA)
                             ReqB -> (y == RepB)
        if flag then
            checkAll xs
        else
            return (property False)

    addr = "ipc:///tmp/zeromq4-patterns-test-req-rep.socket"
