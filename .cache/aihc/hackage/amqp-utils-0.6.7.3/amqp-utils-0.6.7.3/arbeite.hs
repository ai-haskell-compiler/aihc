-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

-- generic AMQP rpc server
import           Control.Concurrent
import qualified Control.Exception             as X
import           Control.Monad
import qualified Data.ByteString.Char8         as BS
import           Data.Map                      (singleton)
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Time
import           Data.Version                  (showVersion)
import           Network.AMQP
import           Network.AMQP.Types
import           Network.AMQP.Utils.Connection
import           Network.AMQP.Utils.Helpers
import           Network.AMQP.Utils.Options
import           Paths_amqp_utils              (version)
import           System.Environment
import           System.IO

main :: IO ()
main = do
  hr "starting"
  tid <- myThreadId
  args <- getArgs >>= parseargs 'r'
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  X.onException
    (printparam "worker" $ fromJust $ fileProcess args)
    (error "-X option required")
  printparam "cleanup temp file" $ cleanupTmpFile args
  let addiArgs = reverse $ additionalArgs args
  printparam "client version" ["amqp-utils", showVersion version]
  (conn, chan) <- connect args
  addChannelExceptionHandler chan (X.throwTo tid)
  -- set prefetch
  printparam "prefetch" $ preFetch args
  qos chan 0 (preFetch args) False
  queue <-
    maybe
      (declareQueue
         chan
         newQueue {queueExclusive = True, queueName = (T.pack $ tmpQName args)} >>=
       (\(x, _, _) -> return x))
      (return)
      (fmap T.pack (qName args))
  printparam "queue name" queue
  if (currentExchange args /= "")
    then do
      printparam "exchange" $ currentExchange args
      bindQueue chan queue (T.pack $ currentExchange args) queue
    else return ()
  ctag <-
    consumeMsgs
      chan
      queue
      (if ack args
         then Ack
         else NoAck)
      (rpcServerCallback tid args addiArgs chan)
  printparam "consumer tag" ctag
  printparam "send acks" $ ack args
  printparam "requeue if rejected" $ (ack args) && (requeuenack args)
  hr "entering main loop"
  sleepingBeauty >>= printparam "exception"
  closeConnection conn
  hr "connection closed"

rpcServerCallback ::
     ThreadId -> Args -> [String] -> Channel -> (Message, Envelope) -> IO ()
rpcServerCallback tid a addi c m@(msg, env) = do
  let numstring = show $ envDeliveryTag env
  hr $ "BEGIN " ++ numstring
  now <- getZonedTime
  (callbackoptions, callbackenv) <-
    X.catch
      (printmsg Nothing m (anRiss a) now)
      (\x -> X.throwTo tid (x :: X.IOException) >> return ([], []))
  either (\e -> printparam "ERROR" (e :: X.IOException)) return =<<
    X.try
      (optionalFileStuff
         m
         callbackoptions
         addi
         numstring
         a
         tid
         (Just reply)
         callbackenv)
  hr $ "END " ++ numstring
  where
    reply e contents = do
      void $
        publishMsg
          c
          (envExchangeName env)
          (fromJust $ msgReplyTo msg)
          newMsg
            { msgBody = contents
            , msgCorrelationID = msgCorrelationID msg
            , msgTimestamp = msgTimestamp msg
            , msgExpiration = msgExpiration msg
            , msgHeaders =
                Just $
                FieldTable $ singleton "exitcode" $ FVString $ BS.pack $ show e
            }
