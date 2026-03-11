-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

-- generic AMQP rpc client
import           Control.Concurrent
import qualified Control.Exception             as X
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.Text                     as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Version                  (showVersion)
import           Network.AMQP
import           Network.AMQP.Utils.Connection
import           Network.AMQP.Utils.Helpers
import           Network.AMQP.Utils.Options
import           Paths_amqp_utils              (version)
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
  hr "starting"
  tid <- myThreadId
  args <- getArgs >>= parseargs 'p'
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  X.onException
    (printparam "rpc_timeout" [show (rpc_timeout args), "s"])
    (error $ "invalid rpc_timeout")
  printparam "client version" ["amqp-utils", showVersion version]
  printparam "routing key" $ rKey args
  (conn, chan) <- connect args
  addChannelExceptionHandler chan (X.throwTo tid)
  (q, _, _) <- declareQueue chan newQueue {queueExclusive = True}
  if (currentExchange args /= "")
    then do
      printparam "exchange" $ currentExchange args
      bindQueue chan q (T.pack $ currentExchange args) q
    else return ()
  let inputFile' = firstInputFile (inputFiles args)
  printparam "input file" $ inputFile'
  message <-
    if inputFile' == "-"
      then BL.getContents
      else readFileRawLazy inputFile'
  printparam "output file" $ outputFile args
  h <-
    if outputFile args == "-"
      then return stdout
      else openBinaryFile (outputFile args) WriteMode
  ctag <- consumeMsgs chan q NoAck (rpcClientCallback h tid args)
  printparam "consumer tag" ctag
  now <- getCurrentTime >>= return . floor . utcTimeToPOSIXSeconds
  hr "publishing request"
  _ <-
    publishMsg
      chan
      (T.pack $ currentExchange args)
      (T.pack $ rKey args)
      newMsg
        { msgBody = message
        , msgReplyTo = Just q
        , msgCorrelationID = corrid args
        , msgExpiration = msgexp args
        , msgTimestamp = Just now
        , msgHeaders = msgheader args
        }
  hr "waiting for answer"
  _ <-
    forkIO
      (threadDelay (floor (1000000 * rpc_timeout args)) >>
       throwTo tid TimeoutException)
  X.catch
    (forever $ threadDelay 200000)
    (\x -> do
       ec <- exceptionHandler x
       hr "closing connection"
       closeConnection conn
       printparam "exiting" ec
       exitWith ec)

exceptionHandler :: RpcException -> IO (ExitCode)
exceptionHandler ReceivedException = hr "success" >> (return ExitSuccess)
exceptionHandler TimeoutException  = hr "timeout" >> (return $ ExitFailure 1)

rpcClientCallback :: Handle -> ThreadId -> Args -> (Message, Envelope) -> IO ()
rpcClientCallback h tid a m@(_, env) = do
  let numstring = show $ envDeliveryTag env
  hr $ "received " ++ numstring
  now <- getZonedTime
  _ <-
    X.catch
      (printmsg (Just h) m (anRiss a) now)
      (\x -> X.throwTo tid (x :: X.IOException) >> return ([], []))
  throwTo tid ReceivedException

data RpcException
  = ReceivedException
  | TimeoutException
  deriving (Show)

instance X.Exception RpcException
