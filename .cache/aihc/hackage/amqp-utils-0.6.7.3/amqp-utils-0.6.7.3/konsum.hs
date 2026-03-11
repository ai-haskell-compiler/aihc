-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

-- generic amqp consumer
import Control.Concurrent
import qualified Control.Exception as X
import qualified Data.Text as T
import Data.Time
import Data.Version (showVersion)
import Network.AMQP
import Network.AMQP.Utils.Connection
import Network.AMQP.Utils.Helpers
import Network.AMQP.Utils.Options
import Paths_amqp_utils (version)
import System.Environment
import System.IO

main :: IO ()
main = do
  hr "starting"
  tid <- myThreadId
  args <- getArgs >>= parseargs 'k'
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let addiArgs = reverse $ additionalArgs args
  printparam "client version" ["amqp-utils", showVersion version]
  (conn, chan) <- connect args
  addChannelExceptionHandler chan (X.throwTo tid)
  -- set prefetch
  printparam "prefetch" $ preFetch args
  qos chan 0 (preFetch args) False
  -- attach to given queue? or build exclusive queue and bind it?
  queue <-
    maybe
      (tempQueue chan (tmpQName args) (bindings args))
      (return)
      (fmap T.pack (qName args))
  printparam "queue name" queue
  printparam "consumer args" $ formatheaders fieldshow $ streamoffset args
  printparam "shown body chars" $ anRiss args
  printparam "temp dir" $ tempDir args
  printparam "callback" $ fileProcess args
  printparam "callback args" $ addiArgs
  printparam "cleanup temp file" $
    maybe Nothing (\_ -> Just (cleanupTmpFile args)) (fileProcess args)
  -- subscribe to the queue
  ctag <-
    consumeMsgs'
      chan
      queue
      (if ack args
         then Ack
         else NoAck)
      (myCallback args addiArgs tid)
      (\_ -> return ())
      (streamoffset args)
  printparam "consumer tag" ctag
  printparam "send acks" $ ack args
  printparam "requeue if rejected" $ (ack args) && (requeuenack args)
  printparam "delay negative acknowledgements for" $ if delaynack args == 0 then Nothing else Just [(show (delaynack args)),"s"]

  hr "entering main loop"
  sleepingBeauty >>=
    (\x -> do
       closeConnection conn
       hr "connection closed"
       X.throw x)

-- | exclusive temp queue
tempQueue :: Channel -> String -> [(String, String)] -> IO T.Text
tempQueue chan tmpqname bindlist = do
  (q, _, _) <-
    declareQueue
      chan
      newQueue {queueExclusive = True, queueName = T.pack tmpqname}
  mapM_
    (\(xchange, bkey) ->
       bindQueue chan q (T.pack xchange) (T.pack bkey) >>
       printparam "binding" [xchange, bkey])
    bindlist
  return q

-- | process received message
myCallback :: Args -> [String] -> ThreadId -> (Message, Envelope) -> IO ()
myCallback a addi tid m@(_, envi) = do
  let numstring = show $ envDeliveryTag envi
  hr $ "BEGIN " ++ numstring
  now <- getZonedTime
  (callbackoptions, callbackenv) <-
    X.catch
      (printmsg Nothing m (anRiss a) now)
      (\x -> X.throwTo tid (x :: X.IOException) >> return ([], []))
  either (\e -> printparam "ERROR" (e :: X.IOException) >> reje envi a) return =<<
    X.try
      (optionalFileStuff
         m
         callbackoptions
         addi
         numstring
         a
         tid
         Nothing
         callbackenv)
  hr $ "END " ++ numstring
