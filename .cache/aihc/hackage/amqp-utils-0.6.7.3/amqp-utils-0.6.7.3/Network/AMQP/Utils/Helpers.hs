-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Utils.Helpers where

import           Control.Concurrent
import qualified Control.Exception                as X
import           Control.Monad
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BL
import qualified Data.ByteString.UTF8             as BU
import           Data.Int                         (Int64)
import           Data.List
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Word                        (Word16)
import           Network.AMQP
import           Network.AMQP.Types
import           Network.AMQP.Utils.Options
import           Network.Socket                   (PortNumber)
import           System.Directory                 (removeFile)
import           System.Environment               (getEnvironment)
import           System.Exit
import           System.FilePath.Posix.ByteString (RawFilePath)
import           System.IO
import           System.Posix.IO.ByteString
import           System.Process

-- | print config parameters
class (Show a) =>
      Flexprint a
  where
  flexprint :: a -> IO ()
  flexprint = (hPutStrLn stderr) . show
  empty :: a -> Bool
  empty _ = False
  printparam :: String -> a -> IO ()
  printparam label x =
    if empty x
      then return ()
      else do
        mapM_ (hPutStr stderr) [" --- ", label, ": "]
        flexprint x
        hFlush stderr

instance (Flexprint a) => Flexprint (Maybe a) where
  empty = isNothing
  printparam _ Nothing  = return ()
  printparam x (Just y) = printparam x y

instance Flexprint BS.ByteString where
  flexprint = BS.hPutStrLn stderr
  empty = BS.null

instance Flexprint String where
  flexprint = hPutStrLn stderr
  empty = null

instance Flexprint [BS.ByteString] where
  flexprint = flexprint . BS.unwords
  empty = null

instance Flexprint [String] where
  flexprint = flexprint . unwords
  empty = null

instance Flexprint [Maybe BS.ByteString] where
  flexprint = flexprint . catMaybes
  empty = null . catMaybes

instance Flexprint [Maybe String] where
  flexprint = flexprint . catMaybes
  empty = null . catMaybes

instance Flexprint T.Text where
  flexprint = flexprint . T.unpack
  empty = T.null

instance Flexprint BL.ByteString where
  flexprint x = hPutStrLn stderr "" >> BL.hPut stderr x >> hPutStrLn stderr ""
  empty = BL.null

instance Flexprint Bool where
  empty = not

instance Flexprint Int

instance Flexprint Int64

instance Flexprint Word16

instance Flexprint ExitCode

instance Flexprint X.SomeException

instance Flexprint X.IOException

instance Flexprint AMQPException

instance Flexprint ConfirmationResult

instance Flexprint PortNumber

-- | log marker
hr :: String -> IO ()
hr x = hPutStrLn stderr hr' >> hFlush stderr
  where
    hr' = take 72 $ (take 25 hr'') ++ " " ++ x ++ " " ++ hr''
    hr'' = repeat '-'

-- | format headers for printing
formatheaders :: ((T.Text, FieldValue) -> [a]) -> FieldTable -> [a]
formatheaders f (FieldTable ll) = concat $ map f $ M.toList ll

-- | format headers for setting environment variables
formatheadersEnv ::
     ((Int, (T.Text, FieldValue)) -> [(String, String)])
  -> FieldTable
  -> [(String, String)]
formatheadersEnv f (FieldTable ll) = concat $ map f $ zip [0 ..] $ M.toList ll

-- | log formatting
fieldshow :: (T.Text, FieldValue) -> String
fieldshow (k, v) = "\n        " ++ T.unpack k ++ ": " ++ valueshow v

fieldshow' :: (T.Text, FieldValue) -> String
fieldshow' (k, v) = "\n           " ++ T.unpack k ++ ": " ++ valueshow v

-- | callback cmdline formatting
fieldshowOpt :: (T.Text, FieldValue) -> [String]
fieldshowOpt (k, v) = ["-h", T.unpack k ++ "=" ++ valueshow v]

-- | environment variable formatting
fieldshowEnv :: (Int, (T.Text, FieldValue)) -> [(String, String)]
fieldshowEnv (n, (k, v)) =
  [ ("AMQP_HEADER_KEY_" ++ nn, T.unpack k)
  , ("AMQP_HEADER_VALUE_" ++ nn, valueshow v)
  ]
  where
    nn = show n

-- | showing a FieldValue
valueshow :: FieldValue -> String
valueshow (FVString value)     = BU.toString value
valueshow (FVInt8 value)       = show value
valueshow (FVInt16 value)      = show value
valueshow (FVInt32 value)      = show value
valueshow (FVInt64 value)      = show value
valueshow (FVFloat value)      = show value
valueshow (FVDouble value)     = show value
valueshow (FVBool value)       = show value
valueshow (FVFieldTable value) = (formatheaders fieldshow') value
valueshow value                = show value

-- | skip showing body head if binary type
isimage :: Maybe String -> Bool
isimage Nothing = False
isimage (Just ctype)
  | isPrefixOf "application/xml" ctype = False
  | isPrefixOf "application/json" ctype = False
  | otherwise = any (flip isPrefixOf ctype) ["application", "image"]

-- | show the first bytes of message body
anriss' :: Maybe Int64 -> BL.ByteString -> BL.ByteString
anriss' x =
  case x of
    Nothing -> id
    Just y  -> BL.take y

-- | callback cmdline with optional parameters
printopt :: (String, Maybe String) -> [String]
printopt (_, Nothing)  = []
printopt (opt, Just s) = [opt, s]

-- | prints header and head on stderr and returns
-- cmdline options and environment variables to callback
printmsg ::
     Maybe Handle
  -> (Message, Envelope)
  -> Maybe Int64
  -> ZonedTime
  -> IO ([String], [(String, String)])
printmsg h (msg, envi) anR now = do
  mapM_
    (uncurry printparam)
    [ ("routing key", rkey)
    , ("message-id", messageid)
    , ("headers", headers)
    , ("content-type", ctype)
    , ("content-encoding", cenc)
    , ("redelivered", redeliv)
    , ("timestamp", timestamp'')
    , ("time now", now')
    , ("size", size)
    , ("priority", pri)
    , ("type", mtype)
    , ("user id", muserid)
    , ("application id", mappid)
    , ("cluster id", mclusterid)
    , ("reply to", mreplyto)
    , ("correlation id", mcorrid)
    , ("expiration", mexp)
    , ("delivery mode", mdelivmode)
    ]
  printparam label anriss
  mapM_ (\hdl -> BL.hPut hdl body >> hFlush hdl) h
  oldenv <- getEnvironment
  let environment =
        foldr
          step
          oldenv
          [ ("ROUTINGKEY", rkey)
          , ("CONTENTTYPE", ctype)
          , ("ENCODING", cenc)
          , ("MSGID", messageid)
          , ("TIMESTAMP", timestamp)
          , ("PRIORITY", pri)
          , ("REDELIVERED", redeliv)
          , ("SIZE", size)
          , ("TYPE", mtype)
          , ("USERID", muserid)
          , ("APPID", mappid)
          , ("CLUSTERID", mclusterid)
          , ("REPLYTO", mreplyto)
          , ("CORRID", mcorrid)
          , ("EXPIRATION", mexp)
          , ("DELIVERYMODE", mdelivmode)
          ] ++
        headersEnv
  return (cmdline, environment)
  where
    step (_, Nothing) xs = xs
    step (k, Just v) xs  = ("AMQP_" ++ k, v) : xs
    cmdline =
      concat
        (map
           printopt
           [ ("-r", rkey)
           , ("-m", ctype)
           , ("-e", cenc)
           , ("-i", messageid)
           , ("-t", timestamp)
           , ("-p", pri)
           , ("-R", redeliv)
           ] ++
         headersOpt)
    headers = fmap (formatheaders fieldshow) $ msgHeaders msg
    headersOpt =
      maybeToList $ fmap (formatheaders fieldshowOpt) $ msgHeaders msg
    headersEnv =
      concat . maybeToList $
      fmap (formatheadersEnv fieldshowEnv) $ msgHeaders msg
    body = msgBody msg
    anriss =
      if isimage ctype
        then Nothing
        else Just (anriss' anR body) :: Maybe BL.ByteString
    anriss'' = maybe "" (\a -> "first " ++ (show a) ++ " bytes of ") anR
    label = anriss'' ++ "body"
    ctype = fmap T.unpack $ msgContentType msg
    cenc = fmap T.unpack $ msgContentEncoding msg
    rkey = Just . T.unpack $ envRoutingKey envi
    messageid = fmap T.unpack $ msgID msg
    pri = fmap show $ msgPriority msg
    mtype = fmap T.unpack $ msgType msg
    muserid = fmap T.unpack $ msgUserID msg
    mappid = fmap T.unpack $ msgApplicationID msg
    mclusterid = fmap T.unpack $ msgClusterID msg
    mreplyto = fmap T.unpack $ msgReplyTo msg
    mcorrid = fmap T.unpack $ msgCorrelationID msg
    mexp = fmap T.unpack $ msgExpiration msg
    mdelivmode = fmap show $ msgDeliveryMode msg
    size = Just . show $ BL.length body
    redeliv =
      if envRedelivered envi
        then Just "YES"
        else Nothing
    tz = zonedTimeZone now
    nowutc = zonedTimeToUTCFLoor now
    msgtime = msgTimestamp msg
    msgtimeutc = fmap (posixSecondsToUTCTime . realToFrac) msgtime
    timestamp = fmap show msgtime
    timediff = fmap (difftime nowutc) msgtimeutc
    now' =
      case timediff of
        Just "now" -> Nothing
        _          -> showtime tz $ Just nowutc
    timestamp' = showtime tz msgtimeutc
    timestamp'' =
      liftM3
        (\a b c -> a ++ " (" ++ b ++ ") (" ++ c ++ ")")
        timestamp
        timestamp'
        timediff

-- | timestamp conversion
zonedTimeToUTCFLoor :: ZonedTime -> UTCTime
zonedTimeToUTCFLoor x =
  posixSecondsToUTCTime $
  realToFrac ((floor . utcTimeToPOSIXSeconds . zonedTimeToUTC) x :: Timestamp)

-- | show the timestamp
showtime :: TimeZone -> Maybe UTCTime -> Maybe String
showtime tz = fmap (show . (utcToZonedTime tz))

-- | show difference between two timestamps
difftime :: UTCTime -> UTCTime -> String
difftime now msg
  | now == msg = "now"
  | now > msg = diff ++ " ago"
  | otherwise = diff ++ " in the future"
  where
    diff = show (diffUTCTime now msg)

-- | if the message is to be saved
-- and maybe processed further
optionalFileStuff ::
     (Message, Envelope)
  -> [String]
  -> [String]
  -> String
  -> Args
  -> ThreadId
  -> Maybe (ExitCode -> BL.ByteString -> IO ())
  -> [(String, String)]
  -> IO ()
optionalFileStuff (msg, envi) callbackoptions addi numstring a tid action environment = do
  path <- saveFile (tempDir a) numstring (msgBody msg)
  printparam "saved to" path
  let callbackcmdline =
        liftM2
          (constructCallbackCmdLine (simple a) callbackoptions addi numstring)
          (fileProcess a)
          path
  printparam "calling" callbackcmdline
  maybe
    (acke envi a)
    (\c ->
       forkFinally
         (doProc a numstring envi c action path environment)
         (either (throwTo tid) return) >>
       return ())
    callbackcmdline

-- | save message into temp file
saveFile :: Maybe String -> String -> BL.ByteString -> IO (Maybe String)
saveFile Nothing _ _ = return Nothing
saveFile (Just tempD) numstring body = do
  (p, h) <-
    openBinaryTempFileWithDefaultPermissions
      tempD
      ("amqp-utils-" ++ numstring ++ "-.tmp")
  BL.hPut h body
  hClose h
  return $ Just p

-- | construct cmdline for callback script
constructCallbackCmdLine ::
     Bool -> [String] -> [String] -> String -> String -> String -> [String]
constructCallbackCmdLine True _ addi _ exe path = exe : addi ++ path : []
constructCallbackCmdLine False opts addi num exe path =
  exe : "-f" : path : "-n" : num : opts ++ addi

-- | call callback script
doProc ::
     Args
  -> String
  -> Envelope
  -> [String]
  -> Maybe (ExitCode -> BL.ByteString -> IO ())
  -> Maybe String
  -> [(String, String)]
  -> IO ()
doProc a numstring envi (exe:args) action path environment = do
  (_, h, _, processhandle) <-
    createProcess
      (proc exe args)
        {std_out = out, std_err = Inherit, env = Just environment'}
  sout <- mapM BL.hGetContents h
  exitcode <-
    maybe 0 id (fmap BL.length sout) `seq` waitForProcess processhandle
  printparam (numstring ++ " call returned") exitcode
  if isJust action && isJust sout
    then ((fromJust action $ exitcode) (fromJust sout)) >> acke envi a
    else case exitcode of
           ExitSuccess   -> acke envi a
           ExitFailure _ -> reje envi a
  if (cleanupTmpFile a)
    then X.catch
           (maybe (return ()) removeFile path)
           (\e -> printparam "error removing temp file" (e :: X.IOException))
    else return ()
  where
    out =
      if isJust action
        then CreatePipe
        else Inherit
    environment' =
      ("AMQP_NUMBER", numstring) : ("AMQP_FILE", fromJust path) : environment
doProc _ _ _ _ _ _ _ = return ()

-- | ack
acke :: Envelope -> Args -> IO ()
acke envi a
  | (ack a) = ackEnv envi
  | otherwise = return ()

-- | reject
reje :: Envelope -> Args -> IO ()
reje envi a
  | (ack a) = delayreject (delaynack a) >>  rejectEnv envi (requeuenack a)
  | otherwise = return ()

-- | optionally delay reject
delayreject :: Int -> IO ()
delayreject 0 = return ()
delayreject delay = do
  printparam "delaying negative acknowledgement for" [(show delay),"s"]
  threadDelay (delay * 1000000)

-- | main loop: sleep forever or wait for an exception
sleepingBeauty :: IO (X.SomeException)
sleepingBeauty =
  X.catch
    (forever (threadDelay 600000000) >>
     return (X.toException $ X.ErrorCall "not reached"))
    return

-- | extract first input file in case only one is needed
firstInputFile :: [(RawFilePath,String,String)] -> RawFilePath
firstInputFile []          = "-"
firstInputFile ((x,_,_):_) = x

-- | read RawFilePath to Lazy ByteString
readFileRawLazy :: RawFilePath -> IO BL.ByteString
readFileRawLazy path = do
  h <- openFd path ReadOnly defaultFlags >>= fdToHandle
  hSetBinaryMode h True
  BL.hGetContents h
  where
    defaultFlags = defaultFileFlags { noctty = True }
