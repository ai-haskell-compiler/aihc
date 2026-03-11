-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- generic AMQP publisher
import           Control.Concurrent
import qualified Control.Exception                as X
import           Control.Monad                    (forM_, when)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BL
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Text                        as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Version                     (showVersion)
import           Data.Word                        (Word64)
import           Magic
import           Network.AMQP
import           Network.AMQP.Types
import           Network.AMQP.Utils.Connection
import           Network.AMQP.Utils.Helpers
import           Network.AMQP.Utils.Options
import           Paths_amqp_utils                 (version)
import qualified RawFilePath.Directory            as RD
import           System.Environment
import           System.Exit
import           System.FilePath.Posix.ByteString
#if linux_HOST_OS
import           System.INotify
#endif
import qualified System.Posix.Files.ByteString    as F
import           System.IO

main :: IO ()
main = do
  hr "starting"
  tid <- myThreadId
  args <- getArgs >>= parseargs 'a'
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  printparam "client version" ["amqp-utils", showVersion version]
  printparam "routing key" $ rKey args
  printparam "exchange" $ currentExchange args
  (conn, chan) <- connect args
  addChannelExceptionHandler chan (X.throwTo tid)
  printparam "confirm mode" $ confirm args
  if (confirm args)
    then do
      confirmSelect chan False
      addConfirmationListener chan confirmCallback
    else return ()
  let inputFile' = firstInputFile (inputFiles args)
  isDir <-
    if inputFile' == "-"
      then return False
      else F.getFileStatus inputFile' >>= return . F.isDirectory
  let publishOneMsg =
        publishOneMsg' chan args {removeSentFile = removeSentFile args && isDir}
  if isDir
    then do
      printparam "hotfolder mode" True
      printparam "initial scan" (initialScan args)
      printparam "watch create events" (watchCreate args)
      if isNothing (moveSentFileTo args)
        then printparam "remove sent file" (removeSentFile args)
        else printparam "move sent file to" (moveSentFileTo args)
    else printparam
           "input file"
           [ inputFile'
           , if (lineMode args)
               then "(line-by-line)"
               else ""
           ]
  X.catch
    (if isDir
       then do
#if linux_HOST_OS
         wds <- mapM (watchHotfolder args publishOneMsg) (inputFiles args)
         sleepingBeauty >>= (\x -> do
           forM_ wds (\(wd,folder) -> do
             removeWatch wd
             printparam "END watching" folder
             )
           X.throw x)
#else
         X.throw (X.ErrorCall "ERROR: watching a directory is only supported in Linux")
#endif
       else do
         hr $ "BEGIN sending"
         messageFile <-
           if inputFile' == "-"
             then BL.getContents
             else readFileRawLazy inputFile'
         if (lineMode args)
           then mapM_ (publishOneMsg (currentExchange args) (rKey args) Nothing) (BL.lines messageFile)
           else publishOneMsg (currentExchange args) (rKey args) (Just (inputFile')) messageFile
         hr "END sending"
         if (confirm args)
           then waitForConfirms chan >>= printparam "confirmed"
           else return ()
         X.catch (closeConnection conn) exceptionHandler
         )
    exceptionHandler

#if linux_HOST_OS
-- | watch a hotfolder
watchHotfolder ::
     Args
  -> (String -> String -> Maybe RawFilePath -> BL.ByteString -> IO ())
  -> (RawFilePath, String, String)
  -> IO (WatchDescriptor, RawFilePath)
watchHotfolder args publishOneMsg (folder, exchange, rkey) = do
  printparam "hotfolder" folder
  inotify <- initINotify
  wd <-
    addWatch
      inotify
      [CloseWrite, MoveIn, Create]
      folder
      (handleEvent (publishOneMsg exchange rkey) (suffix args) (watchCreate args) folder)
  hr "BEGIN watching"
  if (initialScan args)
    then RD.listDirectory folder
           >>= mapM_
                 (\fn ->
                    handleFile
                      (publishOneMsg exchange rkey)
                      (suffix args)
                      folder
                      fn)
    else return ()
  return (wd, folder)
#endif

-- | A handler for clean exit
exceptionHandler :: AMQPException -> IO ()
exceptionHandler (ChannelClosedException Normal txt) =
  printparam "exit" txt >> exitWith ExitSuccess
exceptionHandler (ConnectionClosedException Normal txt) =
  printparam "exit" txt >> exitWith ExitSuccess
exceptionHandler x = printparam "exception" x >> exitWith (ExitFailure 1)

-- | The handler for publisher confirms
confirmCallback :: (Word64, Bool, AckType) -> IO ()
confirmCallback (deliveryTag, isAll, ackType) =
  printparam
    "confirmed"
    [ show deliveryTag
    , if isAll
        then "all"
        else "this"
    , show ackType
    ]

#if linux_HOST_OS
-- | Hotfolder event handler
handleEvent ::
     (Maybe RawFilePath -> BL.ByteString -> IO ()) -> [BS.ByteString] -> Bool -> RawFilePath -> Event -> IO ()
-- just handle closewrite and movedin events
handleEvent func suffixes _ folder (Closed False (Just fileName) True) =
  handleFile func suffixes folder fileName
handleEvent func suffixes _ folder (MovedIn False fileName _) =
  handleFile func suffixes folder fileName
-- handle create events when requested (detects ln)
handleEvent func suffixes True folder (Created False fileName) =
  handleFile func suffixes folder fileName
handleEvent _ _ _ _ _ = return ()

-- | Hotfolder file handler
handleFile ::
     (Maybe RawFilePath -> BL.ByteString -> IO ())
  -> [BS.ByteString]
  -> RawFilePath
  -> RawFilePath
  -> IO ()
handleFile func suffixes@(_:_) folder fileName =
  when
    (any (flip BS.isSuffixOf fileName) suffixes)
    (handleFile func [] folder fileName)
handleFile func [] folder fileName = do
  fStatus <- F.getFileStatus path
  when (not ("." `BS.isPrefixOf` fileName) && F.isRegularFile fStatus && F.fileSize fStatus > 0)
    $ X.catch
        (readFileRawLazy path >>= func (Just path))
        (\e ->
           printparam "exception while processing" fileName
             >> printparam "exception in handleFile" (e :: X.IOException))
  where
    path = folder </> fileName
#endif

-- | Publish one message with our settings
publishOneMsg' ::
     Channel
  -> Args
  -> String
  -> String
  -> Maybe RawFilePath
  -> BL.ByteString
  -> IO ()
publishOneMsg' chan a exchange rkey fn content = do
  printparam "sending" fn
  (mtype, mencoding) <-
    if (magic a)
      then do
        let firstchunk = if BL.null content then BS.empty else head $ BL.toChunks content
        m <- magicOpen [MagicMimeType]
        magicLoadDefault m
        t <- BS.useAsCStringLen firstchunk (magicCString m)
        magicSetFlags m [MagicMimeEncoding]
        e <- BS.useAsCStringLen firstchunk (magicCString m)
        return (Just (T.pack t), Just (T.pack e))
      else return ((contenttype a), (contentencoding a))
  now <- getCurrentTime >>= return . floor . utcTimeToPOSIXSeconds
  publishMsg
    chan
    (T.pack $ exchange)
    (T.pack $ rkey)
    newMsg
      { msgBody = content
      , msgDeliveryMode = persistent a
      , msgTimestamp = Just now
      , msgID = msgid a
      , msgType = msgtype a
      , msgUserID = userid a
      , msgApplicationID = appid a
      , msgClusterID = clusterid a
      , msgContentType = mtype
      , msgContentEncoding = mencoding
      , msgReplyTo = replyto a
      , msgPriority = prio a
      , msgCorrelationID = corrid a
      , msgExpiration = msgexp a
      , msgHeaders = substheader (fnheader a) (fmap takeFileName fn) $ msgheader a
      } >>=
    printparam "sent"
  removeSentFileIfRequested (removeSentFile a) (moveSentFileTo a) fn
  where
    substheader ::
         [String] -> Maybe BS.ByteString -> Maybe FieldTable -> Maybe FieldTable
    substheader (s:r) (Just fname) old =
      substheader r (Just fname) (addheader' old s fname)
    substheader _ _ old = old
    removeSentFileIfRequested False _ _ = return ()
    removeSentFileIfRequested True _ Nothing = return ()
    removeSentFileIfRequested True Nothing (Just fname) =
      printparam "removing" fname >> RD.removeFile fname
    removeSentFileIfRequested True (Just path) (Just fname) =
      printparam "moving" [fname,"to",path] >>
      F.rename fname (replaceDirectory fname ((takeDirectory fname) </> path))
    addheader' :: Maybe FieldTable -> String -> BS.ByteString -> Maybe FieldTable
    addheader' Nothing k v =
      Just $ FieldTable $ M.singleton (T.pack k) (FVString v)
    addheader' (Just (FieldTable oldheader)) k v =
      Just $ FieldTable $ M.insert (T.pack k) (FVString v) oldheader
