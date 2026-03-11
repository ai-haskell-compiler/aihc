{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-name-shadowing #-}
-- |an interface for using the methods in /var/lib/apt/methods
module Debian.Apt.Methods
    ( withMethodPath
    , withMethodURI
    , whichMethodPath
    , openMethod
    , closeMethod
    , recvStatus
    , sendCommand
    , getLastModified
    , simpleFetch
    , fetch
    , FetchCallbacks(..)
    , emptyFetchCallbacks
    , cliFetchCallbacks
    , Command(..)
    , Status(..)
    , Message, Site, User, Password, Media, Drive, Header, ConfigItem
    )
    where

import Debian.Time
import Debian.URI (URI(..), parseURI, uriToString')

import Control.Exception
import Control.Monad (liftM, unless)
import Control.Monad.Except
import Data.Maybe
import Data.Time
import System.Directory
import System.Exit
import System.IO
import System.Posix.Files
import System.Process

type MethodHandle = (Handle, Handle, Handle, ProcessHandle)

capabilities, logMsg, status, uriStart, uriDone, uriFailure, generalFailure, authorizationRequired, mediaFailure, uriAcquire, configuration, authorizationCredentials, mediaChanged :: String
capabilities = "100"
logMsg = "101"
status = "102"
uriStart = "200"
uriDone = "201"
uriFailure = "400"
generalFailure = "401"
authorizationRequired = "402"
mediaFailure = "403"
uriAcquire = "600"
configuration = "601"
authorizationCredentials = "602"
mediaChanged = "603"

type Message = String
type Site = String
type User = String
type Password = String
type Media = String
type Drive = String

data Status
    = Capabilities { version :: String, singleInstance :: Bool, preScan :: Bool, pipeline :: Bool, sendConfig :: Bool
                   , needsCleanup :: Bool, localOnly :: Bool }
    | LogMsg Message
    | Status URI Message
    | URIStart { uri :: URI, size :: Maybe Integer, lastModified :: Maybe UTCTime, resumePoint :: Maybe Integer }
    | URIDone { uri :: URI, size :: Maybe Integer,  lastModified :: Maybe UTCTime, resumePoint :: Maybe Integer
              , filename :: Maybe FilePath, hashes :: Hashes, imsHit :: Bool }
    | URIFailure { uri :: URI, message :: Message }
    | GeneralFailure Message
    | AuthorizationRequired Site
    | MediaFailure Media Drive
      deriving (Show, Eq)

data Hashes
    = Hashes { md5 :: Maybe String
             , sha1 :: Maybe String
             , sha256 :: Maybe String
             }
      deriving (Show, Eq)

emptyHashes = Hashes Nothing Nothing Nothing

data Command
    = URIAcquire URI FilePath (Maybe UTCTime)
    | Configuration [ConfigItem]
    | AuthorizationCredentials Site User Password
    | MediaChanged Media (Maybe Bool) -- I don't really understand the Fail field, I am assuming it is 'Fail: true'
      deriving (Show, Eq)

type Header = (String, String)
type ConfigItem = (String, String)

withMethodURI :: URI -> (MethodHandle -> IO a) -> IO a
withMethodURI uri f =
    do  mp <- liftM fromJust (whichMethodPath uri)
        withMethodPath mp f

-- |withMethod - run |methodPath| bracketed with
-- openMethod\/closeMethod. |f| gets the open handle.
withMethodPath :: FilePath -> (MethodHandle -> IO a) -> IO a
withMethodPath methodPath f =
    bracket (openMethod methodPath) closeMethod $ f

-- |whichMethodBinary - find the method executable associated with a URI
-- throws an exception on failure
whichMethodPath :: URI -> IO (Maybe FilePath)
whichMethodPath uri =
    let scheme = init (uriScheme uri)
        path = "/usr/lib/apt/methods/" ++ scheme
    in
      doesFileExist path >>= return . bool Nothing (Just path)

{-
The flow of messages starts with the method sending out a
100 Capabilities and APT sending out a 601 Configuration.

The flow is largely unsynchronized, but our function may have to
respond to things like authorization requests. Perhaps we do a
recvContents and then mapM_ over that ? Not all incoming messages
require a response, so...

-}

parseStatus :: [String] -> Status
parseStatus [] = error "parseStatus"
parseStatus (code' : headers') =
    parseStatus' (take 3 code') (map parseHeader headers')
    where
      parseStatus' code headers
          | code == capabilities =
              foldr updateCapability defaultCapabilities headers
                  where
                    updateCapability (a,v) c
                        | a == "Version"         = c { version = v }
                        | a == "Single-Instance" = c { singleInstance = parseTrueFalse v }
                        | a == "Pre-Scan"        = c { preScan = parseTrueFalse v }
                        | a == "Pipeline"        = c { pipeline = parseTrueFalse v }
                        | a == "Send-Config"     = c { sendConfig = parseTrueFalse v }
                        | a == "Needs-Cleanup"   = c { needsCleanup = parseTrueFalse v }
                        | a == "Local-Only"      = c { localOnly = parseTrueFalse v }
                        | otherwise = error $ "unknown capability: " ++ show (a,v)
                    defaultCapabilities =
                        Capabilities { version = ""
                                     , singleInstance = False
                                     , preScan        = False
                                     , pipeline       = False
                                     , sendConfig     = False
                                     , needsCleanup   = False
                                     , localOnly      = False
                                     }
      parseStatus' code headers
          | code == logMsg =
              case headers of
                [("Message", msg)] -> LogMsg msg
                _ -> error "parseStatus'"
          | code == status =
                Status (fromJust $ parseURI $ fromJust $ lookup "URI" headers) (fromJust $ lookup "Message" headers)
          | code == uriStart =
              foldr updateUriStart (URIStart undefined Nothing Nothing Nothing) headers
                  where
                    updateUriStart (a,v) u
                        | a == "URI" = u { uri = fromJust $ parseURI v }
                        | a == "Size" = u { size = Just (read v) }
                        | a == "Last-Modified" = u { lastModified = parseTimeRFC822 v } -- if the date is unparseable, we silently truncate. Is that bad ?
                        | a == "Resume-Point" = u { resumePoint = Just (read v) }
                    updateUriStart _ _ = error "updateUriStart"
      parseStatus' code headers
          | code == uriDone =
              foldr updateUriDone (URIDone undefined Nothing Nothing Nothing Nothing emptyHashes False) headers
                  where
                    updateUriDone (a,v) u
                        | a == "URI" = u { uri = fromJust $ parseURI v }
                        | a == "Size" = u { size = Just (read v) }
                        | a == "Last-Modified" = u { lastModified = parseTimeRFC822 v } -- if the date is unparseable, we silently truncate. Is that bad ?
                        | a == "Filename" = u { filename = Just v }
                        | a == "MD5Sum-Hash" = u { hashes = (hashes u) { md5    = Just v } }
                        | a == "MD5-Hash" = u { hashes = (hashes u) { md5    = Just v } }
                        | a == "SHA1-Hash"   = u { hashes = (hashes u) { sha1   = Just v } }
                        | a == "SHA256-Hash" = u { hashes = (hashes u) { sha256 = Just v } }
                        | a == "Resume-Point" = u { resumePoint = Just (read v) }
                        | a == "IMS-Hit" && v == "true" = u { imsHit = True }
                        | otherwise = error $ "updateUriDone: unknown header: " ++ show (a,v)
      parseStatus' code headers
          | code == uriFailure =
              URIFailure (fromJust $ parseURI $ fromJust $ lookup "URI" headers) (fromJust $ lookup "Message" headers)
          | code == generalFailure =
              GeneralFailure (fromJust $ lookup "Message" headers)
          | code == authorizationRequired =
              AuthorizationRequired (fromJust $ lookup "Site" headers)
          | code == mediaFailure =
              MediaFailure (fromJust $ lookup "Media" headers) (fromJust $ lookup "Drive" headers)
      parseStatus' _ _ = error "parseStatus'"

formatCommand :: Command -> [String]
formatCommand (URIAcquire uri filepath mLastModified) =
    [ uriAcquire ++ " URI Acquire"
    , "URI: " ++ uriToString' uri -- will this get credentials correct ? Or do we always pass those in seperately
    , "FileName: " ++ filepath
    ] ++ maybe [] (\lm -> ["Last-Modified: " ++ formatTimeRFC822 lm ]) mLastModified
formatCommand (Configuration configItems) =
    (configuration ++ " Configuration") : (map formatConfigItem configItems)
    where
      formatConfigItem (a,v) = concat ["Config-Item: ", a, "=", v]
formatCommand (AuthorizationCredentials site user passwd) =
    (authorizationCredentials ++ " Authorization Credentials") :
    [ "Site: " ++ site
    , "User: " ++ user
    , "Password: " ++ passwd
    ]
formatCommand (MediaChanged media mFail) =
    [ mediaChanged ++ " Media Changed"
    , "Media: " ++ media
    ] ++ maybe [] (\b -> ["Fail: " ++ case b of True -> "true" ; False -> "false"]) mFail


parseTrueFalse :: String -> Bool
parseTrueFalse "true" = True
parseTrueFalse "false" = False
parseTrueFalse s = error $ "Invalid boolean string: " ++ s


recvStatus :: MethodHandle -> IO Status
recvStatus mh = liftM parseStatus $ recv mh

sendCommand :: MethodHandle -> Command -> IO ()
sendCommand mh cmd = sendMethod mh (formatCommand cmd)


parseHeader :: String -> Header
parseHeader str =
    let (a, r) = span (/= ':') str
        v = dropWhile (flip elem ": \t") r
    in
      (a, v)

openMethod :: FilePath -> IO MethodHandle
openMethod methodBinary =
    do
      -- hPutStrLn stderr ("openMethod " ++ methodBinary)
      runInteractiveCommand methodBinary
      -- runInteractiveProcess methodBinary [] Nothing Nothing

sendMethod :: MethodHandle -> [String] -> IO ()
sendMethod (pIn, _pOut, _, _) strings =
    do
      -- hPutStrLn stderr "send:"
      mapM_ put strings
      hPutStrLn pIn ""
      hFlush pIn
    where
      put line =
          do
            -- hPutStrLn stderr ("  " ++ line)
            hPutStrLn pIn line

closeMethod :: MethodHandle -> IO ExitCode
closeMethod (pIn, pOut, pErr, handle) =
    do
      -- hPutStrLn stderr "closeMethod"
      hClose pIn
      hClose pOut
      hClose pErr
      waitForProcess handle

recv :: MethodHandle -> IO [String]
recv (_pIn, pOut, _pErr, _pHandle) =
    do
      -- hPutStrLn stderr "recv:"
      readTillEmptyLine pOut
    where
      readTillEmptyLine pOut =
          do
            line <- hGetLine pOut
            case line of
              "" -> return []
              line ->
                  do
                    -- hPutStrLn stderr ("  " ++ line)
                    tail <- readTillEmptyLine pOut
                    return $ line : tail
{-
The flow of messages starts with the method sending out a
<em>100 Capabilities</> and APT sending out a <em>601 Configuration</>.

The flow is largely unsynchronized, but our function may have to
respond to things like authorization requests. Perhaps we do a
recvContents and then mapM_ over that ? Not all incoming messages
require a response.

We probably also need to track state, for example, if we are
pipelining multiple downloads and want to show seperate progress bars
for each download.

If someone wants to use fetch, they will need to provide methods to:

 1. prompt for and provide authentication
 2. show progress
 3. show media change dialog
 4. Show log messages
 5. Show failures
 6. Send Configuration

pipeline vs non-pipeline mode.
what if different methods are being used ?

when pipelining, we probably don't want to have too many pipelines to
the same server. Perhaps there can be a limit, and for non-pipelinable
methods, we set the limit to 1.

Each method can run in a seperate thread, since methods do not
interact with each other. In fact, each unique method+uri can be a
seperate thread. We can use a MVar to track the global max download
count. Perhaps we also want a per host throttle, since it is the host
connect that is likely to max out, not the access method.

Plan:

partition fetches by (host,method).
fork off threads for each (host, method).
Use MVar to throttle per host, and total connections

We don't know if a method supports pipelining until we connect atleast
once. So if we have a non-pipelined method, we might want to start
multiple streams. On the other hand, for something like a CDROM, that
will just cause the system to thrash.

cdrom, file, etc, don't have a host, so that is not a unique key then.
Pipelining on local methods is tricky, because it is hard to tell if
the local methods point to the same device or not.

Even though we have multiple threads, the interactor can view the
incoming Stream as a single Stream because all the events are tagged
with the URI (i think). But, sending commands involves a fancy
router. We could include a reference to corresponding command for each
stream.

For now, let's serialize the transfers, but allow pipeling for methods
that really allow pipelining.

-}

data FetchCallbacks
    = FetchCallbacks { logCB :: Message ->  IO ()
                     , statusCB :: URI -> Message -> IO ()
                     , uriStartCB :: URI -> Maybe Integer -> Maybe UTCTime -> Maybe Integer -> IO ()
                     , uriDoneCB ::  URI -> Maybe Integer -> Maybe UTCTime -> Maybe Integer -> Maybe FilePath -> Hashes -> Bool -> IO ()
                     , uriFailureCB :: URI -> Message -> IO ()
                     , generalFailureCB :: Message -> IO ()
                     , authorizationRequiredCB :: Site -> IO (Maybe (User, Password))
                     , mediaFailureCB :: Media -> Drive -> IO ()
                     , debugCB :: String -> IO ()
                     }

simpleFetch :: [ConfigItem] -> URI -> FilePath -> Maybe UTCTime -> IO Bool
simpleFetch = fetch cliFetchCallbacks

-- |fetch a single item, show console output
-- see also: getLastModified
fetch :: FetchCallbacks -> [ConfigItem] -> URI -> FilePath -> Maybe UTCTime -> IO Bool
fetch cb configItems uri fp lastModified =
    do withMethodURI uri $ \mh ->
        do s <- recvStatus mh
           debugCB cb ("<- " ++ show s)
           sendCommand' mh (URIAcquire uri fp lastModified)
           loop mh
    where
      sendCommand' mh c =
          do mapM_ (debugCB cb . ("-> " ++)) (formatCommand c)
             sendCommand mh c
      loop mh =
          do r <- recvStatus mh
             case r of
               Capabilities {} ->
                   do unless (null configItems) (sendCommand' mh (Configuration configItems))
                      loop mh
               LogMsg m ->
                   do logCB cb m
                      loop mh
               Status uri m ->
                   do statusCB cb uri m
                      loop mh
               URIStart uri size lastModified resumePoint ->
                   uriStartCB cb uri size lastModified resumePoint >> loop mh
               URIDone uri size lastModified resumePoint filename hashes imsHit ->
                   uriDoneCB cb uri size lastModified resumePoint filename hashes imsHit >> return True
               URIFailure uri message ->
                   uriFailureCB cb uri message >> return False
               GeneralFailure m -> generalFailureCB cb m >> return False
               AuthorizationRequired site ->
                   do mCredentials <- authorizationRequiredCB cb site
                      case mCredentials of
                        Nothing -> return False -- FIXME: do we need a force close option for closeMethod ?
                        Just (user, passwd) ->
                            do sendCommand' mh (AuthorizationCredentials site user passwd)
                               loop mh
               MediaFailure media drive ->
                    do mediaFailureCB cb media drive
                       return False

-- |set of callbacks which do nothing.
-- suitable for non-interactive usage. In the case authorization is
-- required, no credentials will be supplied and the download should
-- abort.
emptyFetchCallbacks =
    FetchCallbacks { logCB = \ _m -> return ()
                   , statusCB = \ _uri _m -> return ()
                   , uriStartCB = \ _uri _size _lastModified _resumePoint -> return ()
                   , uriDoneCB = \ _uri _size _lastModified _resumePoint _filename _hashes _imsHit -> return ()
                   , uriFailureCB = \ _uri _message -> return ()
                   , generalFailureCB = \ _m -> return ()
                   , authorizationRequiredCB = \ _site -> return Nothing
                   , mediaFailureCB = \ _media _drive -> return ()
                   , debugCB = \ _m -> return ()
                   }

cliFetchCallbacks =
    emptyFetchCallbacks { statusCB = \uri m -> putStrLn $ uriToString' uri ++ " : " ++ m
                        , uriStartCB = \ uri _size lastModified _resumePoint -> putStrLn $ uriToString' uri ++ " started. " ++ show lastModified
                        , uriDoneCB = \uri _size _lastModified _resumePoint _filename _hashes imsHit -> putStrLn $ uriToString' uri ++ (if imsHit then " cached." else " downloaded.")
                        , uriFailureCB = \uri message -> hPutStrLn stderr $ "URI Failure: " ++ uriToString' uri ++ " : " ++ message
                        , generalFailureCB = \message -> hPutStrLn stderr $ "General Failure: " ++ message
                        , authorizationRequiredCB = \site ->
                                                    do putStrLn $ "Authorization Required for " ++ site
                                                       putStrLn "Username: " >> hFlush stdout
                                                       user <- getLine
                                                       putStrLn "Password: " >> hFlush stdout
                                                       passwd <- getLine -- TODO: write a getPasswd function which does not echo input
                                                       return (Just (user, passwd))
                        , mediaFailureCB = \media drive -> hPutStrLn stderr $ "Media Failure: media=" ++ media ++" drive="++ drive
                        , debugCB = \m -> print m
                        }

{-
    FetchCallbacks { logCB = \m -> hPutStrLn stderr m
                   , statusCB = \uri m -> putStrLn (show uri ++" : "++ m)
                   , uriStartCB = \uri
                   }

defaultAuthenticate site =
    do putStrLn $ "Authorization Required for " ++ site
       putStrLn "Username: " >> hFlush stdout
       user <- getLine
       putStrLn "Password: " >> hFlush stdout
       passwd <- getLine -- TODO: write a getPasswd function which does not echo input
       return (user, passwd)
-}

{-
    let itemsByHost = groupOn (regName . fst) items
    in
      do totalQSem <- newQSem 16 -- max number of streams allowed for
         forkIO
    where
      regName = fmap uriRegName . uriAuthority
      withQSem :: QSem -> IO a -> IO a
      withQSem qSem f = bracket (waitQSem qSem) (const $ signalQSem qSem) (const f)

uris = map (fromJust . parseURI) [ "http://n-heptane.com/whee"
                                 , "file:/one/two/three"
                                 , "ssh://jeremy:aoeu@n-heptane.com"
                                 , "cdrom:/one"
                                 ]
-}

-- * Misc Helper Functions

bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True = t


getLastModified :: FilePath -> IO (Maybe UTCTime)
getLastModified fp =
    do e <- doesFileExist fp
       if e
          then getFileStatus fp >>= return . Just . epochTimeToUTCTime . modificationTime
          else return Nothing

{-
groupOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f) . sortBy (compare `on` f)

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
on f g x y = f (g x) (g y)
-}
