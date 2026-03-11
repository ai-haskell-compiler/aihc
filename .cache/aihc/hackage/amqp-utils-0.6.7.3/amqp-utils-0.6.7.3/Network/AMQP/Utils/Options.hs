-- SPDX-FileCopyrightText: 2022 Frank Doepper
--
-- SPDX-License-Identifier: GPL-3.0-only

{-# LANGUAGE OverloadedStrings #-}

module Network.AMQP.Utils.Options where

import qualified Data.ByteString.Char8            as BS
import           Data.Default.Class
import           Data.Int                         (Int64)
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Text                        (Text, pack)
import           Data.Version                     (showVersion)
import           Data.Word                        (Word16)
import           Network.AMQP
import           Network.AMQP.Types
import           Network.Socket                   (PortNumber)
import           Paths_amqp_utils                 (version)
import           System.Console.GetOpt
import           System.FilePath.Posix.ByteString (RawFilePath)
import           Text.Read                        (readMaybe)

portnumber :: Args -> PortNumber
portnumber a
  | (port a) == Nothing && (tls a) = 5671
  | (port a) == Nothing = 5672
  | otherwise = fromJust (port a)

-- | A data type for our options
data Args =
  Args
    { server          :: String
    , port            :: Maybe PortNumber
    , tls             :: Bool
    , vHost           :: String
    , currentExchange :: String
    , bindings        :: [(String, String)]
    , rKey            :: String
    , anRiss          :: Maybe Int64
    , fileProcess     :: Maybe String
    , qName           :: Maybe String
    , cert            :: Maybe String
    , key             :: Maybe String
    , user            :: String
    , pass            :: String
    , preFetch        :: Word16
    , heartBeat       :: Maybe Word16
    , tempDir         :: Maybe String
    , additionalArgs  :: [String]
    , connectionName  :: Maybe String
    , tmpQName        :: String
    , inputFiles      :: [(RawFilePath,String,String)]
    , outputFile      :: String
    , lineMode        :: Bool
    , confirm         :: Bool
    , msgid           :: Maybe Text
    , msgtype         :: Maybe Text
    , userid          :: Maybe Text
    , appid           :: Maybe Text
    , clusterid       :: Maybe Text
    , contenttype     :: Maybe Text
    , contentencoding :: Maybe Text
    , replyto         :: Maybe Text
    , prio            :: Maybe Octet
    , corrid          :: Maybe Text
    , msgexp          :: Maybe Text
    , msgheader       :: Maybe FieldTable
    , fnheader        :: [String]
    , suffix          :: [BS.ByteString]
    , magic           :: Bool
    , persistent      :: Maybe DeliveryMode
    , ack             :: Bool
    , requeuenack     :: Bool
    , rpc_timeout     :: Double
    , connect_timeout :: Int
    , simple          :: Bool
    , cleanupTmpFile  :: Bool
    , removeSentFile  :: Bool
    , moveSentFileTo  :: Maybe RawFilePath
    , initialScan     :: Bool
    , streamoffset    :: FieldTable
    , delaynack       :: Int
    , watchCreate     :: Bool
    }

instance Default Args where
  def =
    Args
      "localhost"
      Nothing
      False
      "/"
      ""
      []
      ""
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      "guest"
      "guest"
      1
      Nothing
      Nothing
      []
      Nothing
      ""
      []
      "-"
      False
      False
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      []
      []
      False
      Nothing
      True
      True
      5
      600
      False
      False
      False
      Nothing
      False
      (FieldTable M.empty)
      0
      False

-- | all options
allOptions :: [(String, OptDescr (Args -> Args))]
allOptions =
  [ ( "k"
    , Option
        ['r']
        ["bindingkey"]
        (ReqArg
           (\s o -> o {bindings = (currentExchange o, s) : (bindings o)})
           "BINDINGKEY")
        ("AMQP binding key"))
  , ( "kr"
    , Option
        ['X']
        ["execute"]
        (OptArg
           (\s o ->
              o
                { fileProcess = Just (fromMaybe callback s)
                , tempDir = Just (fromMaybe "/tmp" (tempDir o))
                })
           "EXE")
        ("Callback Script File (implies -t) (-X without arg: " ++
         callback ++ ")"))
  , ( "kr"
    , Option
        ['a']
        ["args", "arg"]
        (ReqArg (\s o -> o {additionalArgs = s : (additionalArgs o)}) "ARG")
        "additional argument for -X callback")
  , ( "kr"
    , Option
        ['t']
        ["tempdir", "target"]
        (OptArg (\s o -> o {tempDir = Just (fromMaybe "/tmp" s)}) "DIR")
        "tempdir (default: no file creation, -t without arg: /tmp)")
  , ( "kr"
    , Option
        ['f']
        ["prefetch"]
        (ReqArg (\s o -> o {preFetch = read s}) "INT")
        ("Prefetch count. (0=unlimited, 1=off, default: " ++
         show (preFetch def) ++ ")"))
  , ( "kr"
    , Option
        ['A']
        ["ack"]
        (NoArg (\o -> o {ack = not (ack o)}))
        ("Toggle ack messages (default: " ++ show (ack def) ++ ")"))
  , ( "kr"
    , Option
        ['R']
        ["requeuenack"]
        (NoArg (\o -> o {requeuenack = not (requeuenack o)}))
        ("Toggle requeue when rejected (default: " ++
         show (requeuenack def) ++ ")"))
  , ( "a"
    , Option
        ['r']
        ["routingkey"]
        (ReqArg (\s o -> o {rKey = s}) "ROUTINGKEY")
        "AMQP routing key")
  , ( "p"
    , Option
        ['r', 'Q']
        ["routingkey", "qname"]
        (ReqArg (\s o -> o {rKey = s}) "ROUTINGKEY")
        "AMQP routing key")
  , ( "ap"
    , Option
        ['f']
        ["inputfile"]
        (ReqArg (\s o -> o {inputFiles = (BS.pack s,currentExchange o,rKey o):(inputFiles o)}) "INPUTFILE")
        ("Message input file (default: <stdin>)"))
  , ( "p"
    , Option
        ['O']
        ["outputfile"]
        (ReqArg (\s o -> o {outputFile = s}) "OUTPUTFILE")
        ("Message output file (default: " ++ (outputFile def) ++ ")"))
  , ( "a"
    , Option
        ['l']
        ["linemode"]
        (NoArg (\o -> o {lineMode = not (lineMode o)}))
        ("Toggle line-by-line mode (default: " ++ show (lineMode def) ++ ")"))
  , ( "a"
    , Option
        ['C']
        ["confirm"]
        (NoArg (\o -> o {confirm = not (confirm o)}))
        ("Toggle confirms (default: " ++ show (confirm def) ++ ")"))
  , ( "a"
    , Option
        []
        ["msgid"]
        (ReqArg (\s o -> o {msgid = Just $ pack s}) "ID")
        "Message ID")
  , ( "a"
    , Option
        []
        ["type"]
        (ReqArg (\s o -> o {msgtype = Just $ pack s}) "TYPE")
        "Message Type")
  , ( "a"
    , Option
        []
        ["userid"]
        (ReqArg (\s o -> o {userid = Just $ pack s}) "USERID")
        "Message User-ID")
  , ( "a"
    , Option
        []
        ["appid"]
        (ReqArg (\s o -> o {appid = Just $ pack s}) "APPID")
        "Message App-ID")
  , ( "a"
    , Option
        []
        ["clusterid"]
        (ReqArg (\s o -> o {clusterid = Just $ pack s}) "CLUSTERID")
        "Message Cluster-ID")
  , ( "a"
    , Option
        []
        ["contenttype"]
        (ReqArg (\s o -> o {contenttype = Just $ pack s}) "CONTENTTYPE")
        "Message Content-Type")
  , ( "a"
    , Option
        []
        ["contentencoding"]
        (ReqArg (\s o -> o {contentencoding = Just $ pack s}) "CONTENTENCODING")
        "Message Content-Encoding")
  , ( "a"
    , Option
        []
        ["replyto"]
        (ReqArg (\s o -> o {replyto = Just $ pack s}) "REPLYTO")
        "Message Reply-To")
  , ( "p"
    , Option
        ['t']
        ["rpc_timeout"]
        (ReqArg (\s o -> o {rpc_timeout = read s}) "SECONDS")
        ("How long to wait for reply (default: " ++
         show (rpc_timeout def) ++ ")"))
  , ( "a"
    , Option
        []
        ["prio"]
        (ReqArg (\s o -> o {prio = Just $ read s}) "PRIO")
        "Message Priority")
  , ( "ap"
    , Option
        []
        ["corrid"]
        (ReqArg (\s o -> o {corrid = Just $ pack s}) "CORRID")
        "Message CorrelationID")
  , ( "ap"
    , Option
        []
        ["exp"]
        (ReqArg (\s o -> o {msgexp = Just $ pack s}) "EXP")
        "Message Expiration")
  , ( "ap"
    , Option
        ['h']
        ["header"]
        (ReqArg
           (\s o -> o {msgheader = addheader (msgheader o) s})
           "HEADER=VALUE")
        "Message Headers")
  , ( "k"
    , Option
        []
        ["stream_offset"]
        (ReqArg
           (\s o -> o {streamoffset = mkStreamOffset s})
           "OFFSET")
        "x-stream-offset consumer argument")
  , ( "a"
    , Option
        ['F']
        ["fnheader"]
        (ReqArg (\s o -> o {fnheader = s : (fnheader o)}) "HEADERNAME")
        "Put filename into this header")
  , ( "a"
    , Option
        ['S']
        ["suffix"]
        (ReqArg (\s o -> o {suffix = (BS.pack s) : (suffix o)}) "SUFFIX")
        "Allowed file suffixes in hotfolder mode")
  , ( "a"
    , Option
        ['u']
        ["remove", "move"]
        (OptArg (\s o -> o {removeSentFile = True, moveSentFileTo = fmap BS.pack s}) "DIR")
        ("Remove (or move to DIR) sent file in hotfolder mode"))
  , ( "a"
    , Option
        ['d']
        ["dirscan"]
        (NoArg (\o -> o {initialScan = not (initialScan o)}))
        ("Toggle initial directory scan in hotfolder mode (default: " ++
         show (initialScan def) ++ ")"))
  , ( "a"
    , Option
        ['m']
        ["magic"]
        (NoArg (\o -> o {magic = not (magic o)}))
        ("Toggle setting content-type and -encoding from file contents (default: " ++
         show (magic def) ++ ")"))
  , ( "a"
    , Option
        ['e']
        ["persistent"]
        (NoArg (\o -> o {persistent = Just Persistent}))
        "Set persistent delivery")
  , ( "a"
    , Option
        ['E']
        ["nonpersistent"]
        (NoArg (\o -> o {persistent = Just NonPersistent}))
        "Set nonpersistent delivery")
  , ( "krp"
    , Option
        ['l']
        ["charlimit"]
        (ReqArg (\s o -> o {anRiss = Just (read s)}) "INT")
        "limit number of shown body chars (default: unlimited)")
  , ( "kr"
    , Option
        ['q']
        ["queue"]
        (ReqArg (\s o -> o {qName = Just s}) "QUEUENAME")
        "Ignore Exchange and bind to existing Queue")
  , ( "kr"
    , Option
        ['i']
        ["simple"]
        (NoArg
           (\o -> o {simple = True, cleanupTmpFile = not (cleanupTmpFile o)}))
        "call callback with one arg (filename) only")
  , ( "kr"
    , Option
        ['j']
        ["cleanup"]
        (NoArg (\o -> o {cleanupTmpFile = not (cleanupTmpFile o)}))
        "Toggle remove tempfile after script call. Default False, but default True if --simple (-i)")
  , ( "kr"
    , Option
        ['Q']
        ["qname"]
        (ReqArg (\s o -> o {tmpQName = s}) "TEMPQNAME")
        "Name for temporary exclusive Queue")
  , ( "akrp"
    , Option
        ['x']
        ["exchange"]
        (ReqArg (\s o -> o {currentExchange = s}) "EXCHANGE")
        ("AMQP Exchange (default: \"\")"))
  , ( "akrp"
    , Option
        ['o']
        ["server"]
        (ReqArg (\s o -> o {server = s}) "SERVER")
        ("AMQP Server (default: " ++ server def ++ ")"))
  , ( "akrp"
    , Option
        ['y']
        ["vhost"]
        (ReqArg (\s o -> o {vHost = s}) "VHOST")
        ("AMQP Virtual Host (default: " ++ vHost def ++ ")"))
  , ( "akrp"
    , Option
        ['p']
        ["port"]
        (ReqArg (\s o -> o {port = Just (read s)}) "PORT")
        ("Server Port Number (default: " ++ show (portnumber def) ++ ")"))
  , ( "akrp"
    , Option
        ['T']
        ["tls"]
        (NoArg (\o -> o {tls = not (tls o)}))
        ("Toggle TLS (default: " ++ show (tls def) ++ ")"))
  , ( "akrp"
    , Option
        ['c']
        ["cert"]
        (ReqArg (\s o -> o {cert = Just s}) "CERTFILE")
        ("TLS Client Certificate File"))
  , ( "akrp"
    , Option
        ['k']
        ["key"]
        (ReqArg (\s o -> o {key = Just s}) "KEYFILE")
        ("TLS Client Private Key File"))
  , ( "akrp"
    , Option
        ['U']
        ["user"]
        (ReqArg (\s o -> o {user = s}) "USERNAME")
        ("Username for Auth"))
  , ( "akrp"
    , Option
        ['P']
        ["pass"]
        (ReqArg (\s o -> o {pass = s}) "PASSWORD")
        ("Password for Auth"))
  , ( "akrp"
    , Option
        ['s']
        ["heartbeats"]
        (ReqArg (\s o -> o {heartBeat = (Just (read s))}) "INT")
        "heartbeat interval (0=disable, default: set by server)")
  , ( "akrp"
    , Option
        ['n']
        ["name"]
        (ReqArg (\s o -> o {connectionName = Just s}) "NAME")
        "connection name, will be shown in RabbitMQ web interface")
  , ( "akrp"
    , Option
        ['w']
        ["connect_timeout"]
        (ReqArg (\s o -> o {connect_timeout = read s}) "SECONDS")
        ("timeout for establishing initial connection (default: " ++
         show (connect_timeout def) ++ ")"))
  , ( "k"
    , Option
        ['D']
        ["delaynack"]
        (ReqArg (\s o -> o {delaynack = read s}) "SECONDS")
        ("delay negative acknowledgements (default: " ++
         show (delaynack def) ++ ")"))
  , ( "a"
    , Option
        ['L']
        ["watchcreate"]
        (NoArg (\o -> o {watchCreate = not (watchCreate o)}))
        ("Toggle watching Create (ln) events in hotfolder mode (default: " ++ show (watchCreate def) ++ ")"))
  ]

-- | Options for the executables
options :: Char -> [OptDescr (Args -> Args)]
options exename = map snd $ filter ((elem exename) . fst) allOptions

-- | Add a header with a String value
addheader :: Maybe FieldTable -> String -> Maybe FieldTable
addheader Nothing string =
  Just $ FieldTable $ M.singleton (getkey string) (getval string)
addheader (Just (FieldTable oldheader)) string =
  Just $ FieldTable $ M.insert (getkey string) (getval string) oldheader

getkey :: String -> Text
getkey s = pack $ takeWhile (/= '=') s

getval :: String -> FieldValue
getval s = FVString $ BS.pack $ tail $ dropWhile (/= '=') s

-- | Parse streamoffset argument as number or string
mkStreamOffset :: String -> FieldTable
mkStreamOffset s = FieldTable $ M.singleton (pack "x-stream-offset") value
  where
    value = maybe (FVString $ BS.pack s) FVInt64 $ readMaybe s

-- | 'parseargs' exename argstring
-- applies options onto argstring
parseargs :: Char -> [String] -> IO Args
parseargs exename argstring =
  case getOpt Permute opt argstring of
    (o, [], []) -> return $ foldl (flip id) def o
    (_, _, errs) ->
      ioError $ userError $ concat errs ++ usageInfo (usage exename) opt
  where
    opt = options exename

-- | the default callback for the -X option
callback :: String
callback = "/usr/lib/haskell-amqp-utils/callback"

usage :: Char -> String
usage exename =
  "\n\
  \amqp-utils " ++
  (showVersion version) ++
  "\n\n\
  \Usage:\n" ++
  (longname exename) ++
  " [options]\n\n\
  \Options:"

longname :: Char -> String
longname 'a' = "agitprop"
longname 'k' = "konsum"
longname 'r' = "arbeite"
longname 'p' = "plane"
longname _   = "command"
