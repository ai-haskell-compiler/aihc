{-# LANGUAGE OverloadedStrings #-}

-- | The playground helper of The Lazy Reader.
--
-- The helper reads requests from standard input and writes one JSON line
-- for each. See "Aihc.Play.Protocol" for the framing.
--
-- With @--bundle FILE@, the helper reads a bundle of the core libraries
-- from disk at the start. Without arguments, the first request must be
-- @load-bundle@ with the bundle in its body. The page sends the bundle
-- that way, because the browser has no file system.
--
-- The helper runs Template Haskell splices through its host. During a
-- command, it can write a JSON line with the field @engine@:
--
-- * @{"engine":"open"}@ starts an engine with the image of the kit.
-- * @{"engine":"line","line":TEXT}@ gives one command line to the engine.
-- * @{"engine":"load","assembly":TEXT}@ asks the host to assemble and link
--   the assembly as a shared module, and to load it into the engine.
-- * @{"engine":"close"}@ stops the engine.
--
-- After each of the first three, the host writes the reply lines of the
-- engine to the standard input of the helper, as "Aihc.Wasm.Engine"
-- describes them. The reply to @load@ is one line, @ok@ or @error TEXT@.
module Main (main) where

import Aihc.Cli.PlayBundle (BundleStats (..))
import Aihc.Play
import Aihc.Play.Json (Json (..), renderJsonLine)
import Aihc.Play.Protocol (Request (..), readRequest)
import Aihc.Wasm.Engine.Protocol (lineEngine)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hFlush, hIsEOF, hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)

main :: IO ()
main = do
  arguments <- getArgs
  hSetBinaryMode stdin True
  hSetBuffering stdout LineBuffering
  session <- case arguments of
    ["--bundle", path] -> do
      bytes <- BS.readFile path
      either failWith (pure . Just) (bundleSession bytes)
    [] -> pure Nothing
    _ -> failWith "usage: aihc-play [--bundle FILE]"
  loop session

loop :: Maybe Session -> IO ()
loop session = do
  next <- readRequest stdin
  case next of
    Nothing -> pure ()
    Just (Left problem) -> do
      reply (failure (T.pack problem))
      loop session
    Just (Right request) ->
      case (requestCommand request, session) of
        ("load-bundle", _) ->
          case bundleSession (requestBody request) of
            Left problem -> do
              reply (failure (T.pack problem))
              loop session
            Right built -> do
              reply (JObject (("ok", JBool True) : statsFields (sessionBundleStats built)))
              loop (Just built)
        (name, Just ready)
          | Just command <- parseCommand name -> do
              reply =<< runCommand ready hostSplices command (decodeUtf8With lenientDecode (requestBody request))
              loop session
        (name, Nothing)
          | Just _ <- parseCommand name -> do
              reply (failure "no bundle is loaded")
              loop session
        (name, _) -> do
          reply (failure ("unknown command " <> name))
          loop session

-- | The splice host of the helper: the program that sends the requests.
hostSplices :: SpliceHost
hostSplices =
  SpliceHost
    { hostOpenEngine = do
        reply (engineMessage "open" [])
        ready <- engineLine
        case ready of
          "ok" -> pure (lineEngine (\line -> reply (engineMessage "line" [("line", JString (T.pack line))])) (Just <$> engineLine) Nothing)
          _ -> ioError (userError ("The engine did not start: " <> drop 6 ready)),
      hostLoadAssembly = \_ assembly -> do
        reply (engineMessage "load" [("assembly", JString assembly)])
        loaded <- engineLine
        case loaded of
          "ok" -> pure ()
          _ -> ioError (userError ("The splice module did not load: " <> drop 6 loaded)),
      hostCloseEngine = \_ -> reply (engineMessage "close" [])
    }
  where
    engineMessage kind fields = JObject (("engine", JString kind) : fields)

-- | One reply line of the engine from the standard input.
engineLine :: IO String
engineLine = do
  atEnd <- hIsEOF stdin
  if atEnd
    then ioError (userError "The input ended while the engine ran.")
    else T.unpack . decodeUtf8With lenientDecode . BC.filter (/= '\r') <$> BC.hGetLine stdin

reply :: Json -> IO ()
reply value = do
  TIO.putStr (renderJsonLine value)
  hFlush stdout

failure :: Text -> Json
failure message = JObject [("ok", JBool False), ("message", JString message)]

statsFields :: BundleStats -> [(Text, Json)]
statsFields stats =
  [ ("modules", JInt (statsModules stats)),
    ("namingBytes", JInt (statsNamingBytes stats)),
    ("typeBytes", JInt (statsTypeBytes stats)),
    ("facts", JInt (statsFacts stats))
  ]

failWith :: String -> IO a
failWith message = do
  hPutStrLn stderr ("aihc-play: " <> message)
  exitFailure
