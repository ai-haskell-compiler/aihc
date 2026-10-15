{-# LANGUAGE OverloadedStrings #-}

-- | The playground helper of The Lazy Reader.
--
-- The helper reads requests from standard input and writes one JSON line
-- for each. See "Aihc.Play.Protocol" for the framing.
--
-- With @--bundle FILE@, the helper reads a bundle of the core libraries
-- from disk at the start. With @--prim-src@ and @--prelude-src@, it
-- compiles the support modules from disk instead. Without arguments, the
-- first request must be @load-bundle@ with the bundle in its body, or
-- @load@ with the support modules in its body. The page sends the bundle
-- that way, because the browser has no file system.
module Main (main) where

import Aihc.Cli.PlayBundle (BundleStats (..))
import Aihc.Play
import Aihc.Play.Json (Json (..), renderJsonLine)
import Aihc.Play.Protocol (Request (..), parseFiles, readRequest)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Text.IO qualified as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (..), hFlush, hPutStrLn, hSetBinaryMode, hSetBuffering, stderr, stdin, stdout)

main :: IO ()
main = do
  arguments <- getArgs
  hSetBinaryMode stdin True
  hSetBuffering stdout LineBuffering
  session <- case arguments of
    ["--prim-src", primRoot, "--prelude-src", preludeRoot] -> do
      sources <- loadSupportSources primRoot preludeRoot
      either failWith (pure . Just) (buildSession sources)
    ["--bundle", path] -> do
      bytes <- BS.readFile path
      either failWith (pure . Just) (bundleSession bytes)
    [] -> pure Nothing
    _ -> failWith "usage: aihc-play [--bundle FILE | --prim-src DIR --prelude-src DIR]"
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
        ("load", _) ->
          case parseFiles (requestBody request) >>= buildSession of
            Left problem -> do
              reply (failure (T.pack problem))
              loop session
            Right built -> do
              reply (JObject [("ok", JBool True)])
              loop (Just built)
        ("load-bundle", _) ->
          case bundleSession (requestBody request) of
            Left problem -> do
              reply (failure (T.pack problem))
              loop session
            Right built -> do
              reply (JObject (("ok", JBool True) : maybe [] statsFields (sessionBundleStats built)))
              loop (Just built)
        (name, Just ready)
          | Just command <- parseCommand name -> do
              reply (runCommand ready command (decodeUtf8With lenientDecode (requestBody request)))
              loop session
        (name, Nothing)
          | Just _ <- parseCommand name -> do
              reply (failure "no support modules are loaded")
              loop session
        (name, _) -> do
          reply (failure ("unknown command " <> name))
          loop session

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
