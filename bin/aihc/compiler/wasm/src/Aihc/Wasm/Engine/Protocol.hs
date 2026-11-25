-- | The line protocol of an engine of "Aihc.Wasm.Engine".
--
-- This module has no process and no data files. Thus a program for
-- WebAssembly, such as the playground helper, can talk to an engine that
-- its host runs. "Aihc.Wasm.Engine" starts engines as processes.
module Aihc.Wasm.Engine.Protocol
  ( Engine,
    EngineStream (..),
    EngineValue (..),
    lineEngine,
    loadSharedModule,
    callFunction,
    startGuest,
    guestIsRunning,
    runClosure,
    tryRunClosure,
    runIndexedClosure,
    readReply,
  )
where

import Aihc.Native (renderLinkedGlobalSymbol)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | A running driver with one instance of an image.
data Engine = Engine
  { -- | Send one line to the engine. The line has no line break.
    engineSend :: !(String -> IO ()),
    -- | Read one line from the engine, without its line break. The result
    -- is 'Nothing' when the engine stopped.
    engineReceive :: !(IO (Maybe String)),
    -- | The function that gets the bytes that the instance writes to a
    -- standard stream. An engine without this function denies the writes.
    engineStreams :: !(Maybe (EngineStream -> ByteString -> IO ()))
  }

-- | A standard stream that the instance can write to.
data EngineStream
  = EngineStdout
  | EngineStderr
  deriving (Eq, Show)

-- | An argument of a function call.
data EngineValue
  = EngineI32 !Integer
  | EngineI64 !Integer
  deriving (Eq, Show)

-- | An engine that sends and reads the lines of its protocol with the two
-- functions. The engine must already have sent its first reply, which
-- shows that its instance is ready. The last argument gets the bytes of
-- each write of the instance to its standard output or to its standard
-- error. An engine without this function denies the writes.
lineEngine :: (String -> IO ()) -> IO (Maybe String) -> Maybe (EngineStream -> ByteString -> IO ()) -> Engine
lineEngine = Engine

-- | Load a shared module into the instance.
loadSharedModule :: Engine -> FilePath -> IO ()
loadSharedModule engine path
  | '\n' `elem` path = ioError (userError "the path of a shared module cannot contain a line break")
  | otherwise = void (request engine ("load " <> path))

-- | Call an exported function of the image or of a loaded shared module.
-- The result is the value of the function, if it has one.
callFunction :: Engine -> String -> [EngineValue] -> IO (Maybe Integer)
callFunction engine name arguments = do
  reply <- request engine (unwords ("call" : name : map render arguments))
  case reply of
    "" -> pure Nothing
    value -> maybe (ioError (userError ("the engine gave the result " <> value))) (pure . Just) (readMaybe value)
  where
    render value =
      case value of
        EngineI32 number -> "i32:" <> show number
        EngineI64 number -> "i64:" <> show number

-- | Start the program of the image, which is the guest of
-- 'engineGuestPath'. The guest then has control: the engine can load
-- shared modules and run closures, and it cannot call functions.
startGuest :: Engine -> IO ()
startGuest engine = void (request engine "start")

-- | If the guest runs. The guest stops when its program returns or traps.
-- The engine asks the guest for a closure that no module has. A guest
-- that runs gives the error of the driver for an unknown closure.
guestIsRunning :: Engine -> IO Bool
guestIsRunning engine = do
  engineSend engine "run ?"
  outcome <- readOutcomeWith (\_ -> ioError (userError "the engine sent a request that the command does not accept")) engine
  pure (either ("unknown closure" `isPrefixOf`) (const False) outcome)

-- | Run a closure in the guest. The closure is the static value of the type
-- @IO ()@ that the package, the module, and the name identify, in the image
-- or in a loaded shared module. The result has each message that the
-- closure sent with @aihc_engine_send@. An exception in the closure is an
-- error.
--
-- The closure can send a request with @aihc_engine_request@. The function
-- of the caller gets the bytes of each request and gives the bytes of the
-- reply. The closure waits for the reply.
runClosure :: Engine -> (ByteString -> IO ByteString) -> Text -> Text -> Text -> IO [ByteString]
runClosure engine answer package moduleName name =
  tryRunClosure engine answer package moduleName name >>= either (ioError . userError . ("the engine failed: " <>)) pure

-- | 'runClosure' with the failure of the closure as a value. The failure
-- is the text of the exception that the closure raised, or the text of a
-- trap. After a trap, the guest does not run.
tryRunClosure :: Engine -> (ByteString -> IO ByteString) -> Text -> Text -> Text -> IO (Either String [ByteString])
tryRunClosure engine answer package moduleName name =
  tryRunCommand engine answer (T.unpack (renderLinkedGlobalSymbol (T.intercalate "\0" [package, moduleName, name])))

-- | 'runClosure' for a native engine. The closure is the closure at the
-- index in the list of the program.
runIndexedClosure :: Engine -> (ByteString -> IO ByteString) -> Int -> IO [ByteString]
runIndexedClosure engine answer index =
  tryRunCommand engine answer (show index) >>= either (ioError . userError . ("the engine failed: " <>)) pure

-- | Send a @run@ command with the argument, and read the messages of the
-- closure.
tryRunCommand :: Engine -> (ByteString -> IO ByteString) -> String -> IO (Either String [ByteString])
tryRunCommand engine answer argument = do
  engineSend engine ("run " <> argument)
  outcome <- readOutcomeWith answerRequest engine
  case outcome of
    Left failure -> pure (Left failure)
    Right reply -> maybe (ioError (userError ("the engine gave the messages " <> reply))) (pure . Right) (mapM decodeHexadecimal (words reply))
  where
    answerRequest text = do
      bytes <- maybe (ioError (userError "the engine gave a request that is not valid")) pure (decodeHexadecimal text)
      encodeHexadecimal <$> answer bytes

decodeHexadecimal :: String -> Maybe ByteString
decodeHexadecimal = fmap BS.pack . bytes
  where
    bytes text =
      case text of
        [] -> Just []
        high : low : rest | isHexDigit high && isHexDigit low -> (fromIntegral (16 * digitToInt high + digitToInt low) :) <$> bytes rest
        _ -> Nothing

encodeHexadecimal :: ByteString -> String
encodeHexadecimal = concatMap byte . BS.unpack
  where
    byte value = [intToDigit (fromIntegral (value `div` 16)), intToDigit (fromIntegral (value `mod` 16))]

-- | Send one command and read its reply. The result is the text after
-- @ok@.
request :: Engine -> String -> IO String
request engine command = do
  engineSend engine command
  readReply engine

-- | Read the reply of a command that cannot send a request.
readReply :: Engine -> IO String
readReply = readReplyWith (\_ -> ioError (userError "the engine sent a request that the command does not accept"))

-- | Read the reply of a command. Before the reply, the engine can send
-- requests. The function gets the text of a request and gives the text of
-- its reply.
readReplyWith :: (String -> IO String) -> Engine -> IO String
readReplyWith answer engine =
  readOutcomeWith answer engine >>= either (ioError . userError . ("the engine failed: " <>)) pure

-- | 'readReplyWith' with the error of the command as a value.
readOutcomeWith :: (String -> IO String) -> Engine -> IO (Either String String)
readOutcomeWith answer engine = do
  received <- engineReceive engine
  case received of
    Nothing -> ioError (userError "the engine stopped")
    Just line ->
      case (words line, engineStreams engine) of
        ("ok" : _, _) -> pure (Right (drop 3 line))
        ("request" : payload, _) -> do
          reply <- answer (concat payload)
          engineSend engine ("reply " <> reply)
          readOutcomeWith answer engine
        ([stream, payload], Just write)
          | Just target <- lookup stream [("stdout", EngineStdout), ("stderr", EngineStderr)],
            Just bytes <- decodeHexadecimal payload -> do
              write target bytes
              readOutcomeWith answer engine
        _ -> pure (Left (drop 6 line))
