-- | Run WebAssembly code in an instance that can take more code while it
-- lives.
--
-- An image is a core module that exports its memory, its function table,
-- and each symbol. A shared module is a position-independent module, which
-- "Aihc.Wasm.Lir" makes with the 'Aihc.Wasm.Lir.PositionIndependent' code
-- model. An engine has one instance of an image. It loads shared modules
-- into that instance, and each shared module can use the symbols of the
-- image and of the shared modules before it.
--
-- The engine is a Node.js process that runs @driver.mjs@. The instance has
-- no access to the host: the driver denies each import of the image, apart
-- from a clock that gives a fixed time.
module Aihc.Wasm.Engine
  ( Engine,
    EngineValue (..),
    engineDriverPath,
    engineGuestPath,
    engineNodeCommand,
    linkImage,
    linkSharedModule,
    withEngine,
    loadSharedModule,
    callFunction,
    startGuest,
    runClosure,
  )
where

import Aihc.DataFiles (getDataFileName)
import Aihc.Native (WasmSysroot (..), readWasmClangProcessWithExitCode, renderLinkedGlobalSymbol, wasmClangCommand, wasmSysroot)
import Control.Exception (bracket, onException)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, intToDigit, isHexDigit)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)
import Text.Read (readMaybe)

-- | A running driver with one instance of an image.
data Engine = Engine
  { engineInput :: !Handle,
    engineOutput :: !Handle
  }

-- | An argument of a function call.
data EngineValue
  = EngineI32 !Integer
  | EngineI64 !Integer
  deriving (Eq, Show)

-- | The driver script that the Node.js process runs.
engineDriverPath :: IO FilePath
engineDriverPath = getDataFileName "compiler/wasm/runtime/driver.mjs"

-- | The source of the guest: the main module of an image that runs
-- closures. Build it with the image link to get such an image.
engineGuestPath :: IO FilePath
engineGuestPath = getDataFileName "compiler/wasm/runtime/Guest.hs"

-- | The Node.js command. @AIHC_NODE@ replaces the default command.
engineNodeCommand :: IO FilePath
engineNodeCommand = fromMaybe "node" <$> lookupEnv "AIHC_NODE"

-- | Link an image from objects and archives. The image contains each member
-- of each archive and exports each symbol, because a shared module can
-- refer to any of them. An undefined function becomes an import, which the
-- driver denies when the instance calls it. The arguments for the system
-- libraries of the packages follow the archives.
--
-- The image also gets the globals of 'taskGlobalsAssembly'.
linkImage :: FilePath -> [FilePath] -> [FilePath] -> [String] -> IO ()
linkImage output objects archives libraries = do
  sysroot <- wasmSysroot
  let globals = output <> ".globals.o"
  assembleTaskGlobals globals
  runLinker
    ( ["-m", "wasm32", "--no-entry", "--export-all", "--export=__stack_pointer", "--export-table", "--growable-table", "--import-undefined", "--extra-features=atomics"]
        <> objects
        <> [globals]
        <> ["--whole-archive"]
        <> archives
        <> ["--no-whole-archive"]
        <> libraries
        <> [wasmSysrootLibc sysroot]
        <> maybeToList (wasmSysrootBuiltins sysroot)
        <> ["-o", output]
    )

-- | The globals that the task hook of the libc reads.
--
-- The libc of the sysroot keeps the thread-local storage and the stack of
-- each component model task. Its task hook reads @__tls_size@,
-- @__tls_align@, and @__init_stack_pointer@. Only a link with shared memory
-- or with cooperative threads makes these globals, and the image link uses
-- neither. An executable link removes the hook, because nothing calls it.
-- The image link exports each symbol, so it keeps the hook, and the link
-- fails with an undefined symbol. The linker takes the hook from the libc
-- when an object uses a thread function, such as @pthread_self@.
--
-- The engine has one task and never calls the hook. Thus, each global is
-- zero. Each definition is weak, so a global that the linker makes replaces
-- it. The hook also calls @__wasm_init_tls@ and @__wasm_set_tls_base@,
-- which become imports that the driver denies.
taskGlobalsAssembly :: String
taskGlobalsAssembly =
  unlines
    ( concat
        [ ["\t.globaltype\t" <> name <> ", i32, immutable", "\t.hidden\t" <> name, "\t.weak\t" <> name, name <> ":"]
        | name <- ["__tls_size", "__tls_align", "__init_stack_pointer"]
        ]
    )

-- | Assemble 'taskGlobalsAssembly' into an object at the path.
assembleTaskGlobals :: FilePath -> IO ()
assembleTaskGlobals object = do
  override <- lookupEnv "AIHC_WASM_CLANG"
  let (clang, targetArguments) = wasmClangCommand override
      source = object <> ".s"
  writeFile source taskGlobalsAssembly
  (exitCode, out, err) <- readWasmClangProcessWithExitCode clang (targetArguments <> ["-c", source, "-o", object])
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError ("clang failed (" <> show exitCode <> "): " <> if null err then out else err))

-- | Link position-independent objects and each member of the archives
-- into a shared module. Each symbol
-- that the objects do not define becomes an import, which the driver
-- supplies when it loads the module.
linkSharedModule :: FilePath -> [FilePath] -> [FilePath] -> IO ()
linkSharedModule output objects archives =
  runLinker (["-m", "wasm32", "-shared", "--experimental-pic", "--unresolved-symbols=import-dynamic", "--extra-features=atomics", "--no-entry"] <> objects <> ["--whole-archive"] <> archives <> ["--no-whole-archive", "-o", output])

runLinker :: [String] -> IO ()
runLinker arguments = do
  (exitCode, out, err) <- readProcessWithExitCode "wasm-ld" arguments ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError ("wasm-ld failed (" <> show exitCode <> "): " <> if null err then out else err))

-- | Start an engine with an instance of the image, run the action, and stop
-- the engine.
withEngine :: FilePath -> (Engine -> IO value) -> IO value
withEngine image action = bracket start stop (\(engine, _) -> action engine)
  where
    start = do
      node <- engineNodeCommand
      driver <- engineDriverPath
      (input, output, _, process) <-
        createProcess (proc node [driver, image]) {std_in = CreatePipe, std_out = CreatePipe}
      case (input, output) of
        (Just engineInput, Just engineOutput) -> do
          hSetBuffering engineInput LineBuffering
          let engine = Engine {engineInput, engineOutput}
          -- The driver writes one reply after it makes the instance.
          void (readReply engine) `onException` (hClose engineInput >> waitForProcess process)
          pure (engine, process)
        _ -> ioError (userError "the engine process has no pipes")
    stop :: (Engine, ProcessHandle) -> IO ()
    stop (engine, process) = do
      hClose (engineInput engine)
      void (waitForProcess process)
      hClose (engineOutput engine)

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
runClosure engine answer package moduleName name = do
  hPutStrLn (engineInput engine) ("run " <> T.unpack (renderLinkedGlobalSymbol (T.intercalate "\0" [package, moduleName, name])))
  reply <- readReplyWith answerRequest engine
  maybe (ioError (userError ("the engine gave the messages " <> reply))) pure (mapM decodeHexadecimal (words reply))
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
  hPutStrLn (engineInput engine) command
  readReply engine

-- | Read the reply of a command that cannot send a request.
readReply :: Engine -> IO String
readReply = readReplyWith (\_ -> ioError (userError "the engine sent a request that the command does not accept"))

-- | Read the reply of a command. Before the reply, the engine can send
-- requests. The function gets the text of a request and gives the text of
-- its reply.
readReplyWith :: (String -> IO String) -> Engine -> IO String
readReplyWith answer engine = do
  end <- hIsEOF (engineOutput engine)
  if end
    then ioError (userError "the engine stopped")
    else do
      line <- hGetLine (engineOutput engine)
      case words line of
        "ok" : _ -> pure (drop 3 line)
        "request" : payload -> do
          reply <- answer (concat payload)
          hPutStrLn (engineInput engine) ("reply " <> reply)
          readReplyWith answer engine
        _ -> ioError (userError ("the engine failed: " <> drop 6 line))
