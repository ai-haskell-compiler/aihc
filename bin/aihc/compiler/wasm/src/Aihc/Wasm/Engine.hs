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
    engineNodeCommand,
    linkImage,
    linkSharedModule,
    withEngine,
    loadSharedModule,
    callFunction,
  )
where

import Aihc.DataFiles (getDataFileName)
import Aihc.Native (WasmSysroot (..), wasmSysroot)
import Control.Exception (bracket, onException)
import Control.Monad (void)
import Data.Maybe (fromMaybe, maybeToList)
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

-- | The Node.js command. @AIHC_NODE@ replaces the default command.
engineNodeCommand :: IO FilePath
engineNodeCommand = fromMaybe "node" <$> lookupEnv "AIHC_NODE"

-- | Link an image from objects and archives. The image contains each member
-- of each archive and exports each symbol, because a shared module can
-- refer to any of them. An undefined function becomes an import, which the
-- driver denies when the instance calls it.
linkImage :: FilePath -> [FilePath] -> [FilePath] -> IO ()
linkImage output objects archives = do
  sysroot <- wasmSysroot
  runLinker
    ( ["-m", "wasm32", "--no-entry", "--export-all", "--export=__stack_pointer", "--export-table", "--growable-table", "--import-undefined", "--extra-features=atomics"]
        <> objects
        <> ["--whole-archive"]
        <> archives
        <> ["--no-whole-archive", wasmSysrootLibc sysroot]
        <> maybeToList (wasmSysrootBuiltins sysroot)
        <> ["-o", output]
    )

-- | Link position-independent objects into a shared module. Each symbol
-- that the objects do not define becomes an import, which the driver
-- supplies when it loads the module.
linkSharedModule :: FilePath -> [FilePath] -> IO ()
linkSharedModule output objects =
  runLinker (["-m", "wasm32", "-shared", "--experimental-pic", "--unresolved-symbols=import-dynamic", "--extra-features=atomics", "--no-entry"] <> objects <> ["-o", output])

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

-- | Send one command and read its reply. The result is the text after
-- @ok@.
request :: Engine -> String -> IO String
request engine command = do
  hPutStrLn (engineInput engine) command
  readReply engine

readReply :: Engine -> IO String
readReply engine = do
  end <- hIsEOF (engineOutput engine)
  if end
    then ioError (userError "the engine stopped")
    else do
      line <- hGetLine (engineOutput engine)
      case words line of
        "ok" : _ -> pure (drop 3 line)
        _ -> ioError (userError ("the engine failed: " <> drop 6 line))
