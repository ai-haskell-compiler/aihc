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
-- An engine sends commands as lines of text and reads the replies as lines
-- of text. 'lineEngine' makes an engine from two functions that send and
-- read the lines. 'withEngine' makes an engine that is a Node.js process
-- that runs @driver.mjs@. The instance has
-- no access to the host: the driver denies each import of the image, apart
-- from a clock that gives a fixed time. As an option, the instance can read
-- the files of one directory. It cannot write to that directory. As a
-- second option, the instance can write to its standard output and to its
-- standard error, and the engine gives the bytes to a function of the
-- caller.
--
-- A native engine is a native program that speaks the same protocol. It
-- runs the closures of a list, by index, and it has no sandbox. See
-- 'withNativeEngine'.
module Aihc.Wasm.Engine
  ( Engine,
    EngineStream (..),
    EngineValue (..),
    engineDriverPath,
    lineEngine,
    engineGuestPath,
    engineNodeCommand,
    linkImage,
    linkSharedModule,
    withEngine,
    withOutputEngine,
    withNativeEngine,
    loadSharedModule,
    callFunction,
    startGuest,
    guestIsRunning,
    runClosure,
    tryRunClosure,
    runIndexedClosure,
  )
where

import Aihc.DataFiles (getDataFileName)
import Aihc.Native (readWasmClangProcessWithExitCode, wasmClangCommand, wasmSysroot, wasmSysrootArchives)
import Aihc.Wasm.Engine.Protocol
import Control.Exception (bracket, onException)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe, isJust)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO (BufferMode (..), Handle, hClose, hGetLine, hIsEOF, hPutStrLn, hSetBuffering)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, readProcessWithExitCode, waitForProcess)

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
        <> wasmSysrootArchives sysroot
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
-- the engine. If the first argument has a directory, the instance can read
-- the files in that directory, and a relative path starts at it.
withEngine :: Maybe FilePath -> FilePath -> (Engine -> IO value) -> IO value
withEngine = withEngineStreams Nothing

-- | 'withEngine' for an instance that can write to its standard output and
-- to its standard error. The function gets the bytes of each write, in the
-- order of the writes, before the command that made them completes.
withOutputEngine :: (EngineStream -> ByteString -> IO ()) -> Maybe FilePath -> FilePath -> (Engine -> IO value) -> IO value
withOutputEngine = withEngineStreams . Just

withEngineStreams :: Maybe (EngineStream -> ByteString -> IO ()) -> Maybe FilePath -> FilePath -> (Engine -> IO value) -> IO value
withEngineStreams engineStreams readable image action = do
  node <- engineNodeCommand
  driver <- engineDriverPath
  withEngineProcess engineStreams (proc node ([driver, image] <> maybe [] pure readable <> ["--output" | isJust engineStreams])) action

-- | Start a native engine, run the action, and stop the engine. The
-- engine is a native program that gives the protocol of the driver to a
-- list of closures, such as a program that calls
-- @Aihc.TH.NativeEngine.serveSplices@. Run its closures with
-- 'runIndexedClosure'. If the first argument has a directory, the program
-- runs in that directory.
--
-- The program has no sandbox: it has all the access of the user. The
-- standard error of the program is the standard error of the caller.
withNativeEngine :: Maybe FilePath -> FilePath -> (Engine -> IO value) -> IO value
withNativeEngine directory program = withEngineProcess Nothing (proc program []) {cwd = directory}

-- | Start a process that speaks the protocol of the driver, run the
-- action, and stop the process. The process writes one reply when it is
-- ready.
withEngineProcess :: Maybe (EngineStream -> ByteString -> IO ()) -> CreateProcess -> (Engine -> IO value) -> IO value
withEngineProcess engineStreams command action = bracket start stop (\(_, engine, _) -> action engine)
  where
    start = do
      (input, output, _, process) <-
        createProcess command {std_in = CreatePipe, std_out = CreatePipe}
      case (input, output) of
        (Just input', Just output') -> do
          hSetBuffering input' LineBuffering
          let engine = lineEngine (hPutStrLn input') (readLine output') engineStreams
          -- The driver writes one reply after it makes the instance.
          void (readReply engine) `onException` (hClose input' >> waitForProcess process)
          pure ((input', output'), engine, process)
        _ -> ioError (userError "the engine process has no pipes")
    stop :: ((Handle, Handle), Engine, ProcessHandle) -> IO ()
    stop ((input', output'), _, process) = do
      hClose input'
      void (waitForProcess process)
      hClose output'
    readLine handle = do
      end <- hIsEOF handle
      if end then pure Nothing else Just <$> hGetLine handle
