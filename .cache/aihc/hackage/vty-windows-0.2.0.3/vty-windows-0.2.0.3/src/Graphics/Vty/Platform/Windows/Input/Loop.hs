{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK hide #-}

-- | The input layer forks a thread to read input data via the
--   Windows console API. Key presses, mouse events, and window
--   resize events are all obtained by calling ReadConsoleInputW.
module Graphics.Vty.Platform.Windows.Input.Loop
  ( initInput
  )
where

import Graphics.Vty.Input

import Graphics.Vty.Config (VtyUserConfig(..))

import Graphics.Vty.Platform.Windows.Input.Classify ( classify )
import Graphics.Vty.Platform.Windows.Input.Classify.Types
import Graphics.Vty.Platform.Windows.WindowsConsoleInput ( WinConsoleInputEvent )
import Graphics.Vty.Platform.Windows.WindowsInterfaces (readBuf)

import Control.Applicative ( Alternative(many) )
import Control.Concurrent
    ( ThreadId, forkOS, killThread, newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM ( atomically, writeTChan, newTChan )
import Control.Exception (mask, try, catch, SomeException)
import Lens.Micro ( over, ASetter, ASetter' )
import Lens.Micro.Mtl ( (.=), use )
import Control.Monad (unless, mzero, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.State.Class (MonadState, modify)
import Control.Monad.Trans.Reader (ReaderT(..), asks, ask)

import Lens.Micro.TH ( makeLenses )

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)

import Foreign (allocaArray)
import Foreign.Ptr (Ptr, castPtr)

import System.Environment (getEnv)
import System.IO ( Handle )

data InputBuffer = InputBuffer
    { _ptr :: Ptr Word8
    , _inputRecordPtr :: Ptr WinConsoleInputEvent
    , _consoleEventBufferSize :: Int
    }

makeLenses ''InputBuffer

data InputState = InputState
    { _unprocessedBytes :: ByteString
    , _classifierState :: ClassifierState
    , _inputHandle :: Handle
    , _originalInput :: Input
    , _inputBuffer :: InputBuffer
    , _classifier :: ClassifierState -> ByteString -> KClass
    }

makeLenses ''InputState

type InputM a = StateT InputState (ReaderT Input IO) a

logMsg :: String -> InputM ()
logMsg msg = do
    i <- use originalInput
    liftIO $ inputLogMsg i msg

-- this must be run on an OS thread dedicated to this input handling.
-- otherwise the terminal timing read behavior will block the execution
-- of the lightweight threads.
loopInputProcessor :: InputM ()
loopInputProcessor = do
    readFromDevice >>= addBytesToProcess
    validEvents <- many parseEvent
    forM_ validEvents emit
    dropInvalid
    loopInputProcessor

addBytesToProcess :: ByteString -> InputM ()
addBytesToProcess block = unprocessedBytes <>= block

emit :: Event -> InputM ()
emit event = do
    logMsg $ "parsed event: " ++ show event
    lift (asks eventChannel) >>= liftIO . atomically . flip writeTChan (InputEvent event)

-- Precondition: Under the threaded runtime. Only current use is from a
-- forkOS thread. That case satisfies precondition.
readFromDevice :: InputM ByteString
readFromDevice = do
    handle <- use inputHandle

    bufferPtr <- use $ inputBuffer.ptr
    winRecordPtr <- use $ inputBuffer.inputRecordPtr
    maxInputRecords <- use $ inputBuffer.consoleEventBufferSize

    input <- lift ask
    stringRep <- liftIO $ do
        bytesRead <- readBuf (eventChannel input) winRecordPtr handle bufferPtr maxInputRecords
        if bytesRead > 0
        then BS.packCStringLen (castPtr bufferPtr, fromIntegral bytesRead)
        else return BS.empty
    unless (BS.null stringRep) $ logMsg $ "input bytes: " ++ show (BS8.unpack stringRep)
    return stringRep

parseEvent :: InputM Event
parseEvent = do
    c <- use classifier
    s <- use classifierState
    b <- use unprocessedBytes
    case c s b of
        Valid e remaining -> do
            logMsg $ "valid parse: " ++ show e
            logMsg $ "remaining: " ++ show remaining
            classifierState .= ClassifierStart
            unprocessedBytes .= remaining
            return e
        _ -> mzero

dropInvalid :: InputM ()
dropInvalid = do
    c <- use classifier
    s <- use classifierState
    b <- use unprocessedBytes
    case c s b of
        Chunk -> do
            classifierState .=
                case s of
                  ClassifierStart -> ClassifierInChunk b []
                  ClassifierInChunk p bs -> ClassifierInChunk p (b:bs)
            unprocessedBytes .= BS8.empty
        Invalid -> do
            logMsg "dropping input bytes"
            classifierState .= ClassifierStart
            unprocessedBytes .= BS8.empty
        _ -> return ()

runInputProcessorLoop :: ClassifyMap -> Input -> Handle -> IO ()
runInputProcessorLoop classifyTable input handle = do
    let bufferSize = 1024
    -- A key event could require 4 bytes of UTF-8.
    let maxKeyEvents = bufferSize `div` 4
    allocaArray maxKeyEvents $ \(inputRecordBuf :: Ptr WinConsoleInputEvent) -> do
        allocaArray bufferSize $ \(bufferPtr :: Ptr Word8) -> do
            let s0 = InputState BS8.empty ClassifierStart
                        handle
                        input
                        (InputBuffer bufferPtr inputRecordBuf maxKeyEvents)
                        (classify classifyTable)
            runReaderT (evalStateT loopInputProcessor s0) input

initInput :: VtyUserConfig -> Handle -> ClassifyMap -> IO Input
initInput userConfig handle classifyTable = do
    stopSync <- newEmptyMVar
    mDefaultLog <- catch
          (do debugLog <- getEnv "VTY_DEBUG_LOG"
              return $ Just debugLog)
          (\(_ :: IOError) -> return Nothing)
    input <- Input <$> atomically newTChan
                   <*> pure (return ())
                   <*> pure (return ())
                   <*> maybe (return $ append mDefaultLog)
                             (return . appendFile)
                             (configDebugLog userConfig)
    inputThread <- forkOSFinally (runInputProcessorLoop classifyTable input handle)
                                 (\_ -> putMVar stopSync ())
    let killAndWait = do
          killThread inputThread
          takeMVar stopSync
    return $ input { shutdownInput = killAndWait }
    where
        append mDebugLog msg =
          case mDebugLog of
            Just debugLog -> appendFile debugLog $ msg ++ "\n"
            Nothing       -> return ()

forkOSFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkOSFinally action and_then =
  mask $ \restore -> forkOS $ try (restore action) >>= and_then

(<>=) :: (MonadState s m, Monoid a) => ASetter' s a -> a -> m ()
l <>= a = modify (l <>~ a)

(<>~) :: Monoid a => ASetter s t a a -> a -> s -> t
l <>~ n = over l (`mappend` n)
