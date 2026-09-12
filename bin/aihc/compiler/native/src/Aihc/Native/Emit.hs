-- | Shared incremental conversion and object emission for native backends.
module Aihc.Native.Emit
  ( ObjectBackend (..),
    compileLirObjectWith,
    writeLirObjectWith,
    writeGrinObjectWith,
  )
where

import Aihc.Grin.Gc (GcGrinProgram)
import Aihc.Lir.Lint qualified as Lint
import Aihc.Lir.Lower qualified as Lower
import Aihc.Lir.Pretty (renderModule)
import Aihc.Lir.Syntax
import Aihc.Native.Lir
import Aihc.Native.Object (Draft, Image, ObjectError, emptyDraft, layoutDraft, sealFunction)
import Aihc.Native.Object qualified as Object
import Aihc.Native.ObjectWriter (ObjectWriter, alignObject, modifyObject, withObjectWriter)
import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (StateT (..), runStateT)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import System.IO (IOMode (WriteMode), withFile)

-- | The instruction encoder and object format are specific to each target.
data ObjectBackend statement register error = ObjectBackend
  { obNative :: !(NativeBackend statement register error),
    obStatement :: !(Either ObjectError Draft -> statement -> Either ObjectError Draft),
    obImage :: !(Image -> Either ObjectError BL.ByteString),
    obError :: !(ObjectError -> error),
    obAlign :: !(statement -> Maybe Int),
    obBytes :: !(statement -> Maybe BS.ByteString),
    obFill :: !(Draft -> BS.ByteString)
  }

-- | Collect object bytes without a complete list of native instructions.
compileLirObjectWith :: (Ord register) => ObjectBackend statement register error -> Bool -> Module -> Either error BL.ByteString
{-# INLINEABLE compileLirObjectWith #-}
compileLirObjectWith backend lint lirModule = do
  (result, draft) <- objectResult (runStateT (compileNativeTo lint (obNative backend) output endFunction lirModule) emptyDraft)
  result
  objectResult (layoutDraft draft >>= obImage backend)
  where
    output statement = StateT (\draft -> ((),) <$> obStatement backend (Right draft) statement)
    endFunction = StateT (fmap ((),) . sealFunction)
    objectResult = either (Left . obError backend) Right

-- | Write an object with bounded section buffers.
writeLirObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Module -> FilePath -> IO ()
{-# INLINEABLE writeLirObjectWith #-}
writeLirObjectWith backend lint lirModule path =
  withObjectWriter path (obImage backend) $ \writer ->
    compileNativeTo lint (obNative backend) (writeStatement backend writer) (modifyObject writer sealFunction) lirModule >>= checked

-- | Consume each LIR item as GC-GRIN conversion completes it.
writeGrinObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Bool -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO ()
{-# INLINEABLE writeGrinObjectWith #-}
writeGrinObjectWith backend lint checkBounds dumpPath gcProgram path = do
  -- The optional declaration pass repeats conversion without retention of bodies.
  symbols <- if lint then declarations else pure Map.empty
  withDump $ \dump ->
    withObjectWriter path (obImage backend) $ \writer -> do
      state <- newIORef (initialObjectState native)
      let output signatures item = do
            when lint (checkLint (Lint.lintItem symbols item))
            dump item
            current <- readIORef state
            next <- compileNativeItemTo native (writeStatement backend writer) signatures item current >>= checked
            writeIORef state next
            case item of
              ItemFunction _ -> modifyObject writer sealFunction
              _ -> pure ()
      Lower.lowerModuleTo Lower.posixTarget64 checkBounds output gcProgram >>= checked
      current <- readIORef state
      finishNativeTo native (writeStatement backend writer) current >>= checked
      mapM_ (writeStatement backend writer) (nbAfterObject native)
  where
    native = obNative backend
    declarations = do
      items <- newIORef []
      let declaration item = case item of
            ItemFunction function -> ItemExternFunction (ExternFunction (functionName function) (functionSignature function))
            ItemData value -> ItemData value {dataFields = []}
            _ -> item
      let retain _ item =
            let value = declaration item
             in value `seq` modifyIORef' items (value :)
      Lower.lowerModuleTo Lower.posixTarget64 checkBounds retain gcProgram >>= checked
      (symbols, errors) <- Lint.moduleSymbols . Module . reverse <$> readIORef items
      checkLint errors
      pure symbols
    checkLint [] = pure ()
    checkLint errors = checked (Left (nbLintErrors native errors))
    withDump action = case dumpPath of
      Nothing -> action (const (pure ()))
      Just dump -> withFile dump WriteMode $ \handle -> action (TIO.hPutStrLn handle . renderModule . Module . pure)

writeStatement :: ObjectBackend statement register error -> ObjectWriter -> statement -> IO ()
{-# INLINEABLE writeStatement #-}
writeStatement backend writer statement = case obAlign backend statement of
  Just power -> alignObject writer power (obFill backend)
  Nothing -> case obBytes backend statement of
    Just bytes -> chunks bytes
    Nothing -> modifyObject writer (\draft -> obStatement backend (Right draft) statement)
  where
    chunks bytes = unless (BS.null bytes) $ do
      let (prefix, rest) = BS.splitAt 65536 bytes
      modifyObject writer (Object.addItem (Object.Bytes prefix))
      chunks rest

checked :: (Show error) => Either error value -> IO value
checked = either (ioError . userError . show) pure
