{-# LANGUAGE RankNTypes #-}

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
import Aihc.Native.Object (Image, Object, ObjectError, layoutObject, newObject, sealFunction)
import Aihc.Native.ObjectWriter (withObjectWriter)
import Aihc.Native.ObjectWriter qualified as ObjectWriter
import Control.Monad (when)
import Control.Monad.ST (RealWorld, ST, runST, stToIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as TIO
import System.IO (IOMode (WriteMode), withFile)

-- | The instruction encoder and object format are specific to each target.
data ObjectBackend statement register error = ObjectBackend
  { obNative :: !(NativeBackend statement register error),
    obStatement :: !(forall s. Object s -> statement -> ST s (Either ObjectError ())),
    obImage :: !(Image -> Either ObjectError BL.ByteString),
    obError :: !(ObjectError -> error)
  }

-- | Assemble a module into object bytes.
compileLirObjectWith :: (Ord register) => ObjectBackend statement register error -> Bool -> Module -> Either error BL.ByteString
{-# INLINEABLE compileLirObjectWith #-}
compileLirObjectWith backend lint lirModule = runST $ do
  object <- newObject
  result <- runExceptT (compileNativeTo lint (obNative backend) (ExceptT . obStatement backend object) (ExceptT (sealFunction object)) lirModule)
  case result of
    Left err -> pure (Left (obError backend err))
    Right (Left err) -> pure (Left err)
    Right (Right ()) -> objectResult . (>>= obImage backend) <$> layoutObject object
  where
    objectResult = either (Left . obError backend) Right

-- | Write an object, assembling each statement as it is produced.
writeLirObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Module -> FilePath -> IO ()
{-# INLINEABLE writeLirObjectWith #-}
writeLirObjectWith backend lint lirModule path =
  withObjectWriter path (obImage backend) $ \object ->
    compileNativeTo lint (obNative backend) (writeStatement backend object) (seal object) lirModule >>= checked

-- | Consume each LIR item as GC-GRIN conversion completes it.
writeGrinObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Bool -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO ()
{-# INLINEABLE writeGrinObjectWith #-}
writeGrinObjectWith backend lint checkBounds dumpPath gcProgram path = do
  -- The optional declaration pass repeats conversion without retention of bodies.
  symbols <- if lint then declarations else pure Map.empty
  withDump $ \dump ->
    withObjectWriter path (obImage backend) $ \object -> do
      state <- newIORef (initialObjectState native)
      let output signatures item = do
            when lint (checkLint (Lint.lintItem symbols item))
            dump item
            current <- readIORef state
            next <- compileNativeItemTo native (writeStatement backend object) signatures item current >>= checked
            writeIORef state next
            case item of
              ItemFunction _ -> seal object
              _ -> pure ()
      Lower.lowerModuleTo Lower.posixTarget64 checkBounds output gcProgram >>= checked
      current <- readIORef state
      finishNativeTo native (writeStatement backend object) current >>= checked
      mapM_ (writeStatement backend object) (nbAfterObject native)
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

writeStatement :: ObjectBackend statement register error -> Object RealWorld -> statement -> IO ()
{-# INLINEABLE writeStatement #-}
writeStatement backend object statement = stToIO (obStatement backend object statement) >>= ObjectWriter.checked

seal :: Object RealWorld -> IO ()
seal object = stToIO (sealFunction object) >>= ObjectWriter.checked

checked :: (Show error) => Either error value -> IO value
checked = either (ioError . userError . show) pure
