{-# LANGUAGE RankNTypes #-}

-- | Shared incremental conversion and object emission for native backends.
--
-- Assembly is pure: it mutates an object in 'ST' and needs no 'IO' of its
-- own. A caller that writes a dump alongside the object runs the same pass
-- in 'IO' instead, so conversion still happens exactly once.
module Aihc.Native.Emit
  ( ObjectBackend (..),
    compileLirObjectWith,
    compileGrinObjectWith,
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
import Aihc.Native.ObjectWriter (writeObjectFile)
import Control.Monad.ST (ST, runST, stToIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
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

-- | Assemble object bytes from a GC-GRIN program, without 'IO'.
compileGrinObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Bool -> GcGrinProgram -> ST s (Either String BL.ByteString)
{-# INLINEABLE compileGrinObjectWith #-}
compileGrinObjectWith backend =
  assembleGrinObjectWith backend id (const (pure ()))

-- | Assemble object bytes, encoding each LIR item as GC-GRIN conversion
-- completes it. @liftObject@ runs the object mutation in the monad of the
-- caller, so a caller that needs no 'IO' passes 'id' and stays in 'ST'.
-- Conversion runs once whatever the caller does with @dump@.
--
-- The linter types a body against the declarations of the whole module, so
-- @lint@ holds the converted items until the module is complete and checks
-- them before any of them is encoded. Conversion is not repeated for it.
assembleGrinObjectWith ::
  (Monad m, Ord register, Show error) =>
  ObjectBackend statement register error ->
  (forall value. ST s value -> m value) ->
  (Item -> m ()) ->
  Bool ->
  Bool ->
  GcGrinProgram ->
  m (Either String BL.ByteString)
{-# INLINEABLE assembleGrinObjectWith #-}
assembleGrinObjectWith backend liftObject dump lint checkBounds gcProgram = do
  object <- st newObject
  state <- st (newSTRef (initialObjectState native))
  -- Encoding one statement is the hottest step of the pass. A failure is
  -- recorded rather than raised because an error monad would box the result
  -- of every step of the encoder to carry it, and the object is discarded
  -- whole once anything in it has failed.
  failure <- st (newSTRef Nothing)
  let stop message = st (modifySTRef' failure (Just . fromMaybe message))
      running action = st (readSTRef failure) >>= maybe action (const (pure ()))
      objectError = stop . show . obError backend
      emit statement = either objectError pure =<< st (obStatement backend object statement)
      seal = either objectError pure =<< st (sealFunction object)
      encode signatures item = running $ do
        dump item
        current <- st (readSTRef state)
        result <- compileNativeItemTo native emit signatures item current
        case result of
          Left err -> stop (show err)
          Right next -> do
            st (writeSTRef state next)
            case item of
              ItemFunction _ -> seal
              _ -> pure ()
      lowerTo output = either (stop . show) pure =<< Lower.lowerModuleTo Lower.posixTarget64 checkBounds output gcProgram
      -- Each item is kept with the declarations conversion knew at its
      -- boundary, which are the ones its own encoding is typed against.
      collectItems = do
        collected <- st (newSTRef [])
        lowerTo (\signatures item -> st (modifySTRef' collected ((signatures, item) :)))
        reverse <$> st (readSTRef collected)
  if lint
    then do
      items <- collectItems
      case Lint.lintModule (Module (map snd items)) of
        [] -> mapM_ (uncurry encode) items
        errors -> stop (show (nbLintErrors native errors))
    else lowerTo encode
  running $ do
    current <- st (readSTRef state)
    result <- finishNativeTo native emit current
    either (stop . show) (const (mapM_ emit (nbAfterObject native))) result
  failed <- st (readSTRef failure)
  case failed of
    Just message -> pure (Left message)
    Nothing -> objectResult . (>>= obImage backend) <$> st (layoutObject object)
  where
    native = obNative backend
    st = liftObject
    objectResult = either (Left . show . obError backend) Right

-- | Write an object assembled from a LIR module.
writeLirObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Module -> FilePath -> IO ()
{-# INLINEABLE writeLirObjectWith #-}
writeLirObjectWith backend lint lirModule path =
  checked (first show (compileLirObjectWith backend lint lirModule)) >>= writeObjectFile path

-- | Write an object assembled from a GC-GRIN program, and the LIR it was
-- assembled from when a dump is wanted.
writeGrinObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Bool -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO ()
{-# INLINEABLE writeGrinObjectWith #-}
writeGrinObjectWith backend lint checkBounds dumpPath gcProgram path = do
  result <- case dumpPath of
    Nothing -> stToIO (compileGrinObjectWith backend lint checkBounds gcProgram)
    Just dump ->
      withFile dump WriteMode $ \handle ->
        assembleGrinObjectWith backend stToIO (TIO.hPutStrLn handle . renderModule . Module . pure) lint checkBounds gcProgram
  checked result >>= writeObjectFile path

checked :: Either String value -> IO value
checked = either (ioError . userError) pure
