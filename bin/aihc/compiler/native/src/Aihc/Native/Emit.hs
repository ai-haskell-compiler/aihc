{-# LANGUAGE RankNTypes #-}

-- | Shared incremental conversion and object emission for native backends.
--
-- Assembly is pure. It mutates an object in 'ST' and yields the bytes and
-- the LIR they came from; writing either to a file is the business of the
-- caller alone.
module Aihc.Native.Emit
  ( ObjectBackend (..),
    GrinObject (..),
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
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.IO qualified as TLIO

-- | The instruction encoder and object format are specific to each target.
data ObjectBackend statement register error = ObjectBackend
  { obNative :: !(NativeBackend statement register error),
    obStatement :: !(forall s. Object s -> statement -> ST s (Either ObjectError ())),
    obImage :: !(Image -> Either ObjectError BL.ByteString),
    obError :: !(ObjectError -> error)
  }

-- | What one GC-GRIN program assembles to.
data GrinObject = GrinObject
  { -- | The object bytes, or the first failure of any stage.
    grinObjectBytes :: !(Either String BL.ByteString),
    -- | The LIR the object was assembled from, when it was asked for, and
    -- empty otherwise. It holds whatever conversion reached, so a failed
    -- assembly still renders what it saw.
    grinObjectLir :: TL.Text
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

-- | Assemble object bytes, encoding each LIR item as GC-GRIN conversion
-- completes it so that no converted body outlives its own encoding.
-- Conversion runs once: @keepLir@ renders each item on its way through,
-- and the linter is served from the same pass.
--
-- The linter types a body against the declarations of the whole module, so
-- it holds the converted items until the module is complete and checks them
-- before any of them is encoded.
compileGrinObjectWith ::
  (Ord register, Show error) =>
  ObjectBackend statement register error ->
  Bool ->
  Bool ->
  Bool ->
  GcGrinProgram ->
  ST s GrinObject
{-# INLINEABLE compileGrinObjectWith #-}
compileGrinObjectWith backend lint checkBounds keepLir gcProgram = do
  object <- newObject
  state <- newSTRef (initialObjectState native)
  -- Encoding one statement is the hottest step of the pass. A failure is
  -- recorded rather than raised because an error monad would box the result
  -- of every step of the encoder to carry it, and the object is discarded
  -- whole once anything in it has failed.
  failure <- newSTRef Nothing
  rendered <- newSTRef []
  let stop message = modifySTRef' failure (Just . fromMaybe message)
      running action = readSTRef failure >>= maybe action (const (pure ()))
      objectError = stop . show . obError backend
      emit statement = either objectError pure =<< obStatement backend object statement
      seal = either objectError pure =<< sealFunction object
      render item = when keepLir (modifySTRef' rendered (renderModule (Module [item]) :))
      encodeItem signatures item = running $ do
        current <- readSTRef state
        result <- compileNativeItemTo native emit signatures item current
        case result of
          Left err -> stop (show err)
          Right next -> do
            writeSTRef state next
            case item of
              ItemFunction _ -> seal
              _ -> pure ()
      encode signatures item = running (render item) >> encodeItem signatures item
      lowerTo output = either (stop . show) pure =<< Lower.lowerModuleTo Lower.posixTarget64 checkBounds output gcProgram
      -- Each item is kept with the declarations conversion knew at its
      -- boundary, which are the ones its own encoding is typed against.
      collectItems = do
        collected <- newSTRef []
        lowerTo (\signatures item -> modifySTRef' collected ((signatures, item) :))
        reverse <$> readSTRef collected
  if lint
    then do
      items <- collectItems
      -- The dump is what conversion produced, whatever the linter makes of it.
      mapM_ (render . snd) items
      case Lint.lintModule (Module (map snd items)) of
        [] -> mapM_ (uncurry encodeItem) items
        errors -> stop (show (nbLintErrors native errors))
    else lowerTo encode
  running $ do
    current <- readSTRef state
    result <- finishNativeTo native emit current
    either (stop . show) (const (mapM_ emit (nbAfterObject native))) result
  lir <- lirText <$> readSTRef rendered
  failed <- readSTRef failure
  case failed of
    Just message -> pure (GrinObject (Left message) lir)
    Nothing -> do
      image <- layoutObject object
      pure (GrinObject (objectResult (image >>= obImage backend)) lir)
  where
    native = obNative backend
    objectResult = either (Left . show . obError backend) Right
    lirText chunks = TL.fromChunks (foldl' (\text chunk -> chunk : "\n" : text) [] chunks)

-- | Write an object assembled from a LIR module.
writeLirObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Module -> FilePath -> IO ()
{-# INLINEABLE writeLirObjectWith #-}
writeLirObjectWith backend lint lirModule path =
  checked (first show (compileLirObjectWith backend lint lirModule)) >>= writeObjectFile path

-- | Write an object assembled from a GC-GRIN program, and the LIR it was
-- assembled from when a dump is wanted. The dump is written even when
-- assembly fails, which is when it is most wanted.
writeGrinObjectWith :: (Ord register, Show error) => ObjectBackend statement register error -> Bool -> Bool -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO ()
{-# INLINEABLE writeGrinObjectWith #-}
writeGrinObjectWith backend lint checkBounds dumpPath gcProgram path = do
  for_ dumpPath (`TLIO.writeFile` grinObjectLir assembled)
  checked (grinObjectBytes assembled) >>= writeObjectFile path
  where
    assembled = runST (compileGrinObjectWith backend lint checkBounds (isJust dumpPath) gcProgram)

checked :: Either String value -> IO value
checked = either (ioError . userError) pure
