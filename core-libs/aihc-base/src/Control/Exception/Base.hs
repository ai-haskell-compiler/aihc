module Control.Exception.Base
  ( Exception (..),
    SomeException (..),
    ArithException (..),
    IOException,
    catch,
    catchJust,
    evaluate,
    handle,
    handleJust,
    throw,
    throwIO,
    try,
    tryJust,
    onException,
    finally,
    bracket,
    bracket_,
    bracketOnError,
    assert,
    mask,
    mask_,
    uninterruptibleMask,
    uninterruptibleMask_,
    MaskingState (..),
    getMaskingState,
    interruptible,
  )
where

import GHC.Exception (ArithException (..), Exception (..), SomeException (..), throw)
import GHC.IO (MaskingState (..), bracket, catch, evaluate, finally, getMaskingState, interruptible, mask, mask_, onException, throwIO, uninterruptibleMask, uninterruptibleMask_)
import GHC.IO.Exception (IOException)
import Prelude (Bool, Either (..), IO, Maybe (..), const, pure, (.), (>>=))

handle :: (Exception e) => (e -> IO a) -> IO a -> IO a
handle handler action = catch action handler

catchJust :: (Exception e) => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchJust predicate action handler =
  catch action (selectJust predicate handler)

selectJust :: (Exception e) => (e -> Maybe b) -> (b -> IO a) -> e -> IO a
selectJust predicate handler exception =
  case predicate exception of
    Just value -> handler value
    Nothing -> throwIO exception

handleJust :: (Exception e) => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
handleJust predicate handler action = catchJust predicate action handler

try :: (Exception e) => IO a -> IO (Either e a)
try action =
  catch
    (action >>= \value -> pure (Right value))
    (pure . Left)

tryJust :: (Exception e) => (e -> Maybe b) -> IO a -> IO (Either b a)
tryJust predicate action =
  catchJust
    predicate
    (action >>= \value -> pure (Right value))
    (pure . Left)

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after use = bracket before (const after) (const use)

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = do
  resource <- acquire
  use resource `onException` release resource

-- | Assertions are not checked. The value is returned unchanged.
assert :: Bool -> a -> a
assert _ value = value
