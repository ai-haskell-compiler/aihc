{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

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
  )
where

import GHC.Exception (ArithException (..), Exception (..), SomeException (..), throw)
import GHC.IO (IO (..))
import GHC.IO.Exception (IOException)
import GHC.Prim (catch#, raise#)
import Prelude (Bool, Either (..), Maybe (..), const, pure, seq, (.), (>>), (>>=))

throwIO :: (Exception e) => e -> IO a
throwIO exception = IO (\_state -> raise# (toException exception))

catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch action handler =
  catchSomeException action (selectHandler handler)

catchSomeException :: IO a -> (SomeException -> IO a) -> IO a
catchSomeException (IO action) handler =
  IO
    ( catch#
        action
        ( \exception ->
            case handler exception of
              IO handlerAction -> handlerAction
        )
    )

selectHandler :: (Exception e) => (e -> IO a) -> SomeException -> IO a
selectHandler handler exception =
  case fromException exception of
    Just selected -> handler selected
    Nothing -> throwIO exception

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

evaluate :: a -> IO a
evaluate value =
  IO
    ( \state ->
        seq value (# state, value #)
    )

-- | Run the second action when the first action raises an exception.
-- Asynchronous exception masking is not available in this runtime.
onException :: IO a -> IO b -> IO a
onException action cleanup =
  catchSomeException action (\exception -> cleanup >> throwIO exception)

finally :: IO a -> IO b -> IO a
finally action cleanup = do
  result <- action `onException` cleanup
  _ <- cleanup
  pure result

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use = do
  resource <- acquire
  result <- use resource `onException` release resource
  _ <- release resource
  pure result

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after use = bracket before (const after) (const use)

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = do
  resource <- acquire
  use resource `onException` release resource

-- | Assertions are not checked. The value is returned unchanged.
assert :: Bool -> a -> a
assert _ value = value
