{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Control.Exception.Base
  ( Exception (..),
    SomeException (..),
    catch,
    catchJust,
    evaluate,
    handle,
    handleJust,
    throw,
    throwIO,
    try,
    tryJust,
  )
where

import GHC.Exception (Exception (..), SomeException (..), throw)
import GHC.IO (IO (..))
import GHC.Prim (catch#, raise#)
import Prelude (Either (..), Maybe (..), pure, (.), (>>=))

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
        case value of
          evaluated -> (# state, evaluated #)
    )
