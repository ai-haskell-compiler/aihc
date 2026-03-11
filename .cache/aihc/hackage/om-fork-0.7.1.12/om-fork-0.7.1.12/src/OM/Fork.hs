{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | Description: Thread utilities. -}
module OM.Fork (
  -- * Actor Communication.
  Actor(..),
  Responder,
  Responded,
  respond,
  call,
  cast,

  -- * Forking Background Processes.
  logUnexpectedTermination,
  ProcessName(..),
  Race,
  runRace,
  race,
  wait,
) where


import Control.Concurrent (Chan, myThreadId, newEmptyMVar, putMVar,
  takeMVar, writeChan)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch, SomeException,
  try)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Logger.CallStack (MonadLogger, logInfo, logWarn)
import Data.Aeson (ToJSON(toJSON))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Conc (atomically)
import OM.Show (showt)
import Prelude (Either(Left, Right), Monad((>>=), return),
  Semigroup((<>)), Show(show), ($), (.), IO, Monoid)
import UnliftIO (MonadUnliftIO, throwString)
import qualified Ki.Unlifted as Ki


{- | How to respond to a asynchronous message. -}
newtype Responder a = Responder {
    unResponder :: a -> IO ()
  }
instance ToJSON (Responder a) where
  toJSON _ = toJSON ("<Responder>" :: Text)
instance Show (Responder a) where
  show _ = "Responder"


{- | The class of types that can act as the handle for an asynchronous actor. -}
class Actor a where
  {- | The type of messages associated with the actor. -}
  type Msg a
  {- | The channel through which messages can be sent to the actor. -}
  actorChan :: a -> Msg a -> IO ()
instance Actor (Chan m) where
  type Msg (Chan m) = m
  actorChan = writeChan


{- | Respond to an asynchronous message. -}
respond :: (MonadIO m) => Responder a -> a -> m Responded
respond responder val = do
  liftIO (unResponder responder val)
  return Responded


{- | Send a message to an actor, and wait for a response. -}
call
  :: ( Actor actor
     , MonadIO m
     )
  => actor {- ^ The actor to which we are sending a call request. -}
  -> (Responder a -> Msg actor)
     {- ^
       Given a way for the actor to respond to the message, construct
       a message that should be sent to the actor.

       Typically, your 'Msg' type will look something like this:

       > data MyMsg
       >   = MsgWithResponse SomeData (Responder ResponseType)
       >     -- In this example, this type of message requires a
       >     -- response. We package the responder up as part of the
       >     -- message itself. Idiomatically it is best to put the
       >     -- responder as the last argument so that it is easy to pass
       >     -- 'MsgWithResponse someData' to 'call'.
       >   | MsgWithoutResponse SomeData
       >     -- In this example, this type of message requires no response. It
       >     -- is a "fire and forget" message.

       you will call 'call' like this:

       > do
       >   response <- call actor (MsgWithResponse someData)
       >   -- response :: ResponseType

     -}
  -> m a
call actor mkMessage = liftIO $ do
  mVar <- newEmptyMVar
  actorChan actor (mkMessage (Responder (putMVar mVar)))
  takeMVar mVar


{- | Send a message to an actor, but do not wait for a response. -}
cast :: (Actor actor, MonadIO m) => actor -> Msg actor -> m ()
cast actor = liftIO . actorChan actor


{- |
  Proof that 'respond' was called. Clients can use this type in their
  type signatures when they require that 'respond' be called at least
  once, because calling 'respond' is the only way to generate values of
  this type.
-}
data Responded = Responded


{- | Log (at WARN) when the action terminates for any reason. -}
logUnexpectedTermination :: (MonadLogger m, MonadCatch m)
  => ProcessName
  -> m a
  -> m a
logUnexpectedTermination (ProcessName name) action =
  try action >>= \case
    Left err -> do
      logWarn
        $ "Thread " <> name <> " finished with an error: " <> showt err
      throwM (err :: SomeException)
    Right v -> do
      logWarn $ "Thread " <> name <> " finished normally."
      return v


{- |
  Run a thread race.

  Within the provided action, you can call 'race' to fork new background
  threads. When the action terminates, all background threads forked
  with 'race' are also terminated. Likewise, if any one of the racing
  threads terminates, then all other racing threads are terminated _and_
  'runRace' will throw an exception.

  In any event, when 'runRace' returns, all background threads forked
  by the @action@ using 'race' will have been terminated.
-}
runRace
  :: (MonadUnliftIO m)
  => (Race => m a) {- ^ - @action@: The provided "race" action. -}
  -> m a
runRace action = do
  Ki.scoped $ \scope ->
    let ?scope = scope in action


{- |
  This constraint indicates that we are in the context of a thread race. If any
  threads in the race terminate, then all threads in the race terminate.
  Threads are "in the race" if they were forked using 'race'.
-}
type Race = (?scope :: Ki.Scope)


{- |
  Fork a new thread within the context of a race. This thread will be
  terminated when any other racing thread terminates, or else if this
  thread terminates first it will cause all other racing threads to
  be terminated.

  Generally, we normally expect that the thread is a "background thread"
  and will never terminate under "normal" conditions.
-}
race
  :: ( MonadCatch m
     , MonadLogger m
     , MonadUnliftIO m
     , Race
     )
  => ProcessName
  -> m a
  -> m ()
race name action = do
  Ki.fork_ ?scope $ do
    tid <- liftIO myThreadId
    logUnexpectedTermination name $ do
      logInfo $ "Starting thread (tid, name): " <> showt (tid, name)
      void action
    throwString $ "Thread Finished (tid, name): " <> show (tid, name)


{- | The name of a process. -}
newtype ProcessName = ProcessName
  { unProcessName :: Text
  }
  deriving newtype (IsString, Semigroup, Monoid, Show)


{- | Wait for all racing threads to terminate. -}
wait :: (MonadIO m, Race) => m ()
wait = liftIO . atomically $ Ki.awaitAll ?scope


