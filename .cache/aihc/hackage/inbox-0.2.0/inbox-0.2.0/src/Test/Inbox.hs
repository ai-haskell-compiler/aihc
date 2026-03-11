{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Inbox

Facilitates testing of asynchronouse code.

== Example
Assume you have a server that accepts incoming messages and
produces responses asynchronously. In order to test it, open a
connection to it, and process any incomming message from the server by
adding it to the Inbox. Now that we know the messages are all arriving
in the inbox, the test can send messages to the server and use
`takeInbox` to wait for expected responses.
-}
module Test.Inbox (

  Inbox
  , newInbox
  , putInbox
  , takeInbox
  , takeInbox'

  , Filter(Filter)
  , equalTo
  , predicate

  , expectEmpty
  , expectEmpty'
)
 where

import qualified Control.Category as Cat
import Control.Arrow (Arrow(..), first)
import Data.IORef (newIORef, readIORef, atomicModifyIORef, IORef)
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Text as T
import Data.ErrorOr
import Control.Concurrent
import Control.Concurrent.Async
import Data.Maybe (isJust)
import Control.Monad (unless)
import Data.Foldable (sequenceA_)
import Control.Monad (forM_)
import Control.Exception
import Data.Time

#if __GLASGOW_HASKELL__ < 880
import Prelude hiding (fail) -- out with the old, Monad.fail
import Control.Monad.Fail (fail) -- in with the new, MonadFail.fail
import Data.Semigroup
#endif


-- | An entity holding a number of messages of type `a`.
data Inbox a =
  Inbox (IORef (MessagesAndObservers a))

data MessagesAndObservers a = MessagesAndObservers {
  messages :: ![a]
  , observers :: !Observers
  }

type Observer = MVar ()
type Observers = [Observer]

-- | Create an empty Inbox.
newInbox :: IO (Inbox a)
newInbox = Inbox `fmap` newIORef (MessagesAndObservers [] [])

-- | Add a message to the Inbox.
putInbox :: forall m a . MonadIO m => Inbox a -> a -> m ()
putInbox (Inbox r) newmsg = do
  liftIO $ mask_ $ do
    observers <- atomicModifyIORef r f
    mapM_ (flip putMVar ()) observers
  where
    f :: MessagesAndObservers a -> (MessagesAndObservers a, [MVar ()])
    f MessagesAndObservers {..} = (MessagesAndObservers (newmsg:messages) [], observers)

-- | 'takeInbox'' with a timeout of 3s
takeInbox :: (MonadIO m, Show a) => Inbox a -> Filter a b -> m b
takeInbox = takeInbox' 3

-- | Take a single message out of the inbox, waiting for it up to the specified timeout in seconds.
--   It respects the order the messages were inserted into the inbox.
takeInbox' ::
  forall m a b.
  (MonadIO m, Show a) =>
#if __GLASGOW_HASKELL__ >= 880
  -- | timeout in seconds
#endif
  Float ->
  Inbox a ->
  Filter a b ->
  m b
takeInbox' sec t@(Inbox r) filter@(Filter text f) = do
  observer <- liftIO $ newEmptyMVar
  match <- liftIO $ mask_ $ do -- mask, so we are not interrupted before notifying the observers
    match <- atomicModifyIORef r (checkInbox observer)
    -- I got a bit ahead of myself. In the current design there are no
    -- observers waiting for decrease of messages in the Inbox, but I
    -- am considering making the `assertEmpty` observe the Inbox
    forM_ (fst <$> match) $ mapM_ (flip putMVar ()) . reverse
    return (snd <$> match)
  case match of
    Just msg -> return msg
    Nothing -> do
      time0 <- liftIO getCurrentTime
      res <- liftIO $ race (threadDelay (round $ sec * 10^6)) (readMVar observer)
      case res of
        Right () -> do
          -- something changed in the Inbox, let's retest the filter
          now <- liftIO getCurrentTime
          let elapsed = diffUTCTime time0 now
          takeInbox' (sec - realToFrac elapsed) t filter
        Left () -> do
          xs <- liftIO $ messages <$> readIORef r
          error (T.unpack $ "Timed out waiting for `" <> text <> "`. Contents: " <> (T.pack $ show xs))
 where
    checkInbox :: Observer -> MessagesAndObservers a -> (MessagesAndObservers a, Maybe (Observers, b))
    checkInbox observer MessagesAndObservers{..} =
      case first reverse . pick f . reverse $ messages of
        (_, Nothing) -> (MessagesAndObservers messages (observer:observers), Nothing)
        (newMsgs, Just matched) -> (MessagesAndObservers newMsgs [], Just (observers, matched))

    pick _ [] = ([], Nothing)
    pick f (x:xs) =
        case f x of
          Nothing ->
              let (rest, res) = pick f xs in
              (x:rest, res)
          Just found -> (xs, Just found)

-- | It is a selector\/matcher/extractor with a name. It specifies
-- what message to pick from the `Inbox` and how to transform it. The
-- name provides for a better error messages. See `predicate` for a
-- 'Filter' 'a' 'a' that selects an element and does not apply any
-- transformation.
data Filter a b = Filter T.Text (a -> Maybe b)

instance Cat.Category Filter where
  id = Filter "id" Just
  (.) (Filter n1 f1) (Filter n2 f2) = Filter (n2 <> ">>>" <> n1) (\x -> f2 x >>= f1)

instance Arrow Filter where
   arr f = Filter "arr" (Just . f)
   first (Filter name f) = Filter (name <> "*") (\(x,y) -> fmap (,y) (f x))

-- | A filter that matches messages equal to the given one.
equalTo :: (Eq a, Show a) => a -> Filter a ()
equalTo a = Filter (T.pack $ show a) (\x -> if a == x then Just () else Nothing)

-- | A filter that matches messages based on a predicate.
predicate :: T.Text -- ^ name
    -> (a -> Bool) -- ^ the predicate
    -> Filter a a
predicate name p = Filter name (\x -> if p x then Just x else Nothing)

-- | Validate that the inbox has no messages inside at the moment.
expectEmpty :: Show a => Inbox a -> IO (ErrorOr ())
expectEmpty (Inbox r) = do
  xs <- messages <$> readIORef r
  case xs of
    [] -> pure (pure ())
    _ -> return . tag "Unconsumed messages" . sequenceA_ . map (fail . show) $ xs

-- | Validate that the filter does not match anything in the Inbox.
expectEmpty' :: (Show a, MonadIO m) => Inbox a -> Filter a b -> m ()
expectEmpty' (Inbox r) (Filter name p) = do
  elems <- liftIO (filter (isJust.p) . messages <$> readIORef r)
  unless (null elems) $ do
    liftIO
      . toE
      . tag ("There are msgs matching " <> name)
      . sequenceA_
      . map (fail . show) $ elems
