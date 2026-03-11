-- |
-- = Record and replay HTTP interactions
--
-- This module provides functionality for recording and replaying HTTP
-- interactions using a tape. A tape represents a stored log of
-- request/response pairs and can be operated in two modes:
--
-- * `AnyOrder` (the default): Requests may be replayed in any order.  In this
--   mode the tape is interpreted as a mapping from requests to responses.
-- * `Sequential`: Requests must be replayed in the order in which they were
--   recorded.  In this mode the tape is interpreted as a list of
--   request/response pairs.
--
-- The module exports the following operations to work with tapes:
--
-- * `with` - Replays previously recorded interactions and records new ones.
-- * `record` - Records new interactions, overwriting any existing ones.
-- * `play` - Replays previously recorded interactions and fails when a
--   requested interaction is not found on the tape.
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module VCR (
  -- * Types
  Tape(..)
, Mode(..)
  -- * Tape Operations
, with
, record
, play
) where

import Imports

import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Data.List
import Data.Either
import Data.String
import Data.Map.Strict (Map)
import Data.Map qualified as Map
import GHC.Stack (HasCallStack)
import Test.HUnit

import WebMock hiding (withRequestAction)
import WebMock qualified

import VCR.Serialize qualified as Serialize

-- | A `Tape` is used to record and replay HTTP interactions.
--
-- It consists of:
--
-- * A `file` path to persist the tape to.
-- * A `mode` that controls whether interactions are replayed sequentially or
--   in any order.
-- * A `redact` function that can modify HTTP requests (by default this redacts
--   the @Authorization@ header).
data Tape = Tape {
  file :: FilePath -- ^ Path to the tape file.
, mode :: Mode -- ^ Mode of operation.
, redact :: Request -> Request -- ^ Function to redact sensitive information from a request.
}

-- | Represents the mode of operation for a tape.
data Mode =
    Sequential -- ^ Requests must be replayed in the order in which they were recorded.
  | AnyOrder -- ^ Requests may be replayed in any order.
  deriving (Eq, Show)

instance IsString Tape where
  fromString file = Tape file AnyOrder redactAuthorization

-- | Execute an action with the given tape in read-write mode.
--
-- * Replay interactions that have been recorded earlier.
-- * Record new interactions that do not yet exist on the tape.
--
-- Use `with` when you want to both replay and record interactions.
with :: HasCallStack => Tape -> IO a -> IO a
with = runTape ReadWriteMode

-- | Execute an action with the given tape in write mode.
--
-- * Overwrite interactions that have been recorded earlier.
-- * Record new interactions that do not yet exist on the tape.
--
-- Use `record` when you want to update a tape with new responses, replacing
-- any existing interactions for a given request.
record :: Tape -> IO a -> IO a
record = runTape WriteMode

-- | Execute an action with the given tape in read mode.
--
-- * Replay interactions that have been recorded earlier.
-- * Fail on new interactions that do not yet exist on the tape.
--
-- Use `play` when you want to test against recorded responses while at the
-- same time also deny real HTTP requests for interactions that have not been
-- recorded.
play :: HasCallStack => Tape -> IO a -> IO a
play = runTape ReadMode

data OpenTape mode = OpenTape {
  file :: FilePath
, interactions :: InteractionsFor mode
, redact :: Request -> Request
}

type family InteractionsFor (mode :: Mode)

type instance InteractionsFor AnyOrder = MVar Interactions

data Interactions = Interactions {
  modified :: Modified
, interactions :: Map Request (WithIndex (Async Response))
, nextIndex :: Index
}

data Modified = NotModified | Modified

data WithIndex a = WithIndex Index a

newtype Index = Index Int
  deriving newtype (Eq, Show, Ord, Enum, Num)

type instance InteractionsFor Sequential = MVar InteractionSequence

data InteractionSequence = InteractionSequence {
  modified :: Modified
, replay :: [(Request, Response)]
, rec :: [(Request, Response)]
}

checkLeftover :: HasCallStack => OpenTape Sequential -> IO ()
checkLeftover tape = withMVar tape.interactions \ interactions -> case interactions.replay of
  [] -> pass
  _ -> assertFailure $ "Expected " ++ show total ++ " requests, but only received " ++ show actual ++ "!"
    where
      total = actual + length interactions.replay
      actual = length interactions.rec

data ReadWriteMode = ReadMode | WriteMode | ReadWriteMode

shouldRecord :: ReadWriteMode -> Bool
shouldRecord = \ case
  ReadMode -> False
  WriteMode -> True
  ReadWriteMode -> True

shouldReplay :: ReadWriteMode -> Bool
shouldReplay = \ case
  ReadMode -> True
  WriteMode -> False
  ReadWriteMode -> True

runTape :: HasCallStack => ReadWriteMode -> Tape -> IO a -> IO a
runTape mode tape = case tape.mode of
  AnyOrder -> runTape'AnyOrder mode tape
  Sequential -> runTape'Sequential mode tape

runTape'AnyOrder :: HasCallStack => ReadWriteMode -> Tape -> IO c -> IO c
runTape'AnyOrder mode tape action = bracket (openTape'AnyOrder tape) closeTape'AnyOrder \ t ->  do
  processTape'AnyOrder mode t action

processTape'AnyOrder :: HasCallStack => ReadWriteMode -> OpenTape AnyOrder -> IO a -> IO a
processTape'AnyOrder mode tape = withRequestAction tape.redact \ makeRequest request -> join do
  modifyMVar tape.interactions \ interactions -> do
    case guard (shouldReplay mode) >> Map.lookup request interactions.interactions of
      Just (WithIndex _ response) -> do
        return (interactions, wait response)
      Nothing | shouldRecord mode -> do
        response <- async makeRequest
        return (addInteraction request response interactions, wait response)
      Nothing -> do
        unexpectedRequest request

addInteraction :: Request -> Async Response -> Interactions -> Interactions
addInteraction request response (Interactions _ interactions nextIndex) =
  case Map.lookup request interactions of
    Nothing -> Interactions {
        modified = Modified
      , interactions = Map.insert request (WithIndex nextIndex response) interactions
      , nextIndex = succ nextIndex
      }
    Just (WithIndex n _) -> Interactions {
        modified = Modified
      , interactions = Map.insert request (WithIndex n response) interactions
      , nextIndex
      }

runTape'Sequential :: HasCallStack => ReadWriteMode -> Tape -> IO c -> IO c
runTape'Sequential mode tape action = do
  bracket (openTape'Sequential mode tape) closeTape'Sequential \ t -> do
    processTape'Sequential mode t action <* checkLeftover t

processTape'Sequential :: HasCallStack => ReadWriteMode -> OpenTape Sequential -> IO a -> IO a
processTape'Sequential mode tape = withRequestAction tape.redact \ makeRequest request -> do
  modifyMVar tape.interactions \ interactions -> do
    case interactions.replay of
      recorded@(recordedRequest, recordedResponse) : replay -> do
        request @?= recordedRequest
        return (interactions {
            replay
          , rec = recorded : interactions.rec
          }, recordedResponse)
      [] | shouldRecord mode -> do
        response <- makeRequest
        return (interactions {
            modified = Modified
          , rec = (request, response) : interactions.rec
          }, response)
      [] -> do
        unexpectedRequest request

unexpectedRequest :: HasCallStack => Request -> IO a
unexpectedRequest request = assertFailure $ "Unexpected HTTP request: " ++ show request

withRequestAction :: (Request -> Request) -> (IO Response -> Request -> IO Response) -> IO a -> IO a
withRequestAction redact requestAction = WebMock.withRequestAction
  \ makeRequest request -> requestAction makeRequest (redact request)

openTape'AnyOrder :: Tape -> IO (OpenTape AnyOrder)
openTape'AnyOrder Tape{..} = do
  interactions <- Serialize.loadTape file >>= toInteractions >>= newMVar
  return OpenTape {
    file
  , interactions
  , redact
  }
  where
    toInteractions :: [(Request, Response)] -> IO Interactions
    toInteractions recordedInteractions = do
      interactions <- Map.fromList . addIndices <$> traverse toAsyncResponse recordedInteractions
      return Interactions {
        modified = NotModified
      , interactions
      , nextIndex = Index (Map.size interactions)
      }

    toAsyncResponse :: (Request, Response) -> IO (Request, Async Response)
    toAsyncResponse = traverse $ return >>> async

    addIndices :: [(a, b)] -> [(a, WithIndex b)]
    addIndices = zipWith (\ n (request, response) -> (request, WithIndex n response)) [0 ..]

closeTape'AnyOrder :: OpenTape AnyOrder -> IO ()
closeTape'AnyOrder tape = do
  withMVar tape.interactions \ (Interactions modified interactions _) -> case modified of
    NotModified -> pass
    Modified -> sequenceInteractions (toSortedList interactions) >>= Serialize.saveTape tape.file
  where
    sequenceInteractions :: [(Request, Async Response)] -> IO [(Request, Response)]
    sequenceInteractions = fmap rights . traverse waitCatchAll

    waitCatchAll :: (Request, Async Response) -> IO (Either SomeException (Request, Response))
    waitCatchAll = fmap sequence . traverse waitCatch

    toSortedList :: Map a (WithIndex b) -> [(a, b)]
    toSortedList = map (fmap value) . sortOnIndex . Map.toList

    sortOnIndex :: [(a, WithIndex b)] -> [(a, WithIndex b)]
    sortOnIndex = sortOn (index . snd)

    index :: WithIndex a -> Index
    index (WithIndex n _) = n

    value :: WithIndex a -> a
    value (WithIndex _ a) = a

openTape'Sequential :: ReadWriteMode -> Tape -> IO (OpenTape Sequential)
openTape'Sequential mode Tape{file, redact} = do
  replay <- if shouldReplay mode then Serialize.loadTape file else return []
  interactions <- newMVar InteractionSequence {
    modified = NotModified
  , replay
  , rec = []
  }
  return OpenTape {
    file
  , interactions
  , redact
  }

closeTape'Sequential :: OpenTape Sequential -> IO ()
closeTape'Sequential tape = withMVar tape.interactions \ interactions -> case interactions.modified of
  Modified -> Serialize.saveTape tape.file (reverse interactions.rec)
  _ -> pass

redactAuthorization :: Request -> Request
redactAuthorization request@Request{..} = request { requestHeaders = map redact requestHeaders }
  where
    redact :: Header -> Header
    redact header@(name, _)
      | name == hAuthorization = (name, "********")
      | otherwise = header
