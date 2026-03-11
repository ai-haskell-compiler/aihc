{-# LANGUAGE FlexibleContexts #-}

module Pipes.MongoDB (
  fromCursor
, action
, find
, runMR
) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Database.MongoDB (Document)
import Database.MongoDB.Query (Action, Cursor, Query, MapReduce, nextBatch)
import Pipes (Producer, MonadIO, lift, each)
import qualified Database.MongoDB.Query as Q

fromCursor :: (MonadIO m, MonadBaseControl IO m)
           => Cursor -> Producer Document (Action m) ()
fromCursor c = do
  docs <- lift (nextBatch c)
  case docs of
    [] -> return ()
    xs -> each xs >> fromCursor c

action :: (MonadBaseControl IO m, MonadIO m)
       => Action m Cursor -> Producer Document (Action m) ()
action f = lift f >>= fromCursor

find :: (MonadIO m, MonadBaseControl IO m)
     => Query -> Producer Document (Action m) ()
find = action . Q.find

runMR :: (MonadIO m, MonadBaseControl IO m)
     => MapReduce -> Producer Document (Action m) ()
runMR = action . Q.runMR
