{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : GitLab.SystemHooks.GitLabSystemHooks
-- Description : Haskell records corresponding to JSON data from GitLab system hooks
-- Copyright   : (c) Rob Stewart, Heriot-Watt University, 2020
-- License     : BSD3
-- Maintainer  : robstewart57@gmail.com
-- Stability   : stable
module GitLab.SystemHooks.GitLabSystemHooks
  ( receive,
    receiveString,
    tryFire,
  )
where

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Reader as MR
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Typeable
import GitLab.SystemHooks.Types
import GitLab.Types
import System.IO.Temp
import System.PosixCompat.Files

-- | Attempts to fire each rule in sequence. Reads the JSON data
-- received from the GitLab server from standard input.
receive :: [Rule] -> GitLab ()
receive rules = do
  eventContent <- liftIO TIO.getContents
  receiveString eventContent rules

-- | Attempts to fire each rule in sequence. Reads the JSON data
-- received from a function argument.
receiveString :: Text -> [Rule] -> GitLab ()
receiveString eventContent rules = do
  -- maybe log the JSON received
  traceSystemHook eventContent
  -- fire the rules
  didFire <- mapM (fire eventContent) rules
  -- if nothing fired
  when (not (or didFire)) $ do
    cfg <- MR.asks serverCfg
    -- maybe log the JSON if it was not parsed
    when (debugSystemHooks cfg == Just NonParsedJSON) $ liftIO $ do
      -- no rules fired, was it because the JSON was not parsed?
      when (not (attemptGitLabEventParse eventContent)) $ do
        fpath <- writeSystemTempFile "gitlab-system-hook-nonparsed-" (T.unpack eventContent)
        void $ setFileMode fpath otherReadMode
    -- maybe log the JSON if no rules were fired for it
    when (debugSystemHooks cfg == Just UnprocessedEvents) $ liftIO $ do
      fpath <- writeSystemTempFile "gitlab-system-hook-unprocessed-" (T.unpack eventContent)
      void $ setFileMode fpath otherReadMode

traceSystemHook :: Text -> GitLab ()
traceSystemHook eventContent = do
  cfg <- MR.asks serverCfg
  liftIO $
    E.catch
      ( when (debugSystemHooks cfg == Just AllJSON) $ do
          fpath <- writeSystemTempFile "gitlab-system-hook-" (T.unpack eventContent)
          void $ setFileMode fpath otherReadMode
      )
      -- runGitLabDbg must have been used, which doesn't define a GitLabServerConfig
      (\(_exception :: E.ErrorCall) -> return ())

orElse :: GitLab Bool -> GitLab Bool -> GitLab Bool
orElse f g = do
  x <- f
  if x
    then return True
    else g

fire :: Text -> Rule -> GitLab Bool
fire contents rule = do
  result <- tryFire contents rule
  case result of
    True -> do
      liftIO (putStrLn ("fired: " <> labelOf rule))
      return True
    False -> return False
  where
    labelOf :: Rule -> String
    labelOf (Match lbl _) = lbl
    labelOf (MatchIf lbl _ _) = lbl

-- | Try to fire a GitLab rule, returns 'True' if the rule fired and
-- 'False' if it did not fire.
tryFire :: Text -> Rule -> GitLab Bool
tryFire contents (Match _ f) = do
  fireIf'
    (Just (\_ -> return True))
    (cast f :: Maybe (ProjectCreate -> GitLab ()))
    (parseEvent contents :: Maybe ProjectCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectDestroy -> GitLab ()))
      (parseEvent contents :: Maybe ProjectDestroy)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectRename -> GitLab ()))
      (parseEvent contents :: Maybe ProjectRename)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectTransfer -> GitLab ()))
      (parseEvent contents :: Maybe ProjectTransfer)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (ProjectUpdate -> GitLab ()))
      (parseEvent contents :: Maybe ProjectUpdate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupMemberUpdate -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberUpdate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserAddToTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserAddToTeam)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserUpdateForTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserUpdateForTeam)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserRemoveFromTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserRemoveFromTeam)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserCreate -> GitLab ()))
      (parseEvent contents :: Maybe UserCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserRemove -> GitLab ()))
      (parseEvent contents :: Maybe UserRemove)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserFailedLogin -> GitLab ()))
      (parseEvent contents :: Maybe UserFailedLogin)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (UserRename -> GitLab ()))
      (parseEvent contents :: Maybe UserRename)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (KeyCreate -> GitLab ()))
      (parseEvent contents :: Maybe KeyCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (KeyRemove -> GitLab ()))
      (parseEvent contents :: Maybe KeyRemove)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupCreate -> GitLab ()))
      (parseEvent contents :: Maybe GroupCreate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupRemove)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupRename -> GitLab ()))
      (parseEvent contents :: Maybe GroupRename)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (NewGroupMember -> GitLab ()))
      (parseEvent contents :: Maybe NewGroupMember)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (GroupMemberRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberRemove)
    -- `orElse` fireIf'
    --   (Just (\_ -> return True))
    --   (cast f :: Maybe (ProjectEvent -> GitLab ()))
    --   (parseEvent contents :: Maybe ProjectEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (Push -> GitLab ()))
      (parseEvent contents :: Maybe Push)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (TagPush -> GitLab ()))
      (parseEvent contents :: Maybe TagPush)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (RepositoryUpdate -> GitLab ()))
      (parseEvent contents :: Maybe RepositoryUpdate)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (MergeRequestEvent -> GitLab ()))
      (parseEvent contents :: Maybe MergeRequestEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (BuildEvent -> GitLab ()))
      (parseEvent contents :: Maybe BuildEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (PipelineEvent -> GitLab ()))
      (parseEvent contents :: Maybe PipelineEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (IssueEvent -> GitLab ()))
      (parseEvent contents :: Maybe IssueEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (NoteEvent -> GitLab ()))
      (parseEvent contents :: Maybe NoteEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (WikiPageEvent -> GitLab ()))
      (parseEvent contents :: Maybe WikiPageEvent)
    `orElse` fireIf'
      (Just (\_ -> return True))
      (cast f :: Maybe (WorkItemEvent -> GitLab ()))
      (parseEvent contents :: Maybe WorkItemEvent)
tryFire contents (MatchIf _ predF f) = do
  fireIf'
    (cast predF :: Maybe (ProjectCreate -> GitLab Bool))
    (cast f :: Maybe (ProjectCreate -> GitLab ()))
    (parseEvent contents :: Maybe ProjectCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectDestroy -> GitLab Bool))
      (cast f :: Maybe (ProjectDestroy -> GitLab ()))
      (parseEvent contents :: Maybe ProjectDestroy)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectRename -> GitLab Bool))
      (cast f :: Maybe (ProjectRename -> GitLab ()))
      (parseEvent contents :: Maybe ProjectRename)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectTransfer -> GitLab Bool))
      (cast f :: Maybe (ProjectTransfer -> GitLab ()))
      (parseEvent contents :: Maybe ProjectTransfer)
    `orElse` fireIf'
      (cast predF :: Maybe (ProjectUpdate -> GitLab Bool))
      (cast f :: Maybe (ProjectUpdate -> GitLab ()))
      (parseEvent contents :: Maybe ProjectUpdate)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupMemberUpdate -> GitLab Bool))
      (cast f :: Maybe (GroupMemberUpdate -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberUpdate)
    `orElse` fireIf'
      (cast predF :: Maybe (UserAddToTeam -> GitLab Bool))
      (cast f :: Maybe (UserAddToTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserAddToTeam)
    `orElse` fireIf'
      (cast predF :: Maybe (UserUpdateForTeam -> GitLab Bool))
      (cast f :: Maybe (UserUpdateForTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserUpdateForTeam)
    `orElse` fireIf'
      (cast predF :: Maybe (UserRemoveFromTeam -> GitLab Bool))
      (cast f :: Maybe (UserRemoveFromTeam -> GitLab ()))
      (parseEvent contents :: Maybe UserRemoveFromTeam)
    `orElse` fireIf'
      (cast predF :: Maybe (UserCreate -> GitLab Bool))
      (cast f :: Maybe (UserCreate -> GitLab ()))
      (parseEvent contents :: Maybe UserCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (UserRemove -> GitLab Bool))
      (cast f :: Maybe (UserRemove -> GitLab ()))
      (parseEvent contents :: Maybe UserRemove)
    `orElse` fireIf'
      (cast predF :: Maybe (UserFailedLogin -> GitLab Bool))
      (cast f :: Maybe (UserFailedLogin -> GitLab ()))
      (parseEvent contents :: Maybe UserFailedLogin)
    `orElse` fireIf'
      (cast predF :: Maybe (UserRename -> GitLab Bool))
      (cast f :: Maybe (UserRename -> GitLab ()))
      (parseEvent contents :: Maybe UserRename)
    `orElse` fireIf'
      (cast predF :: Maybe (KeyCreate -> GitLab Bool))
      (cast f :: Maybe (KeyCreate -> GitLab ()))
      (parseEvent contents :: Maybe KeyCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (KeyRemove -> GitLab Bool))
      (cast f :: Maybe (KeyRemove -> GitLab ()))
      (parseEvent contents :: Maybe KeyRemove)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupCreate -> GitLab Bool))
      (cast f :: Maybe (GroupCreate -> GitLab ()))
      (parseEvent contents :: Maybe GroupCreate)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupRemove -> GitLab Bool))
      (cast f :: Maybe (GroupRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupRemove)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupRename -> GitLab Bool))
      (cast f :: Maybe (GroupRename -> GitLab ()))
      (parseEvent contents :: Maybe GroupRename)
    `orElse` fireIf'
      (cast predF :: Maybe (NewGroupMember -> GitLab Bool))
      (cast f :: Maybe (NewGroupMember -> GitLab ()))
      (parseEvent contents :: Maybe NewGroupMember)
    `orElse` fireIf'
      (cast predF :: Maybe (GroupMemberRemove -> GitLab Bool))
      (cast f :: Maybe (GroupMemberRemove -> GitLab ()))
      (parseEvent contents :: Maybe GroupMemberRemove)
    -- `orElse` fireIf'
    --   (cast predF :: Maybe (ProjectEvent -> GitLab Bool))
    --   (cast f :: Maybe (ProjectEvent -> GitLab ()))
    --   (parseEvent contents :: Maybe ProjectEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (Push -> GitLab Bool))
      (cast f :: Maybe (Push -> GitLab ()))
      (parseEvent contents :: Maybe Push)
    `orElse` fireIf'
      (cast predF :: Maybe (TagPush -> GitLab Bool))
      (cast f :: Maybe (TagPush -> GitLab ()))
      (parseEvent contents :: Maybe TagPush)
    `orElse` fireIf'
      (cast predF :: Maybe (RepositoryUpdate -> GitLab Bool))
      (cast f :: Maybe (RepositoryUpdate -> GitLab ()))
      (parseEvent contents :: Maybe RepositoryUpdate)
    `orElse` fireIf'
      (cast predF :: Maybe (MergeRequestEvent -> GitLab Bool))
      (cast f :: Maybe (MergeRequestEvent -> GitLab ()))
      (parseEvent contents :: Maybe MergeRequestEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (BuildEvent -> GitLab Bool))
      (cast f :: Maybe (BuildEvent -> GitLab ()))
      (parseEvent contents :: Maybe BuildEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (PipelineEvent -> GitLab Bool))
      (cast f :: Maybe (PipelineEvent -> GitLab ()))
      (parseEvent contents :: Maybe PipelineEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (IssueEvent -> GitLab Bool))
      (cast f :: Maybe (IssueEvent -> GitLab ()))
      (parseEvent contents :: Maybe IssueEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (NoteEvent -> GitLab Bool))
      (cast f :: Maybe (NoteEvent -> GitLab ()))
      (parseEvent contents :: Maybe NoteEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (WikiPageEvent -> GitLab Bool))
      (cast f :: Maybe (WikiPageEvent -> GitLab ()))
      (parseEvent contents :: Maybe WikiPageEvent)
    `orElse` fireIf'
      (cast predF :: Maybe (WorkItemEvent -> GitLab Bool))
      (cast f :: Maybe (WorkItemEvent -> GitLab ()))
      (parseEvent contents :: Maybe WorkItemEvent)

fireIf' :: (Typeable a, Show a) => Maybe (a -> GitLab Bool) -> Maybe (a -> GitLab ()) -> Maybe a -> GitLab Bool
fireIf' castPred castF parsed = do
  case castPred of
    Nothing -> return False
    Just pred' ->
      case castF of
        Nothing -> return False
        Just f' ->
          case parsed of
            Nothing -> return False
            Just parsed' -> do
              testPred <- pred' parsed'
              if testPred
                then do
                  f' parsed'
                  return True
                else return False

-- | returns 'True' if at least on parsing attemps of the JSON is successful
attemptGitLabEventParse :: T.Text -> Bool
attemptGitLabEventParse contents =
  -- note: this needs refactoring to be less verbose
  case parseEvent contents :: Maybe ProjectCreate of
    Just _ -> True
    Nothing ->
      case parseEvent contents :: Maybe ProjectDestroy of
        Just _ -> True
        Nothing ->
          case parseEvent contents :: Maybe ProjectRename of
            Just _ -> True
            Nothing ->
              case parseEvent contents :: Maybe ProjectTransfer of
                Just _ -> True
                Nothing ->
                  case parseEvent contents :: Maybe ProjectUpdate of
                    Just _ -> True
                    Nothing ->
                      case parseEvent contents :: Maybe GroupMemberUpdate of
                        Just _ -> True
                        Nothing ->
                          case parseEvent contents :: Maybe UserAddToTeam of
                            Just _ -> True
                            Nothing ->
                              case parseEvent contents :: Maybe UserUpdateForTeam of
                                Just _ -> True
                                Nothing ->
                                  case parseEvent contents :: Maybe UserRemoveFromTeam of
                                    Just _ -> True
                                    Nothing ->
                                      case parseEvent contents :: Maybe UserCreate of
                                        Just _ -> True
                                        Nothing ->
                                          case parseEvent contents :: Maybe UserRemove of
                                            Just _ -> True
                                            Nothing ->
                                              case parseEvent contents :: Maybe UserFailedLogin of
                                                Just _ -> True
                                                Nothing ->
                                                  case parseEvent contents :: Maybe UserRename of
                                                    Just _ -> True
                                                    Nothing ->
                                                      case parseEvent contents :: Maybe KeyCreate of
                                                        Just _ -> True
                                                        Nothing ->
                                                          case parseEvent contents :: Maybe KeyRemove of
                                                            Just _ -> True
                                                            Nothing ->
                                                              case parseEvent contents :: Maybe GroupCreate of
                                                                Just _ -> True
                                                                Nothing ->
                                                                  case parseEvent contents :: Maybe GroupRemove of
                                                                    Just _ -> True
                                                                    Nothing ->
                                                                      case parseEvent contents :: Maybe GroupRename of
                                                                        Just _ -> True
                                                                        Nothing ->
                                                                          case parseEvent contents :: Maybe NewGroupMember of
                                                                            Just _ -> True
                                                                            Nothing ->
                                                                              case parseEvent contents :: Maybe GroupMemberRemove of
                                                                                Just _ -> True
                                                                                Nothing ->
                                                                                  case parseEvent contents :: Maybe Push of
                                                                                    Just _ -> True
                                                                                    Nothing ->
                                                                                      case parseEvent contents :: Maybe TagPush of
                                                                                        Just _ -> True
                                                                                        Nothing ->
                                                                                          case parseEvent contents :: Maybe RepositoryUpdate of
                                                                                            Just _ -> True
                                                                                            Nothing ->
                                                                                              case parseEvent contents :: Maybe MergeRequestEvent of
                                                                                                Just _ -> True
                                                                                                Nothing ->
                                                                                                  case parseEvent contents :: Maybe BuildEvent of
                                                                                                    Just _ -> True
                                                                                                    Nothing ->
                                                                                                      case parseEvent contents :: Maybe PipelineEvent of
                                                                                                        Just _ -> True
                                                                                                        Nothing ->
                                                                                                          case parseEvent contents :: Maybe IssueEvent of
                                                                                                            Just _ -> True
                                                                                                            Nothing ->
                                                                                                              case parseEvent contents :: Maybe NoteEvent of
                                                                                                                Just _ -> True
                                                                                                                Nothing ->
                                                                                                                  case parseEvent contents :: Maybe WikiPageEvent of
                                                                                                                    Just _ -> True
                                                                                                                    Nothing ->
                                                                                                                      case parseEvent contents :: Maybe WorkItemEvent of
                                                                                                                        Just _ -> True
                                                                                                                        Nothing -> False
