{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.GitRev
  ( GitRev (..)
  , gitRev
  , tGitRev
  ) where

import Data.Aeson
import Yesod.Core
import Yesod.Core.Types
import Yesod.GitRev.Data

getGitRevR :: Yesod site => SubHandlerFor GitRev site TypedContent
getGitRevR = getSubYesod >>= \GitRev{..} -> liftHandler $ selectRep $ do
  provideRep $ defaultLayout $ do
    [whamlet|
      <dl>
        <dt>Hash
        <dd>#{gitRevHash}
        <dt>Branch
        <dd>#{gitRevBranch}
        <dt>Dirty
        <dd>#{gitRevDirty}
        <dt>Commit Date
        <dd>#{gitRevCommitDate}
        <dt>Commit Count
        <dd>#{gitRevCommitCount}
        <dt>Commit Message
        <dd>#{gitRevCommitMessage}
    |]
  -- TODO: derive & use toJSON
  provideRep $ return $ object
    [ "hash"   .= gitRevHash
    , "branch" .= gitRevBranch
    , "dirty"  .= gitRevDirty
    , "commitDate" .= gitRevCommitDate
    , "commitCount" .= gitRevCommitCount
    , "commitMessage" .= gitRevCommitMessage
    ]

instance Yesod site => YesodSubDispatch GitRev site where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesGitRev)
