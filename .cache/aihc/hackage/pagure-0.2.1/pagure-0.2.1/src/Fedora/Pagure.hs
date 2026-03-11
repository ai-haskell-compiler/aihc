{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
Copyright: (c) 2020-2024 Jens Petersen
SPDX-License-Identifier: GPL-2.0-or-later
Maintainer: Jens Petersen <petersen@redhat.com>

Pagure REST client library
-}

module Fedora.Pagure
  ( pagureProjectInfo
  , pagureListProjects
  , pagureListProjectIssues
  , IssueTitleStatus(..)
  , pagureListProjectIssueTitlesStatus
  , pagureProjectIssueInfo
  , pagureListGitBranches
  , pagureListGitBranchesWithCommits
  , pagureListUsers
  , pagureUserForks
  , pagureUserInfo
  , pagureUserRepos
  , pagureListGroups
  , pagureGroupInfo
  , pagureGroupRepos
  , pagureProjectGitURLs
  , queryPagure
  , queryPagureSingle
  , queryPagureCount
  , queryPagureCountPaged
  , makeKey
  , makeItem
  , maybeKey
  , Query
  , QueryItem
  , lookupKey
  , lookupKey'
  , getRepos
  ) where

import Control.Monad
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Query
import System.IO (hPutStrLn, stderr)

-- | Project info
--
-- @pagureProjectInfo server "<repo>"@
-- @pagureProjectInfo server "<namespace>/<repo>"@
--
-- https://pagure.io/api/0/#projects-tab
pagureProjectInfo :: String -- ^ server
                  -> String -- ^ project
                  -> IO (Either String Object)
pagureProjectInfo server project = do
  let path = project
  queryPagureSingle server path []

-- | List projects
--
-- https://pagure.io/api/0/#projects-tab
pagureListProjects :: String -- ^ server
                   -> Query  -- ^ parameters
                   -> IO Object
pagureListProjects server params = do
  let path = "projects"
  queryPagure server path params

-- | List project issues
--
-- https://pagure.io/api/0/#issues-tab
pagureListProjectIssues :: String -- ^ server
                        -> String -- ^ project repo
                        -> Query  -- ^ parameters
                        -> IO (Either String Object)
pagureListProjectIssues server repo params = do
  let path = repo +/+ "issues"
  queryPagureSingle server path params

data IssueTitleStatus =
  IssueTitleStatus { pagureIssueId :: Integer
                   , pagureIssueTitle :: String
                   , pagureIssueStatus :: T.Text
                   , pagureIssueCloseStatus :: Maybe T.Text
                   }

-- | List project issue titles
--
-- https://pagure.io/api/0/#issues-tab
pagureListProjectIssueTitlesStatus :: String -- ^ server
                                   -> String -- ^ repo
                                   -> Query  -- ^ parameters
  -> IO (Either String [IssueTitleStatus])
pagureListProjectIssueTitlesStatus server repo params = do
  let path = repo +/+ "issues"
  res <- queryPagureSingle server path params
  return $ case res of
    Left e -> Left e
    Right v -> Right $ mapMaybe parseIssue $ lookupKey' "issues" v
  where
    parseIssue :: Object -> Maybe IssueTitleStatus
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        mcloseStatus <- obj .:? "close_status"
        return $ IssueTitleStatus id' (T.unpack title) status mcloseStatus

-- | Issue information
--
-- https://pagure.io/api/0/#issues-tab
pagureProjectIssueInfo :: String -- ^ server
                       -> String -- ^ repo
                       -> Int    -- ^ issue number
                       -> IO (Either String Object)
pagureProjectIssueInfo server repo issue = do
  let path = repo +/+ "issue" +/+ show issue
  queryPagureSingle server path []

-- | List repo branches
--
-- https://pagure.io/api/0/#projects-tab
pagureListGitBranches :: String -- ^ server
                      -> String -- ^ repo
                      -> IO (Either String [String])
pagureListGitBranches server repo = do
  let path = repo +/+ "git/branches"
  res <- queryPagureSingle server path []
  return $ case res of
    Left e -> Left e
    Right v -> map T.unpack <$> lookupKeyEither "branches" v

-- | List repo branches with commits
--
-- https://pagure.io/api/0/#projects-tab
pagureListGitBranchesWithCommits :: String -- ^ server
                                 -> String -- ^ repo
                                 -> IO (Either String Object)
pagureListGitBranchesWithCommits server repo = do
  let path = repo +/+ "git/branches"
      params = makeKey "with_commits" "1"
  res <- queryPagureSingle server path params
  return $ case res of
    Left e -> Left e
    Right v -> lookupKeyEither "branches" v

-- | List users
--
-- https://pagure.io/api/0/#users-tab
pagureListUsers :: String -- ^ server
                -> String -- ^ pattern
                -> IO Object
pagureListUsers server pat = do
  let path = "users"
      params = makeKey "pattern" pat
  queryPagure server path params

-- | User information
--
-- https://pagure.io/api/0/#users-tab
pagureUserInfo :: String -- ^ server
               -> String -- ^ user
               -> Query  -- ^ parameters
               -> IO (Either String Object)
pagureUserInfo server user params = do
  let path = "user" +/+ user
  queryPagureSingle server path params

-- | List groups
--
-- https://pagure.io/api/0/#groups-tab
pagureListGroups :: String -- ^ server
                 -> Maybe String -- ^ optional pattern
                 -> Query  -- ^ parameters
                 -> IO Object
pagureListGroups server mpat paging = do
  let path = "groups"
      params = maybeKey "pattern" mpat ++ paging
  queryPagure server path params

-- | Group information
--
-- https://pagure.io/api/0/#groups-tab
pagureGroupInfo :: String -- ^ server
                -> String -- ^ group
                -> Query  -- ^ parameters
                -> IO (Either String Object)
pagureGroupInfo server group params = do
  let path = "group" +/+ group
  queryPagureSingle server path params

-- | Project Git URLs
--
-- https://pagure.io/api/0/#projects-tab
pagureProjectGitURLs :: String -- ^ server
                     -> String -- ^ repo
                     -> IO (Either String Object)
pagureProjectGitURLs server repo = do
  let path = repo +/+ "git/urls"
  queryPagureSingle server path []

-- | low-level query
queryPagure :: String -- ^ server
            -> String -- ^ api path
            -> Query  -- ^ parameters
            -> IO Object
queryPagure server path params =
  let url = "https://" ++ server +/+ "api/0" +/+ path
  in webAPIQuery url params

-- | single query
queryPagureSingle :: String -- ^ server
                  -> String -- ^ api path
                  -> Query  -- ^ parameters
                  -> IO (Either String Object)
queryPagureSingle server path params = do
  res <- queryPagure server path params
  return $ case lookupKey "error" res of
             Just err -> Left (T.unpack err)
             Nothing -> Right res

-- | count total number of hits
queryPagureCount :: String -- ^ server
                 -> String -- ^ api path
                 -> Query  -- ^ parameters
                 -> String -- ^ pagination name
                 -> IO (Maybe Integer)
queryPagureCount server path params pagination = do
  eres <- queryPagureSingle server path (params ++ makeKey "per_page" "1")
  case eres of
    Left err -> do
      warning err
      return Nothing
    Right res ->
      return $ lookupKey (T.pack pagination) res >>= lookupKey "pages"

-- | get all pages of results
--
-- Warning: this can potentially download very large amounts of data.
-- For potentially large queries, it is a good idea to queryPagureCount first.
queryPagurePaged :: String -- ^ server
                 -> String -- ^ api path
                 -> Query  -- ^ parameters
                 -> (String,String) -- ^  pagination and paging names
                 -> IO [Object]
queryPagurePaged server path params (pagination,paging) = do
  -- FIXME allow overriding per_page
  let maxPerPage = "100"
  eres <- queryPagureSingle server path (params ++ makeKey "per_page" maxPerPage)
  case eres of
    Left err -> do
      warning err
      return []
    Right res1 ->
      case (lookupKey (T.pack pagination) res1 :: Maybe Object) >>= lookupKey "pages" :: Maybe Int of
        Nothing -> return []
        Just pages -> do
          when (pages > 1) $
            warning $ "receiving " ++ show pages ++ " pages × " ++ maxPerPage ++ " results..."
          rest <- mapM nextPage [2..pages]
          return $ res1 : rest
  where
    nextPage p =
      queryPagure server path (params ++ makeKey "per_page" "100" ++ makeKey paging (show p))

-- | list user's repos
pagureUserRepos :: String -- ^ server
                -> String -- ^ user
                -> IO [Text]
pagureUserRepos server user = do
  let path = "user" +/+ user
  pages <- queryPagurePaged server path [] ("repos_pagination", "repopage")
  return $ concatMap (getRepos "repos") pages

-- | list user's forks
pagureUserForks :: String -- ^ server
                -> String -- ^ user
                -> IO [Text]
pagureUserForks server user = do
  let path = "user" +/+ user
  pages <- queryPagurePaged server path [] ("forks_pagination", "forkpage")
  return $ concatMap (getRepos "forks") pages

-- | list group's repos
pagureGroupRepos :: String -- ^ server
                 -> Bool -- ^ count
                 -> String -- ^ group
                 -> IO [Text]
pagureGroupRepos server count group = do
  let path = "group" +/+ group
      params = makeKey "projects" "1"
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  return $ concatMap (getRepos "projects") pages

-- | helper to extract fullnames of repos
getRepos :: Text   -- ^ field (eg "repos")
         -> Object -- ^ results page
         -> [Text]
getRepos field obj =
  map (lookupKey' "fullname") $ lookupKey' field obj

-- | Get count (with queryPagureCount) or full results (queryPagurePaged)
queryPagureCountPaged :: String -- ^ server
                      -> Bool   -- ^ count
                      -> String -- ^ api path
                      -> Query  -- ^ parameters
                      -> (String,String) -- ^ pagination and paging names
                      -> IO [Object]
queryPagureCountPaged server count path params (pagination,paging) =
  if count
    then do
    mnum <- queryPagureCount server path params pagination
    maybe (warning "no results found") print mnum
    return []
    else queryPagurePaged server path params (pagination,paging)

-- from simple-cmd
warning :: String -> IO ()
warning s = hPutStrLn stderr $! s
