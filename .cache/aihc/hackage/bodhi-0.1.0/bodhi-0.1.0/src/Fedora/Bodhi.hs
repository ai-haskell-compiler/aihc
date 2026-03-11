{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: MIT
Maintainer: Jens Petersen <juhpetersen@gmail.com>

Fedora Bodhi REST client library
-}

module Fedora.Bodhi
  ( bodhiBuild
  , bodhiBuilds
  , bodhiComment
  , bodhiComments
  , bodhiCSRF
  , bodhiOverride
  , bodhiOverrides
  , bodhiOverrideDates
  , bodhiPackages
  , bodhiRelease
  , bodhiReleases
  , bodhiUpdate
  , bodhiUpdates
  , bodhiUser
  , bodhiUsers
  , lookupKey
  , lookupKey'
  , queryBodhi
  , makeKey
  , makeItem
  , maybeKey
  , Query
  , QueryItem
  ) where

import Data.Aeson.Types
import Data.Text (Text)
import Data.Time.LocalTime
import Network.HTTP.Query

server :: String
server = "bodhi.fedoraproject.org"

-- | Returns build JSON for NVR
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/builds.html#service-0
bodhiBuild :: String -> IO Object
bodhiBuild nvr =
  queryBodhi [] $ "builds" +/+ nvr

-- | returns JSON list of builds
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/builds.html#service-1
bodhiBuilds :: Query -> IO [Object]
bodhiBuilds params =
  lookupKey' "builds" <$> queryBodhi params "builds/"

-- | Returns comment JSON for id
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/comments.html#service-0
bodhiComment :: String -> IO Object
bodhiComment cid =
  queryBodhi [] $ "comments" +/+ cid

-- | returns JSON list of comments
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/comments.html#service-1
bodhiComments :: Query -> IO [Object]
bodhiComments params =
  lookupKey' "comments" <$> queryBodhi params "comments/"

-- | Get CSRF token
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/csrf.html
bodhiCSRF :: IO (Maybe Text)
bodhiCSRF =
  lookupKey "csrf_token" <$> queryBodhi [] "csrf"

-- | Returns override JSON for NVR
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/overrides.html#service-0
bodhiOverride :: String -> IO (Maybe Object)
bodhiOverride nvr =
  lookupKey "override" <$> queryBodhi [] ("overrides" +/+ nvr)

-- | Returns override expiration and submission dates for NVR
bodhiOverrideDates :: String -> IO (Maybe (LocalTime,LocalTime))
bodhiOverrideDates nvr = do
  mobj <- bodhiOverride nvr
  case mobj of
    Nothing -> do
      putStrLn $ "Override for " ++ nvr ++ " not found"
      return Nothing
    Just obj -> return $ readDates obj
  where
    readDates :: Object -> Maybe (LocalTime,LocalTime)
    readDates =
      parseMaybe $ \obj -> do
        expire <- obj .: "expiration_date"
        submit <- obj .: "submission_date"
        return (expire,submit)

-- | returns JSON list of overrides
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/overrides.html#service-1
bodhiOverrides :: Query -> IO [Object]
bodhiOverrides params =
  lookupKey' "overrides" <$> queryBodhi params "overrides/"

-- | Packages query
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/packages.html#service-0
bodhiPackages :: Query -> IO [Object]
bodhiPackages params =
  lookupKey' "packages" <$> queryBodhi params "packages/"

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/releases.html#service-0
bodhiRelease :: String -> IO Object
bodhiRelease rel =
  queryBodhi [] $ "releases" +/+ rel

-- | read releases metadata from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/releases.html#service-1
bodhiReleases :: Query -> IO [Object]
-- FIXME handle errors:
-- fromList [("status",String "error"),("errors",Array [Object (fromList [("location",String "body"),("name",String "name"),("description",String "No such release")])])]
bodhiReleases params =
  lookupKey' "releases" <$> queryBodhi params "releases/"

-- | read an update from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/updates.html#service-0
bodhiUpdate :: String -> IO (Maybe Object)
bodhiUpdate update =
  lookupKey "update" <$> queryBodhi [] ("updates" +/+ update)

-- | search for updates on Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/updates.html#service-2
bodhiUpdates :: Query -> IO [Object]
bodhiUpdates params =
  lookupKey' "updates" <$> queryBodhi params "updates/"

-- | user info from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/users.html#service-0
bodhiUser :: String -> IO Object
bodhiUser user =
  queryBodhi [] $ "users" +/+ user

-- | list users from Bodhi
--
-- https://bodhi.fedoraproject.org/docs/server_api/rest/users.html#service-1
bodhiUsers :: Query -> IO [Object]
bodhiUsers params =
  lookupKey' "users" <$> queryBodhi params "users/"

-- | low-level query
queryBodhi :: FromJSON a => Query -> String -> IO a
queryBodhi params path =
  let url = "https://" ++ server +/+ path
  in webAPIQuery url params
