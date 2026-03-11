{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE CPP                 #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

{-|
The module provides a higher level API over BodhiReleases
with a Release record type.
-}

module Distribution.Fedora.Release
  ( Release(..)
  , getActiveReleases
  , getFedoraReleases
  , getEPELReleases
  , getBranchRelease
  , getRawhideVersion
  , getCurrentFedoraVersion
  )
where

import Control.Monad (guard)
import Data.Aeson(Object)
import Data.Maybe (mapMaybe)
import Distribution.Fedora.BodhiReleases
import Numeric.Natural (Natural)
import Safe (headMay)

-- | Fedora Release data
--
-- See <https://bodhi.fedoraproject.org/releases/>
data Release = Release {
    releaseName :: String,
    releaseVersion :: String,
    releaseIdPrefix :: String,
    releaseBranch :: String,
    releaseComposed :: Bool,
    releaseCandidateTag :: String,
    releaseDistTag :: String,
    releaseSettingStatus :: Maybe String,
    releaseState :: String,
    releaseAutomaticUpdates :: Bool,
    releaseTestingRepo :: Maybe String,
    releaseTestingTag :: String,
    releaseEOL :: Maybe String
  } deriving (Show,Eq)

readRelease :: Object -> Maybe Release
readRelease obj = do
  name <- lookupKey "name" obj
  ver <- lookupKey "version" obj
  idPref <- lookupKey "id_prefix" obj
  guard (idPref `notElem` ["FEDORA-CONTAINER","FEDORA-FLATPAK"])
  br <- lookupKey "branch" obj
  composed <- lookupKey "composed_by_bodhi" obj
  candidate <- lookupKey "candidate_tag" obj
  disttag <- lookupKey "dist_tag" obj
  let setting = lookupKey "setting_status" obj
  state <- lookupKey "state" obj
  automatic <- lookupKey "create_automatic_updates" obj
  let testrepo = lookupKey "testing_repository" obj
  testtag <- lookupKey "testing_tag" obj
  let eol = lookupKey "eol" obj
  return $ Release name ver idPref br composed candidate disttag setting state automatic testrepo testtag eol

-- | ordered by releaseName
instance Ord Release where
  compare r1 r2 =
    compare (releaseName r1) (releaseName r2)

-- containers and flatpaks are filtered out by readRelease
-- | Get list of all current Fedora Project releases (from Bodhi)
getActiveReleases :: IO [Release]
getActiveReleases =
  mapMaybe readRelease <$> getBodhiReleases

-- getProductReleases :: String -> IO [Release]
-- getProductReleases name =
--   mapMaybe readRelease <$> getBodhiProductReleases name

-- | Get list of current Fedora Linux releases
getFedoraReleases :: IO [Release]
getFedoraReleases =
  mapMaybe readRelease <$> getBodhiFedoraReleases

-- | Get list of current Fedora EPEL releases
getEPELReleases :: IO [Release]
getEPELReleases =
  mapMaybe readRelease <$> getBodhiEPELReleases

-- releaseFilter :: (Release -> a) -> (a -> Bool) -> [Release] -> [Release]
-- releaseFilter f p = filter (p . f)

-- | Get the Release for an active Branch
--
-- Errors for an inactive Branch
getBranchRelease :: String -> IO Release
getBranchRelease br = do
  rels <- mapMaybe readRelease <$> getBodhiBranchReleases br
  case rels of
    [] -> error $ "release not found for branch " ++ br
    [rel] -> return rel
    _ -> error $ "multiple releases for " ++ br ++ ":\n" ++ unwords (map releaseName rels)

-- | Get the Fedora release version of Rawhide
getRawhideVersion :: IO Natural
getRawhideVersion =
  read . releaseVersion <$> getBranchRelease "rawhide"

-- | Get the latest current stable Fedora release version
getCurrentFedoraVersion :: IO Natural
getCurrentFedoraVersion = do
  rels <- filter ((== "current") . releaseState) <$> getFedoraReleases
  case headMay rels of
    Just rel -> return $ read $ releaseVersion rel
    Nothing -> error "current fedora release not found"
