{-# LANGUAGE CPP, OverloadedStrings #-}

module Snapshots (
  getMajorVers,
  latestMajorSnapshot,
  latestLTS,
  latestLtsSnapshot,
  resolveMajor
  )
where

import Data.Aeson
import Data.List.Extra
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import Data.Ord (comparing, Down(Down))
import qualified Data.Text as T
import Network.HTTP.Query
import SimpleCmd (error')
import System.Cached.JSON

import MajorVer

excludedMajors :: [MajorVer]
excludedMajors = [LTS 17, LTS 15, LTS 7, LTS 3, LTS 0]

getMajorVers :: IO [MajorVer]
getMajorVers = do
  obj <- getSnapshots False
  return $
    sortBy (comparing Down) $
    map (readMajor . T.unpack . toText) (M.keys obj \\ ["lts"]) \\ excludedMajors
#if !MIN_VERSION_aeson(2,0,0)
  where toText = id
#endif

latestMajorSnapshot :: Bool -> MajorVer -> IO (Maybe String)
latestMajorSnapshot forcerefresh ver = do
  lookupKey (T.pack (showMajor ver)) <$> getSnapshots forcerefresh

getSnapshots :: Bool -> IO Object
getSnapshots forcerefresh =
  getCachedJSON "stackage-snapshots" "snapshots.json" "https://www.stackage.org/download/snapshots.json" $ if forcerefresh then 0 else 200 --minutes

latestLTS :: Bool -> IO MajorVer
latestLTS refresh = do
  msnap <- lookupKey "lts" <$> getSnapshots refresh
  case msnap of
    Just snap -> return $ snapMajorVer snap
    Nothing -> error' "could not resolve lts major version"

latestLtsSnapshot :: Bool -> IO String
latestLtsSnapshot refresh = do
  msnap <- resolveMajor LatestLTS >>= latestMajorSnapshot refresh
  case msnap of
    Nothing ->
      error' "failed to determine latest lts snapshot"
    Just snap -> return snap

-- converts 'lts' to actual version
resolveMajor :: MajorVerAlias -> IO MajorVer
resolveMajor LatestLTS = latestLTS False
resolveMajor (MajorVer ver) = return ver
