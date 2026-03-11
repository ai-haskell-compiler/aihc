{-# LANGUAGE OverloadedStrings #-}

module BodhiRelease (
  BodhiRelease (..),
  activeFedoraRelease,
  activeFedoraReleases,
  fedoraReleaseState,
  rawhideFedoraRelease,
  fedoraReleasePostBeta
  )
where

import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Distribution.Fedora.BodhiReleases (getBodhiFedoraReleases, lookupKey)
import SimpleCmd (error')

import Types (Natural)

data BodhiRelease =
  Release {releaseVersion :: String, -- to handle "eln"
           releaseState ::  String,
           releaseBranch :: String,
           releaseComposed :: Bool,
           releasePostBeta :: Bool}
  deriving Eq

-- Left is oldest active version
activeFedoraRelease :: Natural -> IO (Either Natural BodhiRelease)
activeFedoraRelease n = do
  active <- activeFedoraReleases
  case L.sortOn releaseVersion active of
    [] -> error' "failed to find active releases with Bodhi API"
    (oldest:_) ->
      return $
      case L.find (\r -> releaseVersion r == show n) active of
        Just rel -> Right rel
        Nothing -> Left $ read $ releaseVersion oldest

activeFedoraReleases :: IO [BodhiRelease]
activeFedoraReleases =
  L.nub . mapMaybe maybeRelease <$> getBodhiFedoraReleases
  where
    maybeRelease obj = do
      version <- lookupKey "version" obj
      branch <- lookupKey "branch" obj
      state <- lookupKey "state" obj
      composed <- lookupKey "composed_by_bodhi" obj
      let setting = lookupKey "setting_status" obj
      return $
        Release version state branch composed $
        setting == Just ("post_beta" :: String)

fedoraReleaseState :: Natural -> IO String
fedoraReleaseState n = do
  eactive <- activeFedoraRelease n
  return $
    case eactive of
      Left _ -> error' $ "could find F" ++ show n ++ " release (state)"
      Right rel -> releaseState rel

rawhideFedoraRelease :: IO Natural
rawhideFedoraRelease = do
  actives <- activeFedoraReleases
  let pending = map releaseVersion (filter (\r -> releaseState r == "pending") actives)
  return $ read $ maximum (L.delete "eln" pending)

fedoraReleasePostBeta :: Natural -> IO Bool
fedoraReleasePostBeta n = do
  eactive <- activeFedoraRelease n
  return $
    case eactive of
      Left _ -> error' $ "could find F" ++ show n ++ " release (setting)"
      Right rel -> releasePostBeta rel
