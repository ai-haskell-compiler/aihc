{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: GPL-2.0-only
Maintainer: Jens Petersen <petersen@redhat.com>

Copr REST client library
-}

module Web.Fedora.Copr
  (coprChroots,
   fedoraCopr)
where

import Data.Aeson.Types (Object)
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import Data.List (sortBy)
import Data.Ord (comparing, Down(Down))
import Data.Text (Text)

import Web.Fedora.Copr.API

-- | Get list of chroots of a user's copr project, sorted in descending order.
coprChroots :: String -- ^ server
            -> String -- ^ owner
            -> String -- ^ project
            -> IO [Text] -- ^ list of chroots
coprChroots server owner project = do
  proj <- coprGetProject server owner project
  case lookupKey "chroot_repos" proj :: Maybe Object of
    Nothing ->
        case lookupKey "error" proj of
          Just err -> error err
          Nothing -> return []
    Just obj -> return $ (sortBy (comparing Down) . map toText . M.keys) obj
#if !MIN_VERSION_aeson(2,0,0)
      where toText = id
#endif

-- | the host name of the Fedora Copr API server
fedoraCopr :: String
fedoraCopr = "copr.fedorainfracloud.org"
