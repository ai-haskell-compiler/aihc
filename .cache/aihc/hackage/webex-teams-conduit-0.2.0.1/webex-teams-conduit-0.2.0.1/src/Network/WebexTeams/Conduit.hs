{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Network.WebexTeams.Conduit
Copyright   : (c) Naoto Shimazaki 2018
License     : MIT (see the file LICENSE)

Maintainer  : https://github.com/nshimaza
Stability   : experimental

This module provides Conduit wrapper for Cisco Webex Teams list APIs.

-}
module Network.WebexTeams.Conduit
    (
    -- * Functions
      streamListWithFilter
    , streamTeamList
    , streamOrganizationList
    , streamRoleList
    ) where

import           Conduit            (ConduitT, MonadIO, liftIO, yieldMany)
import           Control.Monad      (unless)

import           Network.WebexTeams hiding (streamOrganizationList,
                                     streamRoleList, streamTeamList)


{-|
    Common worker function for List APIs.
    It accesses List API with given 'Request', unwrap result into list of items, stream them to Conduit pipe
    and finally it automatically accesses next page designated via HTTP Link header if available.
-}
readerToSource :: (MonadIO m) => ListReader i -> ConduitT () i m ()
readerToSource reader = go
  where
    go = do
        xs <- liftIO reader
        unless (null xs) $ yieldMany xs *> go

-- | Get list of entities with query parameter and stream it into Conduit pipe.  It automatically performs pagination.
streamListWithFilter
    :: (MonadIO m, WebexTeamsFilter filter, WebexTeamsListItem (ToResponse filter))
    => Authorization        -- ^ Authorization string against Webex Teams API.
    -> WebexTeamsRequest    -- ^ Predefined part of 'Request' commonly used for Webex Teams API.
    -> filter               -- ^ Filter criteria of the request.  Type of filter automatically determines
                            --   item type in response.
    -> ConduitT () (ToResponse filter) m ()
streamListWithFilter auth base param = getListWithFilter auth base param >>= readerToSource

-- | List of 'Team' and stream it into Conduit pipe.  It automatically performs pagination.
streamTeamList :: MonadIO m => Authorization -> WebexTeamsRequest -> ConduitT () Team m ()
streamTeamList auth base = getTeamList auth base >>= readerToSource

-- | Filter list of 'Organization' and stream it into Conduit pipe.  It automatically performs pagination.
streamOrganizationList :: MonadIO m => Authorization -> WebexTeamsRequest -> ConduitT () Organization m ()
streamOrganizationList auth base = getOrganizationList auth base >>= readerToSource

-- | List of 'Role' and stream it into Conduit pipe.  It automatically performs pagination.
streamRoleList :: MonadIO m => Authorization -> WebexTeamsRequest -> ConduitT () Role m ()
streamRoleList auth base = getRoleList auth base >>= readerToSource
