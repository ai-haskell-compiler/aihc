{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Yesod.Page.RenderedRoute
  ( RenderedRoute
  , renderedRouteLink
  , getRenderedRoute
  , getRenderedRouteAbsolute
  , updateQueryParameter
  )
where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, intercalate, pack, unpack)
import Network.HTTP.Link.Compat
import Network.URI
  (URI(..), escapeURIString, isUnescapedInURIComponent, parseAbsoluteURI)
import UnliftIO (throwString)
import Yesod.Core
  ( HandlerSite
  , MonadHandler
  , RenderRoute
  , Yesod
  , approot
  , getApprootText
  , getCurrentRoute
  , getRequest
  , getYesod
  , renderRoute
  , reqGetParams
  , waiRequest
  )

-- | Information about a relative Route with query string
data RenderedRoute = RenderedRoute
  { renderedApproot :: Text
  , renderedRoutePath :: [Text]
  , renderedRouteQuery :: [(Text, Text)]
  }

instance ToJSON RenderedRoute where
  toJSON = String . pack . show . renderedRouteURI

-- | Convert a @'RenderedRoute'@ into a @'Link'@ with the given @'Rel'@
renderedRouteLink :: Text -> RenderedRoute -> Link
renderedRouteLink rel = flip linkURI [(Rel, rel)] . renderedRouteURI

-- | Convert a @'RenderedRoute'@ into a (relative) @'URI'@
renderedRouteURI :: RenderedRoute -> URI
renderedRouteURI RenderedRoute {..} = URI
  { uriScheme = scheme
  , uriAuthority = authority
  , uriPath = unpack $ "/" <> intercalate "/" renderedRoutePath
  , uriQuery = unpack $ query renderedRouteQuery
  , uriFragment = ""
  }
 where
  query [] = ""
  query qs = "?" <> intercalate "&" (parts qs)
  parts = map $ \(k, v) -> k <> "=" <> escape v
  escape = pack . escapeURIString isUnescapedInURIComponent . unpack

  (scheme, authority) = fromMaybe ("", Nothing) $ do
    uri <- parseAbsoluteURI $ unpack renderedApproot
    pure (uriScheme uri, uriAuthority uri)

-- | Get the current route as a @'RenderedRoute'@
getRenderedRoute
  :: (MonadHandler m, RenderRoute (HandlerSite m)) => m RenderedRoute
getRenderedRoute = do
  route <- maybe (throwString "no route") pure =<< getCurrentRoute

  -- When I just use _query, it's always empty. Why would renderRoute return
  -- this tuple if the Route value (apparently) never has the query information?
  let (path, _query) = renderRoute route
  query <- reqGetParams <$> getRequest

  pure $ RenderedRoute
    { renderedApproot = ""
    , renderedRoutePath = path
    , renderedRouteQuery = query
    }

getRenderedRouteAbsolute
  :: (MonadHandler m, Yesod (HandlerSite m), RenderRoute (HandlerSite m))
  => m RenderedRoute
getRenderedRouteAbsolute = do
  route <- maybe (throwString "no route") pure =<< getCurrentRoute

  -- When I just use _query, it's always empty. Why would renderRoute return
  -- this tuple if the Route value (apparently) never has the query information?
  let (path, _query) = renderRoute route
  query <- reqGetParams <$> getRequest
  root <- getApprootText approot <$> getYesod <*> waiRequest

  pure $ RenderedRoute
    { renderedApproot = root
    , renderedRoutePath = path
    , renderedRouteQuery = query
    }

-- | Update a single query parameter and preserve the rest
--
-- If given @'Nothing'@, the parameter is removed.
--
updateQueryParameter :: Text -> Maybe Text -> RenderedRoute -> RenderedRoute
updateQueryParameter name = overQuery . asMap . updateKey name

-- Lens? meh
overQuery
  :: ([(Text, Text)] -> [(Text, Text)]) -> RenderedRoute -> RenderedRoute
overQuery f renderedRoute =
  renderedRoute { renderedRouteQuery = f $ renderedRouteQuery renderedRoute }

asMap :: Ord k => (Map k v -> Map k v) -> [(k, v)] -> [(k, v)]
asMap f = Map.toList . f . Map.fromList

updateKey :: Ord k => k -> Maybe v -> Map k v -> Map k v
updateKey k = maybe (Map.delete k) $ Map.insert k
