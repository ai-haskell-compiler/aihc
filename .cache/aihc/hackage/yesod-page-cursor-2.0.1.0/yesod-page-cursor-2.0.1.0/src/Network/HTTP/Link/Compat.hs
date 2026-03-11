{-# LANGUAGE CPP #-}
module Network.HTTP.Link.Compat
    ( Link
    , linkURI
    , parseLinkURI
    , module Network.HTTP.Link
    )
where

import Data.Text (Text)
import Network.HTTP.Link hiding (Link)
import qualified Network.HTTP.Link as HTTP
import Network.URI (URI)

#if MIN_VERSION_http_link_header(1,2,0)
type Link = HTTP.Link URI
#else
type Link = HTTP.Link
#endif

linkURI :: URI -> [(LinkParam, Text)] -> Link
linkURI = HTTP.Link

parseLinkURI :: Text -> Either String [Link]
parseLinkURI = parseLinkHeader'
