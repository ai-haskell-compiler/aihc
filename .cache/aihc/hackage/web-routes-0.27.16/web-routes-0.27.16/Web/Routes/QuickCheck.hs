module Web.Routes.QuickCheck where

import qualified Data.Text.Encoding as Text
import Web.Routes.PathInfo (PathInfo, toPathInfo, fromPathInfo)

-- | test that a 'PathInfo' instance is valid
--
-- Generates 'Arbitrary' 'url' values and checks that:
--
--    fromPathInfo . toPathInfo == id
--
pathInfoInverse_prop :: (Eq url, PathInfo url) => url -> Bool
pathInfoInverse_prop url =
    case (fromPathInfo $ Text.encodeUtf8 $ toPathInfo url) of
      Right url' -> url == url'
      _ -> False
