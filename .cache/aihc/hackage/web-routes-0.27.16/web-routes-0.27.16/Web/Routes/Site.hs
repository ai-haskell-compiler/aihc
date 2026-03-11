module Web.Routes.Site where

import Data.ByteString
import Data.Monoid
import Data.Text (Text)
import Web.Routes.Base (decodePathInfo, encodePathInfo)

{-|

A site groups together the three functions necesary to make an application:

* A function to convert from the URL type to path segments.

* A function to convert from path segments to the URL, if possible.

* A function to return the application for a given URL.

There are two type parameters for Site: the first is the URL datatype, the
second is the application datatype. The application datatype will depend upon
your server backend.
-}
data Site url a
    = Site {
           {-|
               Return the appropriate application for a given URL.

               The first argument is a function which will give an appropriate
               URL (as a String) for a URL datatype. This is usually
               constructed by a combination of 'formatPathSegments' and the
               prepending of an absolute application root.

               Well behaving applications should use this function to
               generating all internal URLs.
           -}
             handleSite         :: (url -> [(Text, Maybe Text)] -> Text) -> url -> a
           -- | This function must be the inverse of 'parsePathSegments'.
           , formatPathSegments :: url -> ([Text], [(Text, Maybe Text)])
           -- | This function must be the inverse of 'formatPathSegments'.
           , parsePathSegments  :: [Text] -> Either String url
           }

-- | Override the \"default\" URL, ie the result of 'parsePathSegments' [].
setDefault :: url -> Site url a -> Site url a
setDefault defUrl (Site handle format parse) =
    Site handle format parse'
  where
    parse' [] = Right defUrl
    parse' x = parse x

instance Functor (Site url) where
  fmap f site = site { handleSite = \showFn u -> f (handleSite site showFn u) }

-- | Retrieve the application to handle a given request.
--
-- NOTE: use 'decodePathInfo' to convert a 'ByteString' url to a properly decoded list of path segments
runSite :: Text -- ^ application root, with trailing slash
        -> Site url a
        -> [Text] -- ^ path info, (call 'decodePathInfo' on path with leading slash stripped)
        -> (Either String a)
runSite approot site pathInfo =
    case parsePathSegments site pathInfo of
        (Left errs) -> (Left errs)
        (Right url) -> Right $ handleSite site showFn url
  where
    showFn url qs =
        let (pieces, qs') = formatPathSegments site url
        in approot `mappend` (encodePathInfo pieces (qs ++ qs'))
