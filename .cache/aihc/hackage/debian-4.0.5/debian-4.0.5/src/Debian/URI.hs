{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings, PackageImports, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Debian.URI
    ( module Network.URI

#if 0
    , _NodeElement -- :: Prism' Node Element
    , _NodeContent -- :: Prism' Node Text
    , eltAttrsLens -- :: Lens' Element (HashMap AttrName AttrValue)
    , eltChildrenLens --  :: Lens' Element [Node]
    , eltNameLens -- :: Lens' Element Text
#endif

    , URIError(..)
    , uriSchemeLens
    , uriAuthorityLens
    , uriPathLens
    , uriQueryLens
    , uriFragmentLens
    -- * String known to parsable by parseURIReference.  Mainly
    -- useful because it has a Read instance.
    , URI'(..)
    , fromURI'
    , toURI'
    , readURI'

    -- Show URI as a Haskell expression
    , showURI
    -- Monadic URI parsers
    , parseURIReference'
    , parseURI'
    , parseAbsoluteURI'
    , parseRelativeReference'
    , parseURIUnsafe
    -- URI appending
    , appendURI
    , appendURIs
    , parentURI
    , uriToString'
    -- * Lift IO operations into a MonadError instance
    , HasParseError(fromParseError)
    , HasURIError(fromURIError)
    -- * QuickCheck properties
    , prop_print_parse
    , prop_append_singleton
    ) where

import Control.Lens (makeLensesFor)
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable (foldrM)
import Data.Maybe (fromJust, fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Network.URI (nullURI, parseURIReference, parseURI, parseAbsoluteURI, parseRelativeReference, URI(..), URIAuth(..), uriToString)
import System.FilePath ((</>), dropTrailingPathSeparator, takeDirectory)
import Text.Parsec (ParseError)

$(makeLensesFor [("uriScheme", "uriSchemeLens"),
                 ("uriAuthority", "uriAuthorityLens"),
                 ("uriPath", "uriPathLens"),
                 ("uriQuery", "uriQueryLens"),
                 ("uriFragment", "uriFragmentLens")] ''URI)

showURI :: URI -> String
showURI (URI {..}) =
    "URI {uriScheme = " <> show uriScheme <>
       ", uriAuthority = " <> show uriAuthority <>
       ", uriPath = " <> show uriPath <>
       ", uriQuery = " <> show uriQuery <>
       ", uriFragment = " <> show uriFragment <> "}"

-- | parseURI with MonadError
parseURI' :: (HasURIError e, MonadError e m) => String -> m URI
parseURI' s = maybe (throwError $ fromURIError $ URIParseError "parseURI" s) return (parseURI s)
parseURIReference' :: (HasURIError e, MonadError e m) => String -> m URI
parseURIReference' s = maybe (throwError $ fromURIError $ URIParseError "parseURIReference" s) return (parseURIReference s)
parseAbsoluteURI' :: (HasURIError e, MonadError e m) => String -> m URI
parseAbsoluteURI' s = maybe (throwError $ fromURIError $ URIParseError "parseAbsoluteURI" s) return (parseAbsoluteURI s)
parseRelativeReference' :: (HasURIError e, MonadError e m) => String -> m URI
parseRelativeReference' s = maybe (throwError $ fromURIError $ URIParseError "parseRelativeReference" s) return (parseRelativeReference s)

parseURIUnsafe :: String -> URI
parseURIUnsafe s = fromMaybe (error ("parseURIUnsafe " ++ show s)) $ parseURIReference s

--parseAbsoluteURI :: String -> Maybe URI
--parseRelativeReference :: String -> Maybe URI
--parseURI :: String -> Maybe URI
--parseURIReference :: String -> Maybe URI

data URIError =
    URIParseError String String
  | URIAppendError URI URI
  deriving (Eq, Ord, Show)

-- | Conservative appending of absolute and relative URIs.  There may
-- be other cases that can be implemented, lets see if they turn up.
appendURI :: MonadError URIError m => URI -> URI -> m URI
    -- Append the two paths
appendURI (URI scheme auth path1 "" "") (URI "" Nothing path2 query fragment) = return $ URI scheme auth (path1 </> path2) query fragment
    -- Use query from RHS
appendURI a b = throwError (URIAppendError a b)

-- | Append a list of URI
-- @@
-- λ> appendURIs (parseURI "http://host.com") (parseURIRelative "/bar")
appendURIs :: (Foldable t, MonadError URIError m) => t URI -> m URI
appendURIs uris = foldrM appendURI nullURI uris

parentURI :: URI -> URI
parentURI uri = uri {uriPath = takeDirectory (dropTrailingPathSeparator (uriPath uri))}

-- properties
-- appendURIs [x] == x

prop_append_singleton :: URI -> Bool
prop_append_singleton uri = appendURIs [uri] == Right uri

prop_print_parse :: URI -> Bool
prop_print_parse uri = parseURIReference (show uri) == Just uri

-- | A wrapper around a String containing a known parsable URI.  Not
-- absolutely safe, because you could say read "URI' \"bogus string\""
-- :: URI'.  But enough to save me from myself.
newtype URI' = URI' String deriving (Read, Show, Eq, Ord)

readURI' :: String -> Maybe URI'
readURI' s = maybe Nothing (const (Just (URI' s))) (parseURIReference s)

fromURI' :: URI' -> URI
fromURI' (URI' s) = fromJust (parseURI s) -- this should provably parse

-- | Using the bogus Show instance of URI here.  If it ever gets fixed
-- this will stop working.  Worth noting that show will obscure any
-- password info embedded in the URI, so that's nice.
toURI' :: URI -> URI'
toURI' = URI' . show

uriToString' :: URI -> String
uriToString' uri = uriToString id uri ""

class HasParseError e where fromParseError :: ParseError -> e
instance HasParseError ParseError where fromParseError = id

class HasURIError e where fromURIError :: URIError -> e
instance HasURIError URIError where fromURIError = id

instance Ord ParseError where
    compare a b = compare (show a) (show b)
