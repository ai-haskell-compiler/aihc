{-# LANGUAGE UnicodeSyntax, Safe #-}

-- | The data type definitions for the HTTP Link header.
module Network.HTTP.Link.Types where

import           Data.Text (Text, pack, unpack)
import           Network.URI (URI, parseURIReference)

-- | The link attribute key.
data LinkParam = Rel | Anchor | Rev | Hreflang | Media | Title | Title' | ContentType | Other Text
  deriving (Eq, Show)

-- | A single link containing some representation of a URL.
data Link uri = Link uri [(LinkParam, Text)]
    deriving (Eq, Show)

-- | Types that can represent URLs.
--
-- For example, to parse links containing @Text.URI.URI@ from the
-- [modern-uri](https://hackage.haskell.org/package/modern-uri-0.3.2.0/docs/Text-URI.html#t:URI)
-- package, simply define the orphan instance:
--
-- @
--    instance IsURI Modern.URI where
--        uriFromText = left displayException . mkURI
--        uriToText = render
-- @
--
-- @since 1.1.0
class IsURI uri where
    uriFromText ∷ Text → Either String uri
    uriToText ∷ uri → Text

instance IsURI URI where
    uriFromText = maybe (Left "") Right . parseURIReference . unpack
    uriToText = pack . show

instance IsURI Text where
    uriFromText = Right
    uriToText = id

-- | Extracts the URI from the link.
href ∷ (IsURI uri) ⇒ Link uri → uri
href (Link h _) = h

-- | Extracts the parameters from the link.
linkParams ∷ Link uri → [(LinkParam, Text)]
linkParams (Link _ ps) = ps
