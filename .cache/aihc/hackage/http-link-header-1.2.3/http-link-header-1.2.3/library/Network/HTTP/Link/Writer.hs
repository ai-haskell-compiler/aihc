{-# LANGUAGE OverloadedStrings, UnicodeSyntax, Safe, CPP #-}

module Network.HTTP.Link.Writer (
  writeLink
, writeLinkHeader
) where

import           Data.Text hiding (map)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid (mconcat)
#endif
import           Network.URI (escapeURIString)
import           Network.HTTP.Link.Types

writeParamKey ∷ LinkParam → Text
writeParamKey Rel = "rel"
writeParamKey Anchor = "anchor"
writeParamKey Rev = "rev"
writeParamKey Hreflang = "hreflang"
writeParamKey Media = "media"
writeParamKey Title = "title"
writeParamKey Title' = "title*"
writeParamKey ContentType = "type"
writeParamKey (Other t) = t

writeParam ∷ (LinkParam, Text) → Text
writeParam (t, v) = mconcat ["; ", writeParamKey t, "=\"", escPar v, "\""]
  where escPar = pack . escapeURIString (/= '"') . unpack
        -- maybe URI escaping is not what we should do here? eh, whatever

writeLink ∷ (IsURI uri) ⇒ Link uri → Text
writeLink (Link u ps) = mconcat $ ["<", uriToText u, ">"] ++ map writeParam ps

writeLinkHeader ∷ (IsURI uri) ⇒ [Link uri] → Text
writeLinkHeader = intercalate ", " . map writeLink
