{-# LANGUAGE FlexibleInstances, OverloadedStrings, UnicodeSyntax, CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.LinkSpec where

import           Test.Hspec
import           Test.QuickCheck
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid (mconcat)
#endif
import qualified Data.Text as T
import           Data.Maybe (fromJust)
import           Network.HTTP.Link
import           Network.URI (URI)

instance Arbitrary (Link URI) where
  arbitrary = do
    urlScheme ← elements ["http://", "https://", "ftp://", "git+ssh://"]
    urlDomain ← listOf1 $ elements ['a'..'z']
    urlTld ← elements ["com", "net", "org", "me", "is", "technology", "club"]
    urlPath ← listOf $ elements ['a'..'z']
    params ← listOf genParam
    return $ fromJust $ lnk (mconcat [urlScheme, urlDomain, ".", urlTld, "/", urlPath]) params
    where genParam = do
            otherParamKey ← suchThat (listOf1 $ elements ['a'..'z']) (\x → x /= "rel" && x /= "rev" && x /= "title"
              && x /= "title*" && x /= "hreflang" && x /= "anchor" && x /= "media" && x /= "type")
            paramKey ← elements [Rel, Rev, Title, Hreflang, Anchor, Media, ContentType, Other (T.pack otherParamKey)]
            paramValue ← listOf $ elements ['a'..'z']
            return (paramKey, T.pack paramValue)

spec ∷ Spec
spec = do
  describe "writeLinkHeader → parseLinkHeader" $
    it "roundtrips successfully" $
      property $ \x → parseLinkHeader (writeLinkHeader x) == Just (x :: [Link URI])
