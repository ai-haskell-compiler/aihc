{-# LANGUAGE
    OverloadedStrings
  , ExtendedDefaultRules
  #-}

module Web.Page.Types where

import Data.Default (Default (def))

-- | Generic layout for a web page. We keep the data type purely parametric to
-- allow for record-syntax overloading / reassignment, like this:
-- .
--
-- >  page :: WebPage (Html ()) T.Text
-- @ page' = page \{pageTitle = "foo!"\}@
--
data WebPage markup attr attrSet = WebPage
  { pageTitle           :: attr -- ^ Page title
  , favicon             :: markup -- ^ Favicon tags
  , metaVars            :: markup -- ^ @\<meta\>@ tags
  , initScripts         :: markup -- ^ JavaScript to include at the top of the page
  , beforeStylesScripts :: markup -- ^ JavaScript to include before @\<style\>@ tags
  , styles              :: markup -- ^ Styles
  , afterStylesScripts  :: markup -- ^ JavaScript to include after @\<style\>@ tags - ie: <http://modernizr.com Modernizr>
  , bodyScripts         :: markup -- ^ JavaScript to include at the base of @\<body\>@
  , bodyStyles          :: attrSet -- ^ Additional styles to assign to @\<body\>@
  } deriving (Show, Eq, Ord)


instance ( Semigroup m
         , Semigroup a
         , Semigroup s
         ) => Semigroup (WebPage m a s) where
  (WebPage t1 f1 m1 is1 bs1 s1 as1 b1 bss1) <> (WebPage t2 f2 m2 is2 bs2 s2 as2 b2 bss2) =
    WebPage (t1 <> t2)
            (f1 <> f2)
            (m1 <> m2)
            (is1 <> is2)
            (bs1 <> bs2)
            (s1 <> s2)
            (as1 <> as2)
            (b1 <> b2)
            (bss1 <> bss2)

instance ( Monoid m
         , Monoid a
         , Monoid s
         ) => Monoid (WebPage m a s) where
  mempty = WebPage mempty mempty mempty mempty mempty mempty mempty mempty mempty


instance ( Monoid m
         , Monoid a
         , Monoid s
         ) => Default (WebPage m a s) where
  def = mempty
