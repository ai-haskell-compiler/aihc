{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
import           Data.Time
import           Test.Tasty
import           Test.Tasty.HUnit        hiding ( assert )
import           Text.RawString.QQ              ( r )

import           Web.Sitemap.Gen

import qualified Data.ByteString               as BS


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup
    "sitemap.org Examples"
    [ testCase "Single Url Sitemap"   singleUrlSitemap
    , testCase "Multiple Url Sitemap" multipleUrlSitemap
    , testCase "Sitemap Index"        sitemapIndex
    ]


singleUrlSitemap :: Assertion
singleUrlSitemap =
    let
        rendered = renderSitemap $ Sitemap
            [ SitemapUrl
                  { sitemapLocation        = "http://www.example.com/"
                  , sitemapPriority        = Just 0.8
                  , sitemapChangeFrequency = Just Monthly
                  , sitemapLastModified    = Just $ UTCTime
                                                 (fromGregorian 2005 01 01)
                                                 0
                  }
            ]
    in  singleUrlSitemapFixture @=? rendered



singleUrlSitemapFixture :: BS.ByteString
singleUrlSitemapFixture =
    [r|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
><url
><loc
>http://www.example.com/</loc
><lastmod
>2005-01-01T00:00:00+00:00</lastmod
><changefreq
>monthly</changefreq
><priority
>0.8</priority
></url
></urlset
>|]


multipleUrlSitemap :: Assertion
multipleUrlSitemap =
    let
        urls =
            [ SitemapUrl "http://www.example.com/"
                         (Just $ UTCTime (fromGregorian 2005 01 01) 0)
                         (Just Monthly)
                         (Just 0.8)
            , SitemapUrl
                "http://www.example.com/catalog?item=12&desc=vacation_hawaii"
                Nothing
                (Just Weekly)
                Nothing
            , SitemapUrl
                "http://www.example.com/catalog?item=73&desc=vacation_new_zealand"
                (Just $ UTCTime (fromGregorian 2004 12 23) 0)
                (Just Weekly)
                Nothing
            , SitemapUrl
                "http://www.example.com/catalog?item=74&desc=vacation_newfoundland"
                (Just $ UTCTime (fromGregorian 2004 12 23) 64815)
                Nothing
                (Just 0.3)
            , SitemapUrl
                "http://www.example.com/catalog?item=83&desc=vacation_usa"
                (Just $ UTCTime (fromGregorian 2004 11 23) 0)
                Nothing
                Nothing
            ]
    in  multipleUrlSitemapFixture @=? renderSitemap (Sitemap urls)

multipleUrlSitemapFixture :: BS.ByteString
multipleUrlSitemapFixture =
    [r|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
><url
><loc
>http://www.example.com/</loc
><lastmod
>2005-01-01T00:00:00+00:00</lastmod
><changefreq
>monthly</changefreq
><priority
>0.8</priority
></url
><url
><loc
>http://www.example.com/catalog?item=12&amp;desc=vacation_hawaii</loc
><changefreq
>weekly</changefreq
></url
><url
><loc
>http://www.example.com/catalog?item=73&amp;desc=vacation_new_zealand</loc
><lastmod
>2004-12-23T00:00:00+00:00</lastmod
><changefreq
>weekly</changefreq
></url
><url
><loc
>http://www.example.com/catalog?item=74&amp;desc=vacation_newfoundland</loc
><lastmod
>2004-12-23T18:00:15+00:00</lastmod
><priority
>0.3</priority
></url
><url
><loc
>http://www.example.com/catalog?item=83&amp;desc=vacation_usa</loc
><lastmod
>2004-11-23T00:00:00+00:00</lastmod
></url
></urlset
>|]


sitemapIndex :: Assertion
sitemapIndex =
    let sitemaps =
                [ IndexEntry
                    "http://www.example.com/sitemap1.xml.gz"
                    ( Just
                    $ UTCTime (fromGregorian 2004 10 01)
                    $ 18
                    * 60
                    * 60
                    + 23
                    * 60
                    + 17
                    )
                , IndexEntry "http://www.example.com/sitemap2.xml.gz"
                             (Just $ UTCTime (fromGregorian 2005 01 01) 0)
                ]
    in  sitemapIndexFixture @=? renderSitemapIndex (SitemapIndex sitemaps)


sitemapIndexFixture :: BS.ByteString
sitemapIndexFixture =
    [r|<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<sitemapindex xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"
><sitemap
><loc
>http://www.example.com/sitemap1.xml.gz</loc
><lastmod
>2004-10-01T18:23:17+00:00</lastmod
></sitemap
><sitemap
><loc
>http://www.example.com/sitemap2.xml.gz</loc
><lastmod
>2005-01-01T00:00:00+00:00</lastmod
></sitemap
></sitemapindex
>|]
