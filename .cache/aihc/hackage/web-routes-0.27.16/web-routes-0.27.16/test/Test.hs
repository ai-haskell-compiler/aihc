{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Data.Text (Text)
import Test.HUnit
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Web.Routes

newtype ArticleId = ArticleId Int deriving (Eq, Show, Num, PathInfo, Arbitrary)

data Sitemap
    = Home
    | Article ArticleId
    deriving (Eq, Show, Generic)

instance PathInfo Sitemap

instance Arbitrary Sitemap where
    arbitrary = oneof [return Home, fmap Article arbitrary]

prop_PathInfo_isomorphism :: Sitemap -> Bool
prop_PathInfo_isomorphism = pathInfoInverse_prop

case_toPathInfo :: Assertion
case_toPathInfo =
    do toPathInfo Home @?= "/home"
       toPathInfo (Article 0) @?= "/article/0"

case_toPathInfoParams :: Assertion
case_toPathInfoParams =
    do toPathInfoParams Home [("q",Just "1"),("r",Just "2")] @?= "/home?q=1&r=2"
       toPathInfoParams (Article 0) [("q",Just "1"),("r",Just "2")] @?= "/article/0?q=1&r=2"

case_fromPathInfo :: Assertion
case_fromPathInfo =
    do fromPathInfo "/home" @?= Right Home
       fromPathInfo "/article/0" @?= Right (Article 0)
       case fromPathInfo "/" :: Either String Sitemap of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

case_fromPathInfoParams :: Assertion
case_fromPathInfoParams =
    do fromPathInfoParams "/home?q=1&r=2" @?= Right (Home, [("q",Just "1"),("r",Just "2")])
       fromPathInfoParams "/article/0?q=1&r=2" @?= Right (Article 0, [("q",Just "1"),("r",Just "2")])
       case fromPathInfoParams "/?q=1&r=2" :: Either String (Sitemap, [(Text, Maybe Text)]) of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

main :: IO ()
main = hspec $ do
 prop "toPathInfo" case_toPathInfo
 prop "toPathInfoParams" case_toPathInfoParams
 prop "fromPathInfo" case_fromPathInfo
 prop "fromPathInfoParams" case_fromPathInfoParams
 prop "PathInfo_isomorphism" prop_PathInfo_isomorphism
