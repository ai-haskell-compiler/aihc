{-# LANGUAGE ExtendedDefaultRules, FlexibleInstances, GeneralizedNewtypeDeriving, OverloadedStrings, TemplateHaskell, DatatypeContexts #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.HUnit
import Test.QuickCheck
import Web.Routes
import Web.Routes.TH

newtype ArticleId = ArticleId Int deriving (Eq, Show, Num, PathInfo, Arbitrary)

-- the use of Show is and 'a' is to exercise the context generation of the 'derivePathInfo' code
data (Show a) => Sitemap a
    = Home a
    | Article ArticleId
    deriving (Eq, Show)

derivePathInfo ''Sitemap

instance Arbitrary (Sitemap Int) where
    arbitrary = oneof [fmap Home arbitrary, fmap Article arbitrary]

prop_PathInfo_isomorphism :: Sitemap Int -> Bool
prop_PathInfo_isomorphism = pathInfoInverse_prop

case_toPathInfo :: Assertion
case_toPathInfo =
    do toPathInfo ((Home 1) :: Sitemap Int) @?= "/home/1"
       toPathInfo ((Article 0):: Sitemap Int) @?= "/article/0"

case_fromPathInfo :: Assertion
case_fromPathInfo =
    do fromPathInfo "/home/1"      @?= ((Right (Home (1 :: Int))) :: Either String (Sitemap Int))
       fromPathInfo "/article/0" @?= Right (Article 0)
       case fromPathInfo "/" :: Either String (Sitemap Int) of
         Left _ -> return ()
         url    -> assertFailure $ "expected a Left, but got: " ++ show url

main :: IO ()
main = hspec $ do
 prop "toPathInfo" case_toPathInfo
 prop "fromPathInfo" case_fromPathInfo
 prop "PathInfo_isomorphism"  prop_PathInfo_isomorphism
