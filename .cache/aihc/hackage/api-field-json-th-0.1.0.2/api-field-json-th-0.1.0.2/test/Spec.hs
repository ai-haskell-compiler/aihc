{-# LANGUAGE TemplateHaskell, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, FlexibleInstances #-}
import Data.Aeson.APIFieldJsonTH
import Test.HUnit
import Data.Aeson
import Control.Lens

data SomeQuery = SomeQuery {
    _someQueryPage :: Int
    , _someQueryText :: String
} deriving (Eq, Show)
makeFields ''SomeQuery
deriveApiFieldJSON ''SomeQuery

fromJson = "fromJson example" ~: dec ~?= (Just $ SomeQuery 1 "hoge")
  where dec = decode "{\"page\": 1, \"text\": \"hoge\"}"

lensExample = "lens makeFields example" ~: (r ^. page) ~?= 3
  where r = SomeQuery 3 "hoge"

-- main :: IO ()
main = runTestTT $ TestList [fromJson, lensExample]
