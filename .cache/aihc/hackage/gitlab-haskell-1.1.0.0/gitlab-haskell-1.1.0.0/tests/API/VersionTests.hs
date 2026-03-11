{-# LANGUAGE FlexibleInstances #-}

module API.VersionTests (versionTests) where

import API.Common
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import GitLab
import Test.Tasty

-- | https://docs.gitlab.com/ee/api/version.html
versionTests :: [TestTree]
versionTests =
  let fname = "data/api/version/version.json"
   in gitlabJsonParserTests
        "version"
        fname
        (parseOne =<< BSL.readFile fname :: IO Version)
        ( do
            decodedFile <- parseOne =<< BSL.readFile fname :: IO Version
            parseOne (encode decodedFile) :: IO Version
        )
