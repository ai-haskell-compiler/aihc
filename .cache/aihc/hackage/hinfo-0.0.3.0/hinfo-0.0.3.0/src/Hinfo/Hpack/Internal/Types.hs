{-# language OverloadedStrings #-}
module Hinfo.Hpack.Internal.Types where


import qualified Data.Text    as T
import qualified Data.Yaml    as Y
import Data.Aeson             (FromJSON(..), Value, (.:), (.:?), withObject)

data PackageFile = PackageFile {
    packageName                :: T.Text
  , packageVersion             :: T.Text
  , packageGithub              :: Maybe T.Text
  , packageLicense             :: Maybe T.Text
  , packageAuthor              :: Maybe T.Text
  , packageMaintainer          :: Maybe T.Text
  , packageCopyright           :: Maybe T.Text
  , packageExtraSourceFiles    :: Maybe [T.Text]
  , packageSynopsis            :: Maybe T.Text
  , packageCategory            :: Maybe T.Text
  , packageDescription         :: Maybe T.Text
  , packageDependencies        :: [T.Text]
  , packageExecutables         :: Maybe Value
  , packageTests               :: Maybe Value
} deriving (Show, Eq)


instance FromJSON PackageFile where
  parseJSON = withObject "PackageFile" $ \v -> PackageFile
    <$> v .: "name"
    <*> v .: "version"
    <*> v .:? "github"
    <*> v .:? "license"
    <*> v .:? "author"
    <*> v .:? "maintainer"
    <*> v .:? "copyright"
    <*> v .:? "extra-source-files"
    <*> v .:? "synopsis"
    <*> v .:? "category"
    <*> v .:? "description"
    <*> v .: "dependencies"
    <*> v .:? "executables"
    <*> v .:? "tests"