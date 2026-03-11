{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.WebDriverWrapper.ChromeDriver (getChromeDriverIfNeeded) where

import Test.WebDriverWrapper.Constants (chromeDriverPath, downloadPath, chromeDriverVersionsUrl, chromeDriverArchIndex, chromeDriverArchivePath, chromeDriverArchiveDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing, copyFile, removeDirectoryRecursive, removeFile)
import Control.Monad (unless)
import Test.WebDriverWrapper.Helpers (download, decompressZip, evalUntillSuccess)
import Network.HTTP.Simple (parseRequest, setRequestMethod, httpLBS)
import Network.HTTP.Client.Conduit (Response(responseBody))
import Data.Aeson (eitherDecode)
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.Process (readProcess)
import Data.Maybe (maybeToList)

-- | Checks if @chromedriver@ is in the `downloadPath`. If not, download it. 
getChromeDriverIfNeeded :: Maybe FilePath -> IO()
getChromeDriverIfNeeded browserBinary = do
    chromeDriverPath' <- chromeDriverPath
    hasChromeDriver   <- doesFileExist chromeDriverPath'
    unless hasChromeDriver $ getChromeDriver browserBinary

getChromeDriver :: Maybe FilePath  -> IO()
getChromeDriver browserBinary = do
    dPath   <- downloadPath
    chromeVersion <- getChromeVersion browserBinary

    url <- getChromeDriverDownloadUrl $ T.pack chromeVersion
    chromeDriverArchivePath' <- chromeDriverArchivePath
    chromeDriverArchiveDirectory' <- chromeDriverArchiveDirectory
    chromeDriverPath' <- chromeDriverPath

    createDirectoryIfMissing True dPath

    download url chromeDriverArchivePath'
    decompressZip chromeDriverArchivePath' dPath
    copyFile (chromeDriverArchiveDirectory' </> "chromedriver") chromeDriverPath'
    
    removeDirectoryRecursive chromeDriverArchiveDirectory'
    removeFile chromeDriverArchivePath'


getChromeDriverDownloadUrl :: T.Text -> IO String
getChromeDriverDownloadUrl chromeVersion = do
    requestUrl <- parseRequest chromeDriverVersionsUrl
    let
        request
            = setRequestMethod "GET"
            requestUrl
    response <- responseBody <$> httpLBS request
    let
        decodedResponse = eitherDecode response :: Either String ChromeDriverMain

        allVersions = case decodedResponse of
            (Left err)        -> error err
            (Right versions') ->  versions  versions'
        
        versionIndex = V.findIndexR ((==chromeVersion).version) allVersions
        versionDownloads = downloads . (allVersions V.!) <$> versionIndex

        maybeLastVersionUrl  =  do
            versionDownloads' <- versionDownloads
            chromedriver <- AKM.lookup "chromedriver" versionDownloads'
            platform <- chromedriver V.!? chromeDriverArchIndex
            AKM.lookup "url" platform

        url = case maybeLastVersionUrl of
            Nothing     -> error "Couldn't get chromedriver url!"
            (Just url') -> T.unpack url'
    return url

getChromeVersion :: Maybe FilePath -> IO String
getChromeVersion executableNames = do
    let candidates = maybeToList executableNames ++ ["google-chrome"] -- defaults to google-chrome in PATH's version. 
    terminalOutput <- evalUntillSuccess $ readVersion <$> candidates
    return $ last $ words terminalOutput
    where
        readVersion exec = readProcess exec ["--version"] ""

-- To help out Aeson in parsing the JSON.
data ChromeDriverMain = ChromeDriverMain{
    timestamp :: T.Text,
    versions :: V.Vector ChromeDriverVersion
}
  deriving (Show, Generic, A.ToJSON, A.FromJSON)

data ChromeDriverVersion = ChromeDriverVersion{
    version :: T.Text,
    revision :: T.Text,
    downloads :: AKM.KeyMap (V.Vector (AKM.KeyMap T.Text))
}
  deriving (Show, Generic, A.ToJSON, A.FromJSON)
