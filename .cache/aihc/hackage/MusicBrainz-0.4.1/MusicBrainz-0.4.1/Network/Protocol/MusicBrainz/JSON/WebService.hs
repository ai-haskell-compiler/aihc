{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.Protocol.MusicBrainz.JSON.WebService (
    getRecordingById
  , getReleaseById
  , searchReleasesByArtistAndRelease
) where

import Network.Protocol.MusicBrainz.Types

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)

musicBrainzWSLookup :: MonadIO m => Text -> Text -> [Text] -> m BL.ByteString
musicBrainzWSLookup reqtype param incparams = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/" ++ T.unpack param ++ incs incparams ++ fj
    simpleHttp url
    where
        incs [] = ""
        incs xs = ("?inc="++) . intercalate "+" . map T.unpack $ xs
        fj = "&fmt=json"

musicBrainzWSSearch :: MonadIO m => Text -> Text -> Maybe Int -> Maybe Int -> m BL.ByteString
musicBrainzWSSearch reqtype query mlimit moffset = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/?query=" ++ urlEncode (T.unpack query) ++ limit mlimit ++ offset moffset ++ fj
    simpleHttp url
    where
        limit Nothing = ""
        limit (Just l) = "&limit=" ++ show l
        offset Nothing = ""
        offset (Just o) = "&offset=" ++ show o
        fj = "&fmt=json"

getRecordingById :: MonadIO m => MBID -> m (Either String Recording)
getRecordingById mbid = do
    lbs <- musicBrainzWSLookup "recording" (unMBID mbid) ["artist-credits"]
    return $ eitherDecode lbs

getReleaseById :: MonadIO m => MBID -> m (Either String Release)
getReleaseById mbid = do
    lbs <- musicBrainzWSLookup "release" (unMBID mbid) ["recordings", "artist-credits"]
    return $ eitherDecode lbs

searchReleasesByArtistAndRelease :: MonadIO m => Text -> Text -> Maybe Int -> Maybe Int -> m (Either String [(Int, Release)])
searchReleasesByArtistAndRelease artist release mlimit moffset = do
    lbs <- musicBrainzWSSearch "release" (T.concat ["artist:\"", artist, "\" AND release:\"", release, "\""]) mlimit moffset
    return $ eitherDecode lbs
