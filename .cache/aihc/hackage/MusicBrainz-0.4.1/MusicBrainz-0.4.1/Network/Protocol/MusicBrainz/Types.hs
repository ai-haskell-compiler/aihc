{-# LANGUAGE OverloadedStrings #-}

module Network.Protocol.MusicBrainz.Types (
    MBID(..)
  , Release(..)
  , TextRepresentation(..)
  , Medium(..)
  , Track(..)
  , Recording(..)
  , ArtistCredit(..)
  , Artist(..)
  , ReleaseGroup(..)
  , LabelInfo(..)
  , Label(..)
  , ReleaseEvent(..)
  , Area(..)
  , ISO3166Code(..)
  , CoverArtArchive(..)
) where

import Data.Text (Text)
import Data.Time.Calendar (Day)
import Data.Vector (Vector)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON(..), (.:), (.:?), Value(..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Format (parseTimeM)
import Data.Time.Locale.Compat (defaultTimeLocale)

newtype MBID = MBID { unMBID :: Text }
    deriving (Eq, Show)

data Release = Release {
    _releaseId :: MBID
  , _releaseTitle :: Text
  , _releaseStatus :: Maybe Text
  , _releaseQuality :: Maybe Text
  , _releasePackaging :: Maybe Text
  , _releaseTextRepresentation :: Maybe TextRepresentation
  , _releaseArtistCredit :: [ArtistCredit]
  , _releaseDate :: Maybe Day
  , _releaseCountry :: Maybe Text
  , _releaseEvents :: [ReleaseEvent]
  , _releaseBarcode :: Maybe Text
  , _releaseASIN :: Maybe Text
  , _releaseCoverArtArchive :: Maybe CoverArtArchive
  , _releaseMedia :: Vector Medium
} deriving (Eq, Show)

instance FromJSON Release where
    parseJSON (Object v) = Release <$>
                                 (MBID <$> v .: "id") <*>
                                 v .: "title" <*>
                                 v .:? "status" <*>
                                 v .:? "quality" <*>
                                 v .:? "packaging" <*>
                                 v .:? "text-representation" <*>
                                 v .: "artist-credit" <*>
                                 ((parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack =<<) <$> v .:? "date") <*>
                                 v .:? "country" <*>
                                 v .: "release-events" <*>
                                 v .:? "barcode" <*>
                                 v .:? "asin" <*>
                                 v .:? "cover-art-archive" <*>
                                 v .: "media"
    parseJSON _          = mzero

data TextRepresentation = TextRepresentation {
    _textRepLanguage :: Maybe Text
  , _textRepScript :: Maybe Text
} deriving (Eq, Show)

instance FromJSON TextRepresentation where
    parseJSON (Object v) = TextRepresentation <$>
                                 v .:? "language" <*>
                                 v .:? "script"
    parseJSON _          = mzero

data Medium = Medium {
    _mediumTitle :: Maybe Text
  , _mediumPosition :: Maybe Integer
  , _mediumFormat :: Maybe Text
  , _mediumTrackCount :: Integer
  , _mediumTrackOffset :: Maybe Integer
  , _mediumTrackList :: Maybe [Track]
} deriving (Eq, Show)

instance FromJSON Medium where
    parseJSON (Object v) = Medium <$>
                                 v .:? "title" <*>
                                 v .:? "position" <*>
                                 v .:? "format" <*>
                                 v .:  "track-count" <*>
                                 v .:? "track-offset" <*>
                                 v .:? "tracks"
    parseJSON _          = mzero

data Track = Track {
    _trackId :: MBID
  , _trackArtistCredit :: [ArtistCredit]
  , _trackPosition :: Maybe Integer
  , _trackNumber :: Maybe Text
  , _trackLength :: Maybe Integer
  , _trackRecording :: Recording
} deriving (Eq, Show)

instance FromJSON Track where
    parseJSON (Object v) = Track <$>
                                 (MBID <$> v .: "id") <*>
                                 v .: "artist-credit" <*>
                                 v .:? "position" <*>
                                 v .:? "number" <*>
                                 v .:? "length" <*>
                                 v .: "recording"
    parseJSON _          = mzero

data Recording = Recording {
    _recordingId :: MBID
  , _recordingTitle :: Maybe Text
  , _recordingLength :: Maybe Integer
  , _recordingArtistCredit :: [ArtistCredit]
} deriving (Eq, Show)

instance FromJSON Recording where
    parseJSON (Object v) = Recording <$>
                                 (MBID <$> v .: "id") <*>
                                 v .:? "title" <*>
                                 v .:? "length" <*>
                                 v .: "artist-credit"
    parseJSON _          = mzero

data ArtistCredit = ArtistCredit {
    _artistCreditArtist :: Artist
  , _artistCreditJoinPhrase :: Maybe Text
  , _artistCreditName :: Maybe Text
} deriving (Eq, Show)

instance FromJSON ArtistCredit where
    parseJSON (Object v) = ArtistCredit <$>
                                 v .: "artist" <*>
                                 v .:? "joinphrase" <*>
                                 v .:? "name"
    parseJSON _          = mzero

data Artist = Artist {
    _artistId :: MBID
  , _artistName :: Maybe Text
  , _artistSortName :: Maybe Text
  , _artistDisambiguation :: Maybe Text
} deriving (Eq, Show)

instance FromJSON Artist where
    parseJSON (Object v) = Artist <$>
                                 (MBID <$> v .: "id") <*>
                                 v .:? "name" <*>
                                 v .:? "sort-name" <*>
                                 v .:? "disambiguation"
    parseJSON _          = mzero

data ReleaseGroup = ReleaseGroup {
    _releaseGroupId :: MBID
  , _releaseGroupType :: Text
  , _releaseGroupTitle :: Maybe Text
  , _releaseGroupFirstReleaseDate :: Maybe Text
  , _releaseGroupPrimaryType :: Maybe Text
  , _releaseGroupArtistCredit :: [ArtistCredit]
} deriving (Eq, Show)

data LabelInfo = LabelInfo {
    _labelInfoCatalogNumber :: Maybe Text
  , _labelInfoLabel :: Label
} deriving (Eq, Show)

data Label = Label {
    _labelId :: MBID
  , _labelName :: Maybe Text
  , _labelSortName :: Maybe Text
  , _labelLabelCode :: Maybe Text
} deriving (Eq, Show)

data ReleaseEvent = ReleaseEvent {
    _releaseEventDate :: Maybe Day
  , _releaseEventArea :: Maybe Area
} deriving (Eq, Show)

instance FromJSON ReleaseEvent where
    parseJSON (Object v) = ReleaseEvent <$>
                                 ((parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack =<<) <$> v .:? "date") <*>
                                 v .:? "area"
    parseJSON _          = mzero

data Area = Area {
    _areaId :: MBID
  , _areaName :: Maybe Text
  , _areaSortName :: Maybe Text
  , _areaISO3166_1Codes :: [ISO3166Code]
  , _areaISO3166_2Codes :: [ISO3166Code]
  , _areaISO3166_3Codes :: [ISO3166Code]
} deriving (Eq, Show)

instance FromJSON Area where
    parseJSON (Object v) = Area <$>
                                 (MBID <$> v .: "id") <*>
                                 v .:? "name" <*>
                                 v .:? "sort-name" <*>
                                 (fromMaybe [] <$> v .:? "iso_3166_1_codes") <*>
                                 (fromMaybe [] <$> v .:? "iso_3166_2_codes") <*>
                                 (fromMaybe [] <$> v .:? "iso_3166_3_codes")
    parseJSON _          = mzero

data CoverArtArchive = CoverArtArchive {
    _coverArtArchiveArtwork :: Maybe Bool
  , _coverArtArchiveCount :: Maybe Integer
  , _coverArtArchiveFront :: Maybe Bool
  , _coverArtArchiveBack :: Maybe Bool
} deriving (Eq, Show)

instance FromJSON CoverArtArchive where
    parseJSON (Object v) = CoverArtArchive <$>
                                 v .:? "artwork" <*>
                                 v .:? "count" <*>
                                 v .:? "front" <*>
                                 v .:? "back"
    parseJSON _          = mzero

newtype ISO3166Code = ISO3166Code { unISO3166Code :: Text }
    deriving (Eq, Show)

instance FromJSON ISO3166Code where
    parseJSON t = ISO3166Code <$> parseJSON t
