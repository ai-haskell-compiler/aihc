{-# LANGUAGE OverloadedStrings #-}

module Network.Protocol.MusicBrainz.Utils (
    nameCreditsToArtistName
  , nameCreditsToArtistSortName
) where

import Control.Applicative ((<|>), liftA2)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Network.Protocol.MusicBrainz.Types

nameCreditsToArtistName :: [ArtistCredit] -> Text
nameCreditsToArtistName = T.concat . map (nameSpan (_artistName . _artistCreditArtist))

nameCreditsToArtistSortName :: [ArtistCredit] -> Text
nameCreditsToArtistSortName = T.concat . map (nameSpan (liftA2 (<|>) _artistSortName _artistName . _artistCreditArtist))

nameSpan :: (ArtistCredit -> Maybe Text) -> ArtistCredit -> Text
nameSpan f = T.concat . catMaybes . liftA2 (:) f (return . _artistCreditJoinPhrase)
