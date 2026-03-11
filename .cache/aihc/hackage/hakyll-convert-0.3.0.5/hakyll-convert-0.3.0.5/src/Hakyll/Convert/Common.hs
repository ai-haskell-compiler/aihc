{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Convert.Common where

import Data.Data
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data DistilledPost = DistilledPost
  { dpUri :: T.Text,
    dpBody :: Text,
    dpTitle :: Maybe Text,
    dpTags :: [Text],
    -- | Categories are coarser-grained than tags; you might be
    --   interested in ignoring tags and just focusing on categories
    --   in cases where you have lots of little uninteresting tags.
    dpCategories :: [Text],
    dpDate :: UTCTime
  }
  deriving (Data, Typeable)

instance Default DistilledPost where
  def =
    DistilledPost
      { dpUri = "",
        dpBody = "",
        dpTitle = Nothing,
        dpTags = [],
        dpCategories = [],
        dpDate = posixSecondsToUTCTime 0
      }
