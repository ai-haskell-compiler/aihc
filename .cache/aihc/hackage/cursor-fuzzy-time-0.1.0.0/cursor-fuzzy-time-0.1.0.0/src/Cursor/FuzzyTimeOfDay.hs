{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyTimeOfDay
  ( FuzzyTimeOfDayCursor (..),
    emptyFuzzyTimeOfDayCursor,
    makeFuzzyTimeOfDayCursor,
    rebuildFuzzyTimeOfDayCursorForwards,
    rebuildFuzzyTimeOfDayCursorBackwards,
    fuzzyTimeOfDayCursorTextCursorL,
    fuzzyTimeOfDayCursorGuessForwards,
    fuzzyTimeOfDayCursorGuessBackwards,
  )
where

import Control.DeepSeq
import Cursor.Text
import Data.FuzzyTime
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Text.Megaparsec

data FuzzyTimeOfDayCursor = FuzzyTimeOfDayCursor
  { fuzzyTimeOfDayCursorTextCursor :: TextCursor,
    fuzzyTimeOfDayCursorBaseTimeOfDay :: TimeOfDay
  }
  deriving (Show, Eq, Generic)

instance Validity FuzzyTimeOfDayCursor

instance NFData FuzzyTimeOfDayCursor

emptyFuzzyTimeOfDayCursor :: TimeOfDay -> FuzzyTimeOfDayCursor
emptyFuzzyTimeOfDayCursor d =
  FuzzyTimeOfDayCursor
    { fuzzyTimeOfDayCursorTextCursor = emptyTextCursor,
      fuzzyTimeOfDayCursorBaseTimeOfDay = d
    }

makeFuzzyTimeOfDayCursor :: TimeOfDay -> FuzzyTimeOfDayCursor
makeFuzzyTimeOfDayCursor d =
  FuzzyTimeOfDayCursor
    { fuzzyTimeOfDayCursorTextCursor =
        fromJust $ makeTextCursor $ T.pack $ formatTime defaultTimeLocale "%T%Q" d,
      fuzzyTimeOfDayCursorBaseTimeOfDay = d
    }

rebuildFuzzyTimeOfDayCursorForwards :: FuzzyTimeOfDayCursor -> TimeOfDay
rebuildFuzzyTimeOfDayCursorForwards fdc@FuzzyTimeOfDayCursor {..} =
  fromMaybe fuzzyTimeOfDayCursorBaseTimeOfDay $ fuzzyTimeOfDayCursorGuessForwards fdc

rebuildFuzzyTimeOfDayCursorBackwards :: FuzzyTimeOfDayCursor -> TimeOfDay
rebuildFuzzyTimeOfDayCursorBackwards fdc@FuzzyTimeOfDayCursor {..} =
  fromMaybe fuzzyTimeOfDayCursorBaseTimeOfDay $ fuzzyTimeOfDayCursorGuessBackwards fdc

fuzzyTimeOfDayCursorTextCursorL :: Lens' FuzzyTimeOfDayCursor TextCursor
fuzzyTimeOfDayCursorTextCursorL =
  lens fuzzyTimeOfDayCursorTextCursor $ \fdc tc -> fdc {fuzzyTimeOfDayCursorTextCursor = tc}

fuzzyTimeOfDayCursorGuessForwards :: FuzzyTimeOfDayCursor -> Maybe TimeOfDay
fuzzyTimeOfDayCursorGuessForwards FuzzyTimeOfDayCursor {..} = do
  ftod <- parseMaybe fuzzyTimeOfDayP $ rebuildTextCursor fuzzyTimeOfDayCursorTextCursor
  resolveTimeOfDayForwards fuzzyTimeOfDayCursorBaseTimeOfDay ftod

fuzzyTimeOfDayCursorGuessBackwards :: FuzzyTimeOfDayCursor -> Maybe TimeOfDay
fuzzyTimeOfDayCursorGuessBackwards FuzzyTimeOfDayCursor {..} = do
  ftod <- parseMaybe fuzzyTimeOfDayP $ rebuildTextCursor fuzzyTimeOfDayCursorTextCursor
  resolveTimeOfDayBackwards fuzzyTimeOfDayCursorBaseTimeOfDay ftod
