{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyDay
  ( FuzzyDayCursor (..),
    emptyFuzzyDayCursor,
    makeFuzzyDayCursor,
    rebuildFuzzyDayCursorForwards,
    rebuildFuzzyDayCursorBackwards,
    fuzzyDayCursorTextCursorL,
    fuzzyDayCursorGuessForwards,
    fuzzyDayCursorGuessBackwards,
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

data FuzzyDayCursor = FuzzyDayCursor
  { fuzzyDayCursorTextCursor :: TextCursor,
    fuzzyDayCursorBaseDay :: Day
  }
  deriving (Show, Eq, Generic)

instance Validity FuzzyDayCursor

instance NFData FuzzyDayCursor

emptyFuzzyDayCursor :: Day -> FuzzyDayCursor
emptyFuzzyDayCursor d =
  FuzzyDayCursor
    { fuzzyDayCursorTextCursor = emptyTextCursor,
      fuzzyDayCursorBaseDay = d
    }

makeFuzzyDayCursor :: Day -> FuzzyDayCursor
makeFuzzyDayCursor d =
  FuzzyDayCursor
    { fuzzyDayCursorTextCursor =
        fromJust $
          makeTextCursor $
            T.pack $
              formatTime defaultTimeLocale "%F" d,
      fuzzyDayCursorBaseDay = d
    }

rebuildFuzzyDayCursorForwards :: FuzzyDayCursor -> Day
rebuildFuzzyDayCursorForwards fdc@FuzzyDayCursor {..} =
  fromMaybe fuzzyDayCursorBaseDay $ fuzzyDayCursorGuessForwards fdc

rebuildFuzzyDayCursorBackwards :: FuzzyDayCursor -> Day
rebuildFuzzyDayCursorBackwards fdc@FuzzyDayCursor {..} =
  fromMaybe fuzzyDayCursorBaseDay $ fuzzyDayCursorGuessBackwards fdc

fuzzyDayCursorTextCursorL :: Lens' FuzzyDayCursor TextCursor
fuzzyDayCursorTextCursorL =
  lens fuzzyDayCursorTextCursor $ \fdc tc ->
    fdc {fuzzyDayCursorTextCursor = tc}

fuzzyDayCursorGuessForwards :: FuzzyDayCursor -> Maybe Day
fuzzyDayCursorGuessForwards FuzzyDayCursor {..} = do
  fd <- parseMaybe fuzzyDayP $ rebuildTextCursor fuzzyDayCursorTextCursor
  resolveDayForwards fuzzyDayCursorBaseDay fd

fuzzyDayCursorGuessBackwards :: FuzzyDayCursor -> Maybe Day
fuzzyDayCursorGuessBackwards FuzzyDayCursor {..} = do
  fd <- parseMaybe fuzzyDayP $ rebuildTextCursor fuzzyDayCursorTextCursor
  resolveDayBackwards fuzzyDayCursorBaseDay fd
