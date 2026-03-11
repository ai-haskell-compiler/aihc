{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Cursor.FuzzyLocalTime
  ( FuzzyLocalTimeCursor (..),
    emptyFuzzyLocalTimeCursor,
    makeFuzzyLocalTimeCursor,
    rebuildFuzzyLocalTimeCursorForwards,
    rebuildFuzzyLocalTimeCursorBackwards,
    fuzzyLocalTimeCursorTextCursorL,
    fuzzyLocalTimeCursorGuessForwards,
    fuzzyLocalTimeCursorGuessBackwards,
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

data FuzzyLocalTimeCursor = FuzzyLocalTimeCursor
  { fuzzyLocalTimeCursorTextCursor :: TextCursor,
    fuzzyLocalTimeCursorBaseLocalTime :: LocalTime
  }
  deriving (Show, Eq, Generic)

instance Validity FuzzyLocalTimeCursor

instance NFData FuzzyLocalTimeCursor

emptyFuzzyLocalTimeCursor :: LocalTime -> FuzzyLocalTimeCursor
emptyFuzzyLocalTimeCursor d =
  FuzzyLocalTimeCursor
    { fuzzyLocalTimeCursorTextCursor = emptyTextCursor,
      fuzzyLocalTimeCursorBaseLocalTime = d
    }

makeFuzzyLocalTimeCursor :: AmbiguousLocalTime -> FuzzyLocalTimeCursor
makeFuzzyLocalTimeCursor alt =
  FuzzyLocalTimeCursor
    { fuzzyLocalTimeCursorTextCursor =
        fromJust $
          makeTextCursor $
            T.pack $
              case alt of
                OnlyDaySpecified d -> formatTime defaultTimeLocale "%F" d
                BothTimeAndDay lt -> formatTime defaultTimeLocale "%F %T%Q" lt,
      fuzzyLocalTimeCursorBaseLocalTime =
        case alt of
          OnlyDaySpecified d -> LocalTime d midnight
          BothTimeAndDay lt -> lt
    }

rebuildFuzzyLocalTimeCursorForwards :: FuzzyLocalTimeCursor -> AmbiguousLocalTime
rebuildFuzzyLocalTimeCursorForwards fdc@FuzzyLocalTimeCursor {..} =
  fromMaybe (BothTimeAndDay fuzzyLocalTimeCursorBaseLocalTime) $ fuzzyLocalTimeCursorGuessForwards fdc

rebuildFuzzyLocalTimeCursorBackwards :: FuzzyLocalTimeCursor -> AmbiguousLocalTime
rebuildFuzzyLocalTimeCursorBackwards fdc@FuzzyLocalTimeCursor {..} =
  fromMaybe (BothTimeAndDay fuzzyLocalTimeCursorBaseLocalTime) $ fuzzyLocalTimeCursorGuessBackwards fdc

fuzzyLocalTimeCursorTextCursorL :: Lens' FuzzyLocalTimeCursor TextCursor
fuzzyLocalTimeCursorTextCursorL =
  lens fuzzyLocalTimeCursorTextCursor $ \fdc tc -> fdc {fuzzyLocalTimeCursorTextCursor = tc}

fuzzyLocalTimeCursorGuessForwards :: FuzzyLocalTimeCursor -> Maybe AmbiguousLocalTime
fuzzyLocalTimeCursorGuessForwards FuzzyLocalTimeCursor {..} = do
  ftod <- parseMaybe fuzzyLocalTimeP $ rebuildTextCursor fuzzyLocalTimeCursorTextCursor
  resolveLocalTimeForwards fuzzyLocalTimeCursorBaseLocalTime ftod

fuzzyLocalTimeCursorGuessBackwards :: FuzzyLocalTimeCursor -> Maybe AmbiguousLocalTime
fuzzyLocalTimeCursorGuessBackwards FuzzyLocalTimeCursor {..} = do
  ftod <- parseMaybe fuzzyLocalTimeP $ rebuildTextCursor fuzzyLocalTimeCursorTextCursor
  resolveLocalTimeBackwards fuzzyLocalTimeCursorBaseLocalTime ftod
