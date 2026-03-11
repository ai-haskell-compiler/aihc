module Text.Time.Pretty
  ( prettyTimeAutoFromNow,
    prettyTimeAuto,
    prettyDayAutoFromNow,
    prettyDayAuto,
    -- Helper functions
    timeAgo,
    timeAgoToDiffTime,
    daysAgo,
    daysAgoToDays,

    -- * Helper Types
    TimeAgo (..),
    DaysAgo (..),

    -- * Rendering
    renderDaysAgoAuto,
    renderTimeAgoAuto,

    -- * Constants
    picoSecondsPerSecond,
    secondsPerMinute,
    minutesPerHour,
    hoursPerDay,
    daysPerWeek,
  )
where

import Data.Time
import Text.Time.Pretty.Constants
import Text.Time.Pretty.Render
import Text.Time.Pretty.TimeAgo

prettyTimeAutoFromNow :: UTCTime -> IO String
prettyTimeAutoFromNow before = do
  now <- getCurrentTime
  pure $ prettyTimeAuto now before

prettyTimeAuto :: UTCTime -> UTCTime -> String
prettyTimeAuto now before = renderTimeAgoAuto $ timeAgo $ diffUTCTime now before

prettyDayAutoFromNow :: Day -> IO String
prettyDayAutoFromNow before = do
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime
  pure $ prettyDayAuto today before

prettyDayAuto :: Day -> Day -> String
prettyDayAuto today before = renderDaysAgoAuto $ daysAgo $ diffDays today before
