module System.Path.ModificationTime where

import Data.Time (UTCTime)

convertTime :: UTCTime -> UTCTime
convertTime = id
