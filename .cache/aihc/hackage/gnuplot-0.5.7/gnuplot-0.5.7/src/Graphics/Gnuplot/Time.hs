module Graphics.Gnuplot.Time where

import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale, )
import Data.Tuple.HT (mapFst, )

{- |
Use it this way:

> import Data.Time
> import Graphics.Gnuplot.Simple
>
> main =
>    plotPath [XTime, XFormat "%m-%d"] $ prepXTime $
>       (UTCTime (fromGregorian 2008 01 01)     0, 1.0) :
>       (UTCTime (fromGregorian 2008 01 05) 43200, 5.0) :
>       (UTCTime (fromGregorian 2008 01 15)     0, 2.5) :
>       []
-}

prepXTime :: (FormatTime a, Read b) => [(a, b)] -> [(b, b)]
prepXTime = map (mapFst (read . formatTime defaultTimeLocale "%s"))
