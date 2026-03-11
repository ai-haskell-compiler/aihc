module Data.Time.Clock.TAI.Compat (
    -- * TAI arithmetic
    AbsoluteTime,taiEpoch,addAbsoluteTime,diffAbsoluteTime,
    taiNominalDayStart,

    -- * leap-second map type
    LeapSecondMap,

    -- * conversion between UTC and TAI with map
    T.utcDayLength,T.utcToTAITime,T.taiToUTCTime,
    taiClock,
    ) where

import Data.Time.Orphans ()

import Data.Time.Compat
import Data.Time.Clock.TAI hiding (utcDayLength,utcToTAITime,taiToUTCTime)
import qualified Data.Time.Clock.TAI as T
