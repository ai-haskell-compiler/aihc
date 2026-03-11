{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Basic missing time utilities. -}
module OM.Time (
  MonadTimeSpec(..),
  Time(..),
  timed,
  diffTimeSpec,
  addTime,
  NowT,
  runNowT,
) where


import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT), ask)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Binary (Binary(get, put))
import Data.Int (Int64)
import Data.Time (Day(ModifiedJulianDay), UTCTime(UTCTime), DiffTime)
import Prelude
  ( Applicative(pure), Fractional((/), fromRational), Monad(return)
  , Num((*), (+), (-)), Real(toRational), RealFrac(truncate), Show(showsPrec)
  , (.), Eq, Functor, IO, Ord, flip, realToFrac
  )
import System.Clock (TimeSpec)
import qualified System.Clock as Clock


{- | Wrapper around 'UTCTime', used mainly to provide a 'Binary' instance. -}
newtype Time = Time {
    unTime :: UTCTime
  }
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
instance Show Time where
  showsPrec n = showsPrec n . unTime
instance Binary Time where
  put (Time (UTCTime (ModifiedJulianDay day) tod)) =
    put (day, toRational tod)
  get = do
    (day, tod) <- get
    return (Time (UTCTime (ModifiedJulianDay day) (fromRational tod)))


{- | A monad that can produce the current time as a TimeSpec. -}
class (Monad m) => MonadTimeSpec m where
  getTime :: m TimeSpec

{- | The IO instances uses 'Clock.getTime' 'Clock.MonotonicCoarse'. -}
instance MonadTimeSpec IO where
  getTime = Clock.getTime Clock.MonotonicCoarse

instance {-# OVERLAPPABLE #-}
    ( Monad (t m)
    , MonadTimeSpec m
    , MonadTrans t
    )
  =>
    MonadTimeSpec (t m)
  where
    getTime = lift getTime


{- | Perform an action and measure how long it takes. -}
timed
  :: MonadTimeSpec m
  => m a
  -> m (a, DiffTime)
timed action = do
  start <- getTime
  result <- action
  end <- getTime
  pure (result, diffTimeSpec end start)


{- | Take the difference of two time specs, as a 'DiffTime'. -}
diffTimeSpec :: TimeSpec -> TimeSpec -> DiffTime
diffTimeSpec a b =
  realToFrac (Clock.toNanoSecs (Clock.diffTimeSpec a b)) / 1_000_000_000


{- | Add a 'DiffTime' to a 'TimeSpec'. -}
addTime :: DiffTime -> TimeSpec -> TimeSpec
addTime diff time =
  let
    rat = toRational diff

    secDiff :: Int64
    secDiff = truncate rat

    nsecDiff :: Int64
    nsecDiff = truncate ((toRational diff - toRational secDiff) * 1_000_000_000)
  in
    Clock.TimeSpec {
      Clock.sec = Clock.sec time + secDiff,
      Clock.nsec = Clock.nsec time + nsecDiff
    }


{-| An instance of MonadTimeSpec where the time is always a constant value.  -}
newtype NowT m a = NowT
  { _unNowT :: ReaderT TimeSpec m a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadTrans
    )
instance (Monad m) => MonadTimeSpec (NowT m) where
  getTime = NowT ask


runNowT :: TimeSpec -> NowT m a -> m a
runNowT now (NowT action) =
  flip runReaderT now action


