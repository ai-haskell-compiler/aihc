{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Data.Time.Orphans () where

import Data.Orphans ()

import Control.DeepSeq (NFData (..))
import Data.Ix (Ix)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.TAI
import Data.Time.Format
import Data.Hashable (Hashable (..))

import Data.Time.Format (TimeLocale (..))
import Data.Time.Clock.System

#if !MIN_VERSION_time(1,11,0)
import Data.Fixed (Pico)
import Text.Read (Read (..))
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
#endif

#if MIN_VERSION_time(1,11,0)
import Data.Ix (Ix (..))
import Data.Time.Calendar.Month
import Data.Time.Calendar.Quarter
import Data.Time.Calendar.WeekDate
#endif

#if !MIN_VERSION_time(1,15,0)
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH
import Data.Fixed (Fixed (..), Pico)
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,11,0)
deriving instance Ord DayOfWeek
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,10,0)
deriving instance Data DayOfWeek
#endif

#if MIN_VERSION_time(1,8,0) && !MIN_VERSION_time(1,10,0)
deriving instance Data SystemTime
#endif

#if MIN_VERSION_time(1,9,0) && !MIN_VERSION_time(1,11,1)
instance NFData DayOfWeek where
    rnf !_ = ()

instance NFData CalendarDiffTime where
    rnf (CalendarDiffTime x y) = rnf x `seq` rnf y

instance NFData CalendarDiffDays where
    rnf (CalendarDiffDays x y) = rnf x `seq` rnf y

deriving instance Ix DayOfWeek
#endif

#if !MIN_VERSION_time(1,11,0)

instance Read DiffTime where
    readPrec = do
        t <- readPrec :: ReadPrec Pico
        _ <- lift $ char 's'
        return $ realToFrac t

instance Read NominalDiffTime where
    readPrec = do
        t <- readPrec :: ReadPrec Pico
        _ <- lift $ char 's'
        return $ realToFrac t

#endif

#if MIN_VERSION_time(1,11,0) && !MIN_VERSION_time(1,11,1)
instance NFData Month where
    rnf (MkMonth m) = rnf m

instance Enum Month where
    succ (MkMonth a) = MkMonth (succ a)
    pred (MkMonth a) = MkMonth (pred a)
    toEnum = MkMonth . toEnum
    fromEnum (MkMonth a) = fromEnum a
    enumFrom (MkMonth a) = fmap MkMonth (enumFrom a)
    enumFromThen (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromThen a b)
    enumFromTo (MkMonth a) (MkMonth b) = fmap MkMonth (enumFromTo a b)
    enumFromThenTo (MkMonth a) (MkMonth b) (MkMonth c) =
        fmap MkMonth (enumFromThenTo a b c)

instance Ix Month where
    range (MkMonth a, MkMonth b) = fmap MkMonth (range (a, b))
    index (MkMonth a, MkMonth b) (MkMonth c) = index (a, b) c
    inRange (MkMonth a, MkMonth b) (MkMonth c) = inRange (a, b) c
    rangeSize (MkMonth a, MkMonth b) = rangeSize (a, b)

instance NFData QuarterOfYear where
    rnf Q1 = ()
    rnf Q2 = ()
    rnf Q3 = ()
    rnf Q4 = ()

instance NFData Quarter where
    rnf (MkQuarter m) = rnf m

instance Enum Quarter where
    succ (MkQuarter a) = MkQuarter (succ a)
    pred (MkQuarter a) = MkQuarter (pred a)
    toEnum = MkQuarter . toEnum
    fromEnum (MkQuarter a) = fromEnum a
    enumFrom (MkQuarter a) = fmap MkQuarter (enumFrom a)
    enumFromThen (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromThen a b)
    enumFromTo (MkQuarter a) (MkQuarter b) = fmap MkQuarter (enumFromTo a b)
    enumFromThenTo (MkQuarter a) (MkQuarter b) (MkQuarter c) =
        fmap MkQuarter (enumFromThenTo a b c)

instance Ix Quarter where
    range (MkQuarter a, MkQuarter b) = fmap MkQuarter (range (a, b))
    index (MkQuarter a, MkQuarter b) (MkQuarter c) = index (a, b) c
    inRange (MkQuarter a, MkQuarter b) (MkQuarter c) = inRange (a, b) c
    rangeSize (MkQuarter a, MkQuarter b) = rangeSize (a, b)

deriving instance Ix QuarterOfYear
#endif

-------------------------------------------------------------------------------
-- Lift & Generic
-------------------------------------------------------------------------------

#if !MIN_VERSION_time(1,15,0)
deriving instance TH.Lift LocalTime
deriving instance TH.Lift TimeZone
deriving instance TH.Lift ZonedTime

#if MIN_VERSION_time(1,9,2)
deriving instance TH.Lift CalendarDiffTime
#endif

#if MIN_VERSION_time(1,11,0)
deriving instance TH.Lift Quarter
#endif

instance TH.Lift TimeOfDay where
  lift (TimeOfDay h m (MkFixed s)) = [| TimeOfDay h m (MkFixed s) |]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (TimeOfDay h m (MkFixed s)) = [|| TimeOfDay h m (MkFixed s) ||]
#endif
#endif

#if !MIN_VERSION_time(1,14,0)
deriving instance TH.Lift Day
deriving instance TH.Lift UTCTime
deriving instance TH.Lift UniversalTime

deriving instance Generic Day
deriving instance Generic LocalTime
deriving instance Generic TimeOfDay
deriving instance Generic TimeZone
deriving instance Generic UTCTime
deriving instance Generic UniversalTime
deriving instance Generic ZonedTime

#if MIN_VERSION_time(1,9,0)
deriving instance TH.Lift DayOfWeek
deriving instance TH.Lift CalendarDiffDays

deriving instance Generic CalendarDiffDays
deriving instance Generic CalendarDiffTime
#endif

#if MIN_VERSION_time(1,11,0)
deriving instance Generic Quarter

deriving instance TH.Lift Month
deriving instance TH.Lift QuarterOfYear
deriving instance TH.Lift FirstWeekType
#endif

instance TH.Lift DiffTime where
    lift x = [| picosecondsToDiffTime x' |]
      where
        x' = diffTimeToPicoseconds x

#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped x = [|| picosecondsToDiffTime x' ||]
      where
        x' = diffTimeToPicoseconds x
#endif

#if MIN_VERSION_time(1,9,1)
instance TH.Lift NominalDiffTime where
    lift x = [| secondsToNominalDiffTime (MkFixed x' :: Pico) |]
      where
        x' = case nominalDiffTimeToSeconds x of MkFixed y -> y

#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped x = [|| secondsToNominalDiffTime (MkFixed x' :: Pico) ||]
      where
        x' = case nominalDiffTimeToSeconds x of MkFixed y -> y
#endif
#else
instance TH.Lift NominalDiffTime where
    lift x = [| realToFrac (MkFixed x' :: Pico) |]
      where
        x' = case realToFrac x :: Pico of MkFixed y -> y

#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped x = [|| realToFrac (MkFixed x' :: Pico) ||]
      where
        x' = case realToFrac x :: Pico of MkFixed y -> y
#endif
#endif

#endif

-------------------------------------------------------------------------------
-- Hashable
-------------------------------------------------------------------------------

instance Hashable UniversalTime where
    hashWithSalt salt = hashWithSalt salt . getModJulianDate

instance Hashable DiffTime where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Hashable UTCTime where
    hashWithSalt salt (UTCTime d dt) =
        salt `hashWithSalt` d `hashWithSalt` dt

instance Hashable NominalDiffTime where
    hashWithSalt salt = hashWithSalt salt . toRational

instance Hashable Day where
    hashWithSalt salt (ModifiedJulianDay d) = hashWithSalt salt d

instance Hashable TimeZone where
    hashWithSalt salt (TimeZone m s n) =
        salt `hashWithSalt` m `hashWithSalt` s `hashWithSalt` n

instance Hashable TimeOfDay where
    hashWithSalt salt (TimeOfDay h m s) =
        salt `hashWithSalt` h `hashWithSalt` m `hashWithSalt` s

instance Hashable LocalTime where
    hashWithSalt salt (LocalTime d tod) =
        salt `hashWithSalt` d `hashWithSalt` tod

instance Hashable TimeLocale where
    hashWithSalt salt (TimeLocale a b c d e f g h) =
      salt `hashWithSalt` a
           `hashWithSalt` b
           `hashWithSalt` c
           `hashWithSalt` d
           `hashWithSalt` e
           `hashWithSalt` f
           `hashWithSalt` g
           `hashWithSalt` h

#if MIN_VERSION_time(1,9,0)
instance Hashable DayOfWeek where
    hashWithSalt salt = hashWithSalt salt . fromEnum
#endif

#if MIN_VERSION_time(1,11,0)
instance Hashable Month where
    hashWithSalt salt (MkMonth x) = hashWithSalt salt x

instance Hashable Quarter where
    hashWithSalt salt (MkQuarter x) = hashWithSalt salt x

instance Hashable QuarterOfYear where
    hashWithSalt salt = hashWithSalt salt . fromEnum
#endif
