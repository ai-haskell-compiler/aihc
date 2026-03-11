{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Binary.Instances ()
import Test.QuickCheck.Instances ()

import Data.Binary           (Binary, decode, encode)
import Data.Typeable         (Typeable, typeOf)
import Test.QuickCheck       (Arbitrary, Property, (===))
import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.ByteString      (ByteString)
import Data.CaseInsensitive (CI)
import Data.HashMap.Lazy    (HashMap)
import Data.HashSet         (HashSet)
import Data.Monoid          (Sum)
import Data.Scientific      (Scientific)
import Data.Tagged          (Tagged)
import Data.Text            (Text)
import Data.Vector          (Vector)

import qualified Data.Vector.Unboxed as VU

import qualified Data.Primitive                    as Prim
import qualified Data.Time.Calendar.Compat         as Time
import qualified Data.Time.Calendar.Month.Compat   as Time
import qualified Data.Time.Calendar.Quarter.Compat as Time
import qualified Data.Time.Clock.Compat            as Time
import qualified Data.Time.Clock.System.Compat     as Time
import qualified Data.Time.Clock.TAI.Compat        as Time
import qualified Data.Time.LocalTime.Compat        as Time

#if MIN_VERSION_base(4,9,0)
import qualified Data.Array.Byte as AB
#endif

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Roundtrip"
    -- unordered containers
    [ roundtripProperty (undefined :: (HashMap Int String))
    , roundtripProperty (undefined :: (HashSet Int))
    -- time
    , roundtripProperty (undefined :: Time.AbsoluteTime)
    , roundtripProperty (undefined :: Time.CalendarDiffDays)
    , roundtripProperty (undefined :: Time.CalendarDiffTime)
    , roundtripProperty (undefined :: Time.Day)
    , roundtripProperty (undefined :: Time.DayOfWeek)
    , roundtripProperty (undefined :: Time.DiffTime)
    , roundtripProperty (undefined :: Time.LocalTime)
    , roundtripProperty (undefined :: Time.NominalDiffTime)
    , roundtripProperty (undefined :: Time.SystemTime)
    , roundtripProperty (undefined :: Time.TimeOfDay)
    , roundtripProperty (undefined :: Time.TimeZone)
    , roundtripProperty (undefined :: Time.UTCTime)
    , roundtripProperty (undefined :: Time.QuarterOfYear)
    , roundtripProperty (undefined :: Time.Quarter)
    , roundtripProperty (undefined :: Time.Month)
    -- case-insensitive & text
#if __GLASGOW_HASKELL__ <807
    -- https://github.com/haskell/text/issues/227
    , roundtripProperty (undefined :: (CI Text))
#endif
    , roundtripProperty (undefined :: (CI ByteString))
    -- semigroups / monoids
    , roundtripProperty (undefined :: (Sum Int))
    -- tagged
    , roundtripProperty (undefined :: Tagged Int Char)
    -- scientific
    , roundtripProperty (undefined :: Scientific)
    -- vector
    , roundtripProperty (undefined :: Vector Char)
    , roundtripProperty (undefined :: VU.Vector Char)
    , roundtripProperty (undefined :: Prim.ByteArray)
#if MIN_VERSION_base(4,9,0)
    , roundtripProperty (undefined :: AB.ByteArray)
#endif
    ]

roundtrip :: (Eq a, Show a, Binary a) => a -> a -> Property
roundtrip _ x = x === decode (encode x)

roundtripProperty
  :: forall a. (Eq a, Show a, Binary a, Arbitrary a, Typeable a)
  => a -> TestTree
roundtripProperty x = testProperty (show (typeOf x)) $ roundtrip x
