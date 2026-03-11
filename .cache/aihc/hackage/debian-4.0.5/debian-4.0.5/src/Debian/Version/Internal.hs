{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module Debian.Version.Internal
    ( DebianVersion(..)
    , Numeric(..)
    , NonNumeric(..)
    , Found(..)
    ) where

import Data.Data (Data)
import Data.Typeable (Typeable)

-- Currently we store the original version string in the data-type so
-- that we can faithfully reproduce it quickly. Currently we do not
-- have any way to modify a version number -- so this works fine. May
-- have to change later.
data DebianVersion
    = DebianVersion String (Found Int, NonNumeric, Found NonNumeric) deriving (Data, Typeable)

data NonNumeric
    = NonNumeric String (Found Numeric)
      deriving (Show, Data, Typeable)

data Numeric
    = Numeric Int (Maybe NonNumeric)
      deriving (Show, Data, Typeable)

data Found a
    = Found { unFound :: a }
    | Simulated { unFound :: a }
      deriving (Show, Data, Typeable)

instance (Eq a) => Eq (Found a) where
    f1 == f2 = (unFound f1) == (unFound f2)

instance (Ord a) => Ord (Found a) where
    compare f1 f2 = compare (unFound f1) (unFound f2)
