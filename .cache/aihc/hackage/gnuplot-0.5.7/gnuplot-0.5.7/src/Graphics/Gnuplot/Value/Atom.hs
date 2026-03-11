{- |
Provide a class that restricts the range of Haskell types
to the ones that gnuplot can process.
-}
module Graphics.Gnuplot.Value.Atom (
   OptionSet(..),
   C(..),
   ) where

import qualified Graphics.Gnuplot.Private.FrameOption as Option
import Graphics.Gnuplot.Utility (quote, )

import qualified Data.Time as Time
import Data.Word (Word8, Word16, Word32, Word64, )
import Data.Int (Int8, Int16, Int32, Int64, )
import Data.Ratio (Ratio, )


data OptionSet a =
   OptionSet {
      optData :: [String],
      optFormat :: [String],
      optOthers :: [(Option.T, [String])]
   }

class C a where
   options :: OptionSet a
   options =
      OptionSet [] [{- quote "%g" -}] []

instance C Float   where
instance C Double  where
instance C Int     where
instance C Integer where
instance (Integral a) => C (Ratio a) where

instance C Int8  where
instance C Int16 where
instance C Int32 where
instance C Int64 where
instance C Word8  where
instance C Word16 where
instance C Word32 where
instance C Word64 where


timeOptions :: OptionSet time
timeOptions =
   OptionSet ["time"] [quote "%d/%m"] [(Option.timeFmt, [quote "%s"])]

instance C Time.Day where
   options = timeOptions
instance C Time.UTCTime where
   options = timeOptions
