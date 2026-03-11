{- |
Provide a class that renders multiple Haskell values in a text form
that is accessible by gnuplot.

Maybe we add a method for the binary interface to gnuplot later.
-}
module Graphics.Gnuplot.Value.Tuple (
   C(text, columnCount),
   ColumnCount(ColumnCount),
   Label(Label),
   ) where

import Graphics.Gnuplot.Utility (quote)

import Data.Time.Format (defaultTimeLocale, )
import qualified Data.Time as Time

import Data.Word (Word8, Word16, Word32, Word64, )
import Data.Int (Int8, Int16, Int32, Int64, )
import Data.Ratio (Ratio, )

import Data.Function (id, ($), (.), )
import Text.Show (Show, ShowS, shows, showString, )
import Prelude
         (Eq, Ord, String, Int, Integer, Integral, Float, Double,
          realToFrac, (+), (++), )


class C a where
   {- |
   For values that are also in Atom class,
   'text' must generate a singleton list.
   -}
   text :: a -> [ShowS]

   {- |
   It must hold @ColumnCount (length (text x)) == columnCount@.
   -}
   columnCount :: ColumnCount a
   columnCount = ColumnCount 1

{- |
Count numbers of gnuplot data columns for the respective type.

Somehow a writer monad with respect to Sum monoid
without material monadic result.

Cf. ColumnSet module.
-}
newtype ColumnCount a = ColumnCount Int
   deriving (Eq, Ord, Show)

{-
Functor and Applicative instances would be useful
for combining column sets,
but they are dangerous, because they can bring
type and column columnCount out of sync.
-}

pure :: a -> ColumnCount a
pure _ = ColumnCount 0

(<*>) :: ColumnCount (a -> b) -> ColumnCount a -> ColumnCount b
ColumnCount n <*> ColumnCount m = ColumnCount (n+m)


singleton :: a -> [a]
singleton = (:[])


instance C Float   where text = singleton . shows
instance C Double  where text = singleton . shows
instance C Int     where text = singleton . shows
instance C Integer where text = singleton . shows
instance (Integral a) => C (Ratio a) where
   text = singleton . shows . (id :: Double->Double) . realToFrac

instance C Int8  where text = singleton . shows
instance C Int16 where text = singleton . shows
instance C Int32 where text = singleton . shows
instance C Int64 where text = singleton . shows
instance C Word8  where text = singleton . shows
instance C Word16 where text = singleton . shows
instance C Word32 where text = singleton . shows
instance C Word64 where text = singleton . shows

instance C Time.Day where
   text d = text $ Time.UTCTime d 0
instance C Time.UTCTime where
   text = singleton . showString . Time.formatTime defaultTimeLocale "%s"

newtype Label = Label String
instance C Label where text (Label str) = singleton $ showString $ quote str


instance (C a, C b) => C (a,b) where
   text (a,b) = text a ++ text b
   columnCount =
      pure (,)
         <*> columnCount
         <*> columnCount

instance (C a, C b, C c) => C (a,b,c) where
   text (a,b,c) = text a ++ text b ++ text c
   columnCount =
      pure (,,)
         <*> columnCount
         <*> columnCount
         <*> columnCount

instance (C a, C b, C c, C d) => C (a,b,c,d) where
   text (a,b,c,d) = text a ++ text b ++ text c ++ text d
   columnCount =
      pure (,,,)
         <*> columnCount
         <*> columnCount
         <*> columnCount
         <*> columnCount
