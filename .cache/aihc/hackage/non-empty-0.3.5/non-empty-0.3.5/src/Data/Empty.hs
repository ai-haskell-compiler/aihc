module Data.Empty where

import qualified Data.NonEmpty.Class as C

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Ix as Ix
import Control.Applicative (pure, )
import Control.DeepSeq (NFData, rnf, )

import qualified Test.QuickCheck as QC


data T a = Cons
   deriving (Eq, Ord)

instance Show (T a) where
   show Cons = "Empty.Cons"

instance C.Show T where
   showsPrec _p Cons = showString "Empty.Cons"

instance Functor T where
   fmap _ Cons = Cons

instance Fold.Foldable T where
   foldr _ y Cons = y

instance Trav.Traversable T where
   sequenceA Cons = pure Cons

instance C.ViewL T where
   viewL _ = Nothing

instance C.ViewR T where
   viewR _ = Nothing

instance C.View T where


instance QC.Arbitrary (T a) where
   arbitrary = return Cons
   shrink _ = []

instance C.Arbitrary T where
   arbitrary = return Cons
   shrink _ = []

instance C.Gen T where
   genOf _gen = return Cons


instance C.Empty T where
   empty = Cons

instance C.Zip T where
   zipWith _f Cons Cons = Cons

instance C.Reverse T where reverse = id

instance C.Sort T where
   sort Cons = Cons

instance C.SortBy T where
   sortBy _ Cons = Cons

{-
This instance allows us to use @transposeClip@ on fixed sized containers.
-}
instance C.Repeat T where
   repeat _ = Cons

{-
This instance allows us to iterate on fixed length lists.
-}
instance C.Iterate T where
   iterate _ _ = Cons

instance C.NFData T where
   rnf Cons = ()

instance NFData (T a) where
   rnf Cons = ()


instance C.Ix T where
   range (Cons, Cons) = [Cons]
   index (Cons, Cons) Cons = 0
   inRange (Cons, Cons) Cons = True
   rangeSize (Cons, Cons) = 1

instance (Ix.Ix i) => Ix.Ix (T i) where
   range = C.range
   index = C.index
   inRange = C.inRange
   rangeSize = C.rangeSize


switch :: b -> T a -> b
switch b Cons = b
