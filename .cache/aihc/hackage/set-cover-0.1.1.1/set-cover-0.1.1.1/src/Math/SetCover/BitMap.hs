module Math.SetCover.BitMap (
   Map(..),
   fromSet,
   add, inc,
   sub, dec,
   intersectionSet,
   differenceSet,
   minimumSet,
   ) where

import qualified Math.SetCover.BitSet as BitSet
import qualified Math.SetCover.Bit as Bit
import Math.SetCover.BitSet (Set(Set))
import Math.SetCover.Bit (difference, xor, (.|.), (.&.))

import qualified Data.List.Reverse.StrictSpine as ListRev
import qualified Data.List as List
import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))
import Data.Tuple.HT (mapSnd, swap)


{-
Sliced representation of Map [0..bitSize-1] Integer.
-}
newtype Map bits = Map {unMap :: [bits]} deriving (Show)

instance (Bit.C bits) => Semigroup (Map bits) where
   (<>) = add

instance (Bit.C bits) => Monoid (Map bits) where
   mempty = Map []
   mappend = add


fromSet :: Bit.C bits => Set bits -> Map bits
fromSet (Set x) = Map [x]

add :: Bit.C bits => Map bits -> Map bits -> Map bits
add (Map xs0) (Map ys0) =
   let go c xs [] = unMap $ inc (Set c) (Map xs)
       go c [] ys = unMap $ inc (Set c) (Map ys)
       go c (x:xs) (y:ys) =
          xor c (xor x y)  :  go (c.&.(x.|.y) .|. x.&.y) xs ys
   in  Map $ go Bit.empty xs0 ys0

inc :: Bit.C bits => Set bits -> Map bits -> Map bits
inc (Set xs0) (Map ys0) =
   Map $
   mapAccumAffix
      (\c -> if c==Bit.empty then [] else [c])
      (\c x -> (c .&. x, xor c x))
      xs0 ys0


sub :: Bit.C bits => Map bits -> Map bits -> Map bits
sub (Map xs0) (Map ys0) =
   let go c xs [] = normalize $ unMap $ dec (Set c) (Map xs)
       go c [] ys =
          if c==Bit.empty && all (==Bit.empty) ys
            then []
            else error "sub: underflow"
       go c (x:xs) (y:ys) =
          xor c (xor x y) : go (difference (c.|.y) x .|. c.&.y) xs ys
   in  Map $ go Bit.empty xs0 ys0

dec :: Bit.C bits => Set bits -> Map bits -> Map bits
dec (Set xs0) (Map ys0) =
   Map $
   mapAccumAffix
      (\c -> if c==Bit.empty then [] else error "dec: underflow")
      (\c x -> (difference c x, xor c x))
      xs0 ys0

{-# INLINE mapAccumAffix #-}
mapAccumAffix, _mapAccumAffix ::
   (acc -> [y]) -> (acc -> x -> (acc, y)) -> acc -> [x] -> [y]
mapAccumAffix affix f =
   let go acc0 (x:xs) = let (acc1, y) = f acc0 x in  y : go acc1 xs
       go acc [] = affix acc
   in  go

_mapAccumAffix affix f acc =
   uncurry (++) . mapSnd affix . swap . List.mapAccumL f acc

intersectionSet :: (Bit.C bits) => Map bits -> Set bits -> Map bits
intersectionSet (Map xs) (Set y) = Map $ normalize $ map (y.&.) xs

differenceSet :: (Bit.C bits) => Map bits -> Set bits -> Map bits
differenceSet (Map xs) (Set y) = Map $ normalize $ map (flip difference y) xs

normalize :: (Bit.C bits) => [bits] -> [bits]
normalize = ListRev.dropWhile (Bit.empty==)


{-
Only elements from the base set are considered.
This way we can distinguish between non-members and members with count zero.
-}
minimumSet :: Bit.C bits => Set bits -> Map bits -> Set bits
minimumSet baseSet (Map xs) =
   foldr
      (\x mins ->
         case BitSet.difference mins $ Set x of
            newMins ->
               if BitSet.null newMins
                 then mins
                 else newMins)
      baseSet xs
