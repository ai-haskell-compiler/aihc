module Game.Mastermind.CodeSet.Tree (
   T, null, member, intersection, size,
   propIntersections,
   ) where

import qualified Game.Mastermind.CodeSet as CodeSet
import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet

import Control.Monad (liftM2, mfilter, )

import qualified Data.NonEmpty as NonEmpty
import qualified Data.EnumSet as EnumSet
import qualified Data.Map as Map
import Data.EnumSet (EnumSet)

import Data.Tuple.HT (mapFst, swap, )
import Data.Ord.HT (comparing, )
import Data.Eq.HT (equating, )
import Data.Maybe (mapMaybe, )

import Prelude hiding (null, )


{- |
@Products [(a,b),(c,d)]@
expresses  a x b  union  c x d,
where @x@ denotes the set product.
-}
data T a = End | Products (Map.Map (NonEmptySet.T a) (T a))
   deriving (Show)

instance CodeSet.C T where
   empty = Products Map.empty
   union = union
   intersection = intersection
   unit = End
   leftNonEmptyProduct c xs =
      Products $
      if null xs
        then Map.empty
        else Map.singleton c xs
   flatten = flatten
   symbols = symbols
   null = null
   size = size
   select = select
   representationSize = representationSize
   compress = compress


flatten :: (Enum a) => T a -> [[a]]
flatten End = [[]]
flatten (Products xs) =
   concatMap
      (\(a,b) -> liftM2 (:) (NonEmptySet.toFlatList a) (flatten b))
      (Map.toList xs)

symbols :: (Enum a) => T a -> EnumSet a
symbols End = EnumSet.empty
symbols (Products xps) =
   EnumSet.unions $
   map (\(x,xs) -> EnumSet.union (NonEmptySet.flatten x) (symbols xs)) $
   Map.toList xps


size :: T a -> Integer
size End = 1
size (Products xs) =
   sum $ map (\(a,b) -> fromIntegral (NonEmptySet.size a) * size b) $
   Map.toList xs

-- somehow inefficient, because the sizes of subsets are recomputed several times
select :: (Enum a) => T a -> Integer -> [a]
select End n =
   case compare n 0 of
     LT -> error "CodeSet.select.end: index negative"
     EQ -> []
     GT -> error "CodeSet.select.end: index too large"
select (Products xps) n0 =
   if n0<0
     then error "CodeSet.select: negative index"
     else
       case dropWhile (\(_, ((n1,sz), _)) -> n1>=sz) $
            zip (Map.toList xps) $
            uncurry zip $
            mapFst (\sizes -> zip (scanl (-) n0 sizes) sizes) $
            unzip $
            map (\(x,xs) ->
               let sz = size xs
               in  (fromIntegral (NonEmptySet.size x) * sz, sz)) $
            Map.toList xps of
         [] -> error "CodeSet.select: index too large"
         ((x,xs), ((n1,_), xsSize)) : _ ->
            let (j,k) = divMod n1 xsSize
            in  (NonEmptySet.toFlatList x !! fromInteger j)
                : select xs k

representationSize :: T a -> Int
representationSize End = 1
representationSize (Products xs) =
   sum $ map (\(a,b) -> NonEmptySet.size a + representationSize b) $
   Map.toList xs


{- |
We could try to merge set products.
I'll first want to see, whether this is needed in a relevant number of cases.
-}
union :: (Enum a) => T a -> T a -> T a
union End End = End
union (Products xs) (Products ys) = Products (Map.unionWith union xs ys)
union _ _ = error "CodeSet.union: sets with different tuple size"

intersection :: (Enum a) => T a -> T a -> T a
intersection End End = End
intersection (Products xps) (Products yps) =
   Products $ Map.fromListWith union $ normalizeProducts $
   liftM2
      (\(x,xs) (y,ys) ->
         (EnumSet.intersection (NonEmptySet.flatten x) (NonEmptySet.flatten y),
          intersection xs ys))
      (Map.toList xps)
      (Map.toList yps)
intersection _ _ =
   error "CodeSet.intersection: sets with different tuple size"

{- |
Remove empty set products.
-}
normalizeProducts :: (Enum a) => [(EnumSet a, T a)] -> [(NonEmptySet.T a, T a)]
normalizeProducts =
   mapMaybe
      (\(x,xs) ->
         liftM2 (,) (NonEmptySet.fetch x) (mfilter (not . null) (Just xs)))


{-
Comparing for structural equivalence is overly strict,
but a lot simpler than comparing for set equivalence.
-}
propIntersections :: (Enum a) => NonEmpty.T [] (T a) -> Bool
propIntersections xs =
   equating Indexable
      (CodeSet.intersections xs)
      (CodeSet.intersectionsPQ xs)


{- |
This allows (T a) to be a key in a Map.
I do not want an Ord (T a) instance,
since it makes no sense and it requires an Eq (T a) instance
that is either expensive (if it means set equality)
or confusing (if it means structural equality).
-}
newtype Indexable a = Indexable (T a)

instance (Enum a) => Eq (Indexable a) where
   (Indexable x) == (Indexable y) =
      case (x,y) of
         (End,End) -> True
         (Products xs, Products ys) -> equating (fmap Indexable) xs ys
         _ -> False

instance (Enum a) => Ord (Indexable a) where
   compare (Indexable x) (Indexable y) =
      case (x,y) of
         (End,End) -> EQ
         (End,Products _) -> LT
         (Products _,End) -> GT
         (Products xs, Products ys) -> comparing (fmap Indexable) xs ys


compress :: (Enum a) => T a -> T a
compress End = End
compress (Products xs) =
   Products $
   Map.fromListWith union $ map swap $
   map (mapFst (\(Indexable set) -> set)) $ Map.toList $
   Map.fromListWith NonEmptySet.union $
   map (mapFst Indexable) $ map swap $ Map.toList $
   fmap compress xs

member :: (Enum a) => [a] -> T a -> Bool
member [] End = True
member (c:cs) (Products xps) =
   any (\(x,xs) -> NonEmptySet.member c x && member cs xs) $
   Map.toList xps
member _ _ =
   error "CodeSet.member: mismatch of tuple size and tuple size in set"

null :: T a -> Bool
null End = False
null (Products xs) = Map.null xs
