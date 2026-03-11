module Game.Mastermind.CodeSet.Union (
   T, member, size,
   fromLists, cube,
   overlappingPairs, overlapping,
   ) where

import qualified Game.Mastermind.CodeSet as CodeSet
import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet

import qualified Data.NonEmpty as NonEmpty
import qualified Data.EnumSet as EnumSet
import qualified Data.Set as Set
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.EnumSet (EnumSet)
import Data.Maybe (mapMaybe, )

import Control.Monad (liftM2, guard, )


{- |
@Cons [[a,b,c,d], [e,f,g,h]]@
expresses  a x b x c x d  union  e x f x g x h,
where @x@ denotes the set product.
-}
newtype T a = Cons [[NonEmptySet.T a]]

instance (Enum a, Show a) => Show (T a) where
   showsPrec n cs =
      showParen (n>=10) $
      showString "CodeSet.fromLists " . shows (toLists cs)

instance CodeSet.C T where
   empty = Cons []
   union = union
   intersection = intersection
   unit = Cons [[]]
   leftNonEmptyProduct c (Cons xs) = Cons (map (c:) xs)
   flatten = flatten
   symbols = symbols
   null (Cons xs) = null xs
   size = size
   select = select
   representationSize = representationSize
   compress = id


toLists :: (Enum a) => T a -> [[[a]]]
toLists (Cons xs) = map (map NonEmptySet.toFlatList) xs

fromLists :: (Enum a) => [[NonEmpty.T [] a]] -> T a
fromLists = Cons . map (map NonEmptySet.fromList)

flatten :: (Enum a) => T a -> [[a]]
flatten = concatMap sequence . toLists

symbols :: (Enum a) => T a -> EnumSet a
symbols = EnumSet.unions . map EnumSet.unions . flattenFactors

cube :: Int -> NonEmptySet.T a -> T a
cube n alphabet = Cons [replicate n alphabet]


size :: T a -> Integer
size = sum . productSizes

productSizes :: T a -> [Integer]
productSizes (Cons x) =
   map (product . map (fromIntegral . NonEmptySet.size)) x

select :: (Enum a) => T a -> Integer -> [a]
select set@(Cons xs) n0 =
   let sizes = productSizes set
   in  if n0<0
         then error "CodeSet.select: negative index"
         else
           case dropWhile (\(n1,sz,_) -> n1>=sz) $
                zip3 (scanl (-) n0 sizes) sizes xs of
             [] -> error "CodeSet.select: index too large"
             (n1,_,prod) : _ ->
                (\(n3,cs) ->
                   if n3==0
                     then cs
                     else error "CodeSet.select: at the end index must be zero") $
                List.mapAccumR
                   (\n2 componentSet ->
                      let (n3,i) =
                              divMod n2
                                 (fromIntegral $ NonEmptySet.size componentSet)
                      in  (n3,
                           NonEmptySet.toFlatList componentSet !! fromInteger i))
                   n1 prod

representationSize :: T a -> Int
representationSize (Cons x) =
   sum . map (sum . map NonEmptySet.size) $ x


{- |
We could try to merge set products.
I'll first want to see, whether this is needed in a relevant number of cases.
-}
union :: T a -> T a -> T a
union (Cons x) (Cons y) = Cons (x++y)

intersection :: (Enum a) => T a -> T a -> T a
intersection x y =
   normalize $
   liftM2 (zipWith EnumSet.intersection) (flattenFactors x) (flattenFactors y)

member :: (Enum a) => [a] -> T a -> Bool
member code (Cons xs) =
   any (and . zipWith NonEmptySet.member code) xs

{- |
Remove empty set products.
-}
normalize :: (Enum a) => [[EnumSet a]] -> T a
normalize = Cons . mapMaybe (mapM NonEmptySet.fetch)

flattenFactors :: (Enum a) => T a -> [[EnumSet a]]
flattenFactors (Cons xs) = map (map NonEmptySet.flatten) xs


disjointProduct :: (Enum a) => [EnumSet a] -> [EnumSet a] -> Bool
disjointProduct prod0 prod1 =
   any EnumSet.null $ zipWith EnumSet.intersection prod0 prod1

{- |
for debugging: list all pairs of products, that overlap
-}
overlappingPairs :: (Enum a) => T a -> [([EnumSet a], [EnumSet a])]
overlappingPairs set = do
   prod0:rest <- ListHT.tails $ flattenFactors set
   prod1 <- rest
   guard $ not $ disjointProduct prod0 prod1
   return (prod0, prod1)

{- |
for debugging: list all subsets, that are contained in more than one product
-}
overlapping :: (Enum a) => T a -> [([EnumSet a], [[EnumSet a]])]
overlapping set = do
   let xs = flattenFactors set
   subset <- Set.toList $ Set.fromList $ do
      prod0:rest <- ListHT.tails xs
      prod1 <- rest
      let sec = zipWith EnumSet.intersection prod0 prod1
      guard $ all (not . EnumSet.null) $ sec
      return sec
   return (subset, filter (not . disjointProduct subset) xs)
