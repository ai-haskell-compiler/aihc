{-# LANGUAGE FlexibleContexts #-}
module Data.NonEmptyPrivate where

import qualified Data.NonEmpty.Foldable as FoldU
import qualified Data.NonEmpty.Class as C
import qualified Data.Empty as Empty

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, )

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Ix as Ix
import Data.Traversable (Traversable, mapAccumL, mapAccumR)
import Data.Foldable (Foldable, )
import Control.Monad.HT (void, )
import Control.Monad (Monad, return, (=<<), )
import Control.Applicative (Applicative, liftA2, pure, (<*>), )
import Control.DeepSeq (NFData, rnf, )

import Data.Functor (Functor, fmap, )
import Data.Function (flip, const, ($), (.), )
import Data.Either (Either(Left, Right), )
import Data.Maybe (Maybe(Just, Nothing), maybe, mapMaybe, )
import Data.Bool.HT (if', )
import Data.Bool (Bool(True), (&&), )
import Data.Ord (Ord, Ordering(GT), (<=), (>), compare, comparing, )
import Data.Eq ((==), )
import Data.Tuple.HT (mapFst, mapSnd, swap, )
import Data.Tuple (fst, snd, )
import qualified Prelude as P
import Prelude (Eq, Show, Num, Int, uncurry, ($!), (*), (+), )

import qualified Test.QuickCheck as QC


{- $setup
>>> import qualified Data.NonEmpty as NonEmpty
>>> import qualified Data.Empty as Empty
>>> import qualified Data.Either.HT as EitherHT
>>> import qualified Control.Functor.HT as FuncHT
>>> import qualified Data.Ix as Ix
>>> import Data.NonEmpty ((!:))
>>> import Data.Tuple.HT (swap)
>>> import Data.Maybe (mapMaybe)
>>> import Control.Applicative (liftA2)
>>> import Control.Functor.HT (void)
>>> import qualified Test.QuickCheck as QC
>>>
>>> forRange :: (QC.Testable test) => ((Char,Char) -> test) -> QC.Property
>>> forRange =
>>>    QC.forAll (liftA2 (,) (QC.choose ('a','h')) (QC.choose ('a','h')))
-}

{-
We could also have (:!) as constructor,
but in order to import it unqualified we have to import 'T' unqualified, too,
and this would cause name clashes with locally defined types with name @T@.
-}
{- |
The type 'T' can be used for many kinds of list-like structures
with restrictions on the size.

* @T [] a@ is a lazy list containing at least one element.

* @T (T []) a@ is a lazy list containing at least two elements.

* @T Vector a@ is a vector with at least one element.
  You may also use unboxed vectors but the first element will be stored in a box
  and you will not be able to use many functions from this module.

* @T Maybe a@ is a list that contains one or two elements.

* @Maybe@ is isomorphic to @Optional Empty@.

* @T Empty a@ is a list that contains exactly one element.

* @T (T Empty) a@ is a list that contains exactly two elements.

* @Optional (T Empty) a@ is a list that contains zero or two elements.

* You can create a list type for every finite set of allowed list length
  by nesting Optional and NonEmpty constructors.
  If list length @n@ is allowed, then place @Optional@ at depth @n@,
  if it is disallowed then place @NonEmpty@.
  The maximum length is marked by @Empty@.
-}
data T f a = Cons { head :: a, tail :: f a }
   deriving (Eq, Ord)


instance (C.NFData f, NFData a) => NFData (T f a) where
   rnf = C.rnf

instance (C.NFData f) => C.NFData (T f) where
   rnf (Cons x xs) = rnf (x, C.rnf xs)


instance (C.Show f, Show a) => Show (T f a) where
   showsPrec = C.showsPrec

instance (C.Show f) => C.Show (T f) where
   showsPrec p (Cons x xs) =
      P.showParen (p>5) $
      P.showsPrec 6 x . P.showString "!:" . C.showsPrec 5 xs


infixr 5 !:, `append`, `appendRight`, `appendLeft`

(!:) :: a -> f a -> T f a
(!:) = Cons


{- |
Force immediate generation of Cons.
-}
force :: T f a -> T f a
force x = Cons (head x) (tail x)


instance Functor f => Functor (T f) where
   fmap f (Cons x xs) = f x !: fmap f xs

instance Foldable f => Foldable (T f) where
   foldr f y (Cons x xs) = f x $ Fold.foldr f y xs
   foldl1 = foldl1
   foldr1 f (Cons x xs) =
      maybe x (f x) $
      Fold.foldr (\y -> Just . maybe y (f y)) Nothing xs
{-
   foldr1 f (Cons x xs) =
      case xs of
         [] -> x
         y:ys -> f x $ Fold.foldr1 f (Cons y ys)
-}


instance Traversable f => Traversable (T f) where
   sequenceA (Cons x xs) = liftA2 Cons x $ Trav.sequenceA xs

instance
   (Applicative f, C.Empty f, C.Cons f, C.Append f) =>
      Applicative (T f) where
   pure = singleton
   (<*>) = apply

instance (Monad f, C.Empty f, C.Cons f, C.Append f) =>
      Monad (T f) where
   return = singleton
   (>>=) = bind


instance (C.Arbitrary f) => C.Arbitrary (T f) where
   arbitrary = arbitrary
   shrink = shrink

instance (QC.Arbitrary a, C.Arbitrary f) => QC.Arbitrary (T f a) where
   arbitrary = arbitrary
   shrink = shrink

arbitrary :: (QC.Arbitrary a, C.Arbitrary f) => QC.Gen (T f a)
arbitrary = liftA2 Cons QC.arbitrary C.arbitrary

shrink :: (QC.Arbitrary a, C.Arbitrary f) => T f a -> [T f a]
shrink (Cons x xs) = fmap (\(y, Aux ys) -> Cons y ys) $ QC.shrink (x, Aux xs)

newtype Aux f a = Aux (f a)

instance (C.Arbitrary f, QC.Arbitrary a) => QC.Arbitrary (Aux f a) where
   arbitrary = fmap Aux C.arbitrary
   shrink (Aux x) = fmap Aux $ C.shrink x


instance (C.Gen f) => C.Gen (T f) where
   genOf gen = liftA2 Cons gen $ C.genOf gen


{- |
Implementation of 'Applicative.<*>' without the 'C.Empty' constraint
that is needed for 'Applicative.pure'.
-}
apply ::
   (Applicative f, C.Cons f, C.Append f) =>
   T f (a -> b) -> T f a -> T f b
apply (Cons f fs) (Cons x xs) =
   Cons (f x) (fmap f xs `C.append` (fs <*> C.cons x xs))

{- |
Implementation of 'Monad.>>=' without the 'C.Empty' constraint
that is needed for 'Monad.return'.
-}
bind ::
   (Monad f, C.Cons f, C.Append f) =>
   T f a -> (a -> T f b) -> T f b
bind (Cons x xs) k =
   appendRight (k x) (flatten . k =<< xs)


toList :: Foldable f => T f a -> [a]
toList (Cons x xs) = x : Fold.toList xs

flatten :: C.Cons f => T f a -> f a
flatten (Cons x xs) = C.cons x xs

fetch :: C.ViewL f => f a -> Maybe (T f a)
fetch = fmap (uncurry Cons) . C.viewL


{- |
Caution:
@viewL (NonEmpty.Cons x []) = Nothing@
because the tail is empty, and thus cannot be NonEmpty!

This instance mainly exist to allow cascaded applications of 'fetch'.
-}
instance C.ViewL f => C.ViewL (T f) where
   viewL (Cons x xs) = fmap ((,) x) $ fetch xs

instance C.Cons f => C.Cons (T f) where
   cons x0 (Cons x1 xs) = x0 !: C.cons x1 xs

instance C.Snoc f => C.Snoc (T f) where
   snoc (Cons x0 xs) x1 = x0 !: C.snoc xs x1


{- |
Synonym for 'Cons'.
For symmetry to 'snoc'.
-}
cons :: a -> f a -> T f a
cons = Cons

snoc :: Traversable f => f a -> a -> T f a
snoc xs x =
   uncurry Cons $ mapAccumR (flip (,)) x xs

-- name of the class could also be ShiftL
class Snoc f where snocFast :: f a -> a -> T f a
instance Snoc [] where snocFast = snocGeneric
instance Snoc Seq where snocFast = snocGeneric
instance Snoc Empty.T where snocFast ~Empty.Cons x = Cons x Empty.Cons
instance Snoc Maybe where
   snocFast mx y = uncurry Cons $ maybe (y, Nothing) (\x -> (x, Just y)) mx

-- | For 'Seq' faster than 'snoc'.
snocGeneric :: (C.ViewL f, C.Empty f, C.Snoc f) => f a -> a -> T f a
snocGeneric xs x =
   uncurry Cons $
   case C.viewL xs of
      Nothing -> (x, C.empty)
      Just (y,ys) -> (y, C.snoc ys x)

snocAlt :: (C.Cons f, Traversable f) => f a -> a -> f a
snocAlt xs x = flatten $ snoc xs x


instance C.Empty f => C.Singleton (T f) where
   singleton = singleton

singleton :: C.Empty f => a -> T f a
singleton x = x !: C.empty


viewL :: T f a -> (a, f a)
viewL (Cons x xs) = (x, xs)

viewR :: (Traversable f) => T f a -> (f a, a)
viewR (Cons x xs) = swap $ mapAccumL (flip (,)) x xs


switchL :: (a -> f a -> b) -> T f a -> b
switchL f (Cons x xs) = f x xs

uncurrier :: (b -> f a -> c) -> (a -> b) -> T f a -> c
uncurrier g = switchL . (g .)

currier :: ((f a -> b) -> c) -> (T f a -> b) -> a -> c
currier g f x = g $ \xs -> f $ x !: xs


mapHead :: (a -> a) -> T f a -> T f a
mapHead f (Cons x xs) = f x !: xs

mapTail :: (f a -> g a) -> T f a -> T g a
mapTail f (Cons x xs) = x !: f xs

init :: (Traversable f) => T f a -> f a
init = fst . viewR

last :: (Foldable f) => T f a -> a
last = foldl1 (flip const)

foldl1 :: (Foldable f) => (a -> a -> a) -> T f a -> a
foldl1 f (Cons x xs) = Fold.foldl f x xs

{- |
It holds:

> foldl1Map f g = foldl1 f . fmap g

but 'foldl1Map' does not need a 'Functor' instance.
-}
foldl1Map :: (Foldable f) => (b -> b -> b) -> (a -> b) -> T f a -> b
foldl1Map f g (Cons x xs) = Fold.foldl (\b a -> f b (g a)) (g x) xs


-- cf. NumericPrelude: Algebra.Additive.sumNestedCommutative
{-
Estimate costs of @foldBalanced ListHT.merge@.
@a, b, c@ length of sub-lists and our measure for the cost.

xs = [a,b,c]
ys = [a,b,c,a+b,c+a+b]
costs: (a+b) + (c+a+b) = 2a+2b+c

xs = [a,b,c,d]
ys = [a,b,c,d,a+b,c+d,a+b+c+d]
costs: (a+b) + (c+d) + (a+b+c+d) = 2a+2b+2c+2d

xs = [a,b,c,d,e]
ys = [a,b,c,d,e,a+b,c+d,e+(a+b),c+d+e+(a+b)]
costs: (a+b) + (c+d) + (e+(a+b)) + (c+d+e+(a+b)) = 3a+3b+2c+2d+2e

Analysis is easiest if @length xs@ is a power of two, e.g. @2^n@.
Then the operator tree has height @n@.
That is, we get a run-time of @n * sum (map length xs)@.
This is usually better than @sort (concat xs)@
which has run-time @let m = sum (map length xs) in m * logBase 2 m@.
-}
{- |
Fold a non-empty list in a balanced way.
/Balanced/ means that each element
has approximately the same depth in the operator tree.
/Approximately the same depth/ means
that the difference between maximum and minimum depth is at most 1.
The accumulation operation must be associative and commutative
in order to get the same result as 'foldl1' or 'foldr1'.
-}
foldBalanced :: (a -> a -> a) -> T [] a -> a
foldBalanced = foldBalancedGen (:)

foldBalancedStrict :: (a -> a -> a) -> T [] a -> a
foldBalancedStrict = foldBalancedGen (\x -> ((:) $! x))

foldBalancedGen :: (a -> [a] -> [a]) -> (a -> a -> a) -> T [] a -> a
foldBalancedGen listCons f xs@(Cons _ rs) =
   let reduce (z0:z1:zs) = listCons (f z0 z1) (reduce zs)
       reduce zs = zs
       ys = appendRight xs $ Match.take rs $ reduce $ flatten ys
   in  last ys


-- | maximum is a total function
maximum :: (Ord a, Foldable f) => T f a -> a
maximum = foldl1 P.max

-- | minimum is a total function
minimum :: (Ord a, Foldable f) => T f a -> a
minimum = foldl1 P.min

-- | maximumBy is a total function
maximumBy :: (Foldable f) => (a -> a -> Ordering) -> T f a -> a
maximumBy f = foldl1 (\x y -> case f x y of P.LT -> y; _ -> x)

-- | minimumBy is a total function
minimumBy :: (Foldable f) => (a -> a -> Ordering) -> T f a -> a
minimumBy f = foldl1 (\x y -> case f x y of P.GT -> y; _ -> x)

-- | maximumKey is a total function
maximumKey :: (Ord b, Foldable f) => (a -> b) -> T f a -> a
maximumKey f =
   snd . Fold.maximumBy (comparing fst) . FoldU.Mapped (attachKey f)

-- | minimumKey is a total function
minimumKey :: (Ord b, Foldable f) => (a -> b) -> T f a -> a
minimumKey f =
   snd . Fold.minimumBy (comparing fst) . FoldU.Mapped (attachKey f)

-- | maximumKey is a total function
_maximumKey :: (Ord b, Foldable f, Functor f) => (a -> b) -> T f a -> a
_maximumKey f =
   snd . maximumBy (comparing fst) . fmap (attachKey f)

-- | minimumKey is a total function
_minimumKey :: (Ord b, Foldable f, Functor f) => (a -> b) -> T f a -> a
_minimumKey f =
   snd . minimumBy (comparing fst) . fmap (attachKey f)

attachKey :: (a -> b) -> a -> (b, a)
attachKey f a = (f a, a)

-- | sum does not need a zero for initialization
sum :: (Num a, Foldable f) => T f a -> a
sum = foldl1 (P.+)

-- | product does not need a one for initialization
product :: (Num a, Foldable f) => T f a -> a
product = foldl1 (P.*)


chop :: (a -> Bool) -> [a] -> T [] [a]
chop p =
   uncurry cons .
   P.foldr (\ x ~(y,ys) -> if p x then ([],y:ys) else ((x:y),ys) ) ([],[])


instance (C.Cons f, C.Append f) => C.Append (T f) where
   append xs ys = appendRight xs (flatten ys)

append :: (C.Append f, Traversable f) => T f a -> T f a -> T (T f) a
append xs ys =
   mapTail (flip appendLeft ys) xs

appendRight :: (C.Append f) => T f a -> f a -> T f a
appendRight (Cons x xs) ys = Cons x (C.append xs ys)

appendLeft ::
   (C.Append f, Traversable f) =>
   f a -> T f a -> T f a
appendLeft xt (Cons y ys) =
   mapTail (flip C.append ys) $ snoc xt y


{- |
generic variants:
'Data.Monoid.HT.cycle' or better @Semigroup.cycle@
-}
cycle :: (C.Cons f, C.Append f) => T f a -> T f a
cycle x =
   let y = C.append x y
   in  y


instance (C.Zip f) => C.Zip (T f) where
   zipWith = zipWith

zipWith :: (C.Zip f) => (a -> b -> c) -> T f a -> T f b -> T f c
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (C.zipWith f as bs)


instance (C.Repeat f) => C.Repeat (T f) where
   repeat a = Cons a $ C.repeat a

instance (C.Iterate f) => C.Iterate (T f) where
   iterate f a = Cons a $ C.iterate f (f a)


{-
This implementation needs quadratic time
with respect to the number of 'Cons'.
Maybe a linear time solution can be achieved using a type function
that maps a container type to the type of the reversed container.
-}
reverse :: (Traversable f, C.Reverse f) => T f a -> T f a
reverse (Cons x xs) = snoc (C.reverse xs) x

instance (Traversable f, C.Reverse f) => C.Reverse (T f) where
   reverse = reverse


{- |
If you nest too many non-empty lists
then the efficient merge-sort (linear-logarithmic runtime)
will degenerate to an inefficient insert-sort (quadratic runtime).
-}
instance (C.Sort f, InsertBy f) => C.Sort (T f) where
   sort (Cons x xs) = insert x $ C.sort xs

instance (C.SortBy f, InsertBy f) => C.SortBy (T f) where
   sortBy f (Cons x xs) = insertBy f x $ C.sortBy f xs


class Insert f where
   {- |
   Insert an element into an ordered list while preserving the order.
   -}
   insert :: (Ord a) => a -> f a -> T f a

instance (Insert f) => Insert (T f) where
   insert y xt@(Cons x xs) =
      uncurry Cons $
      case compare y x of
         GT -> (x, insert y xs)
         _ -> (y, xt)

instance Insert Empty.T where
   insert = insertDefault

instance Insert [] where
   insert = insertDefault

instance Insert Maybe where
   insert = insertDefault

instance Insert Seq where
   insert = insertDefault

{-
This does not work consistently!
A Set is not a sorted list, since it collapses duplicate elements.

*Data.NonEmptyPrivate> mapTail (mapTail Set.toList) $ insert '3' $ insert '7' $ Set.fromList "346"
'3'!:'3'!:'4':'6':'7':[]

instance Insert Set where
   insert y xt =
      uncurry Cons $
      fromMaybe (y, xt) $ do
         (x,xs) <- Set.minView xt
         case compare y x of
            GT -> return (x, Set.insert y xs)
            EQ -> return (x, xs)
            LT -> mzero

We have preserved that function in NonEmpty.Mixed.
-}

{- |
Default implementation for 'insert' based on 'insertBy'.
-}
insertDefault :: (Ord a, InsertBy f, C.SortBy f) => a -> f a -> T f a
insertDefault = insertBy compare


class Insert f => InsertBy f where
   insertBy :: (a -> a -> Ordering) -> a -> f a -> T f a

instance (InsertBy f) => InsertBy (T f) where
   insertBy f y xt@(Cons x xs) =
      uncurry Cons $
      case f y x of
         GT -> (x, insertBy f y xs)
         _ -> (y, xt)

instance InsertBy Empty.T where
   insertBy _ x Empty.Cons = Cons x Empty.Cons

instance InsertBy [] where
   insertBy f y xt =
      uncurry Cons $
      case xt of
         [] -> (y, xt)
         x:xs ->
            case f y x of
               GT -> (x, List.insertBy f y xs)
               _ -> (y, xt)

instance InsertBy Maybe where
   insertBy f y mx =
      uncurry Cons $
      case mx of
         Nothing -> (y, Nothing)
         Just x ->
            mapSnd Just $
            case f y x of
               GT -> (x, y)
               _ -> (y, x)

instance InsertBy Seq where
   {-
   If we assume a sorted list
   we could do binary search for the splitting point.
   -}
   insertBy f y xt =
      uncurry Cons $
      case Seq.spanl ((GT ==) . f y) xt of
         (ys,zs) ->
            case Seq.viewl ys of
               Seq.EmptyL -> (y, xt)
               w Seq.:< ws -> (w, ws Seq.>< y Seq.<| zs)

{-
Certainly not as efficient as insertBy as class method
since all elements of the list are touched.
-}
insertByTraversable ::
   (Traversable f) =>
   (a -> a -> Ordering) -> a -> f a -> T f a
insertByTraversable cmp y0 =
   uncurry (flip snoc . snd) .
   mapAccumL
      (\(searching,y) x ->
         let stillSearching = searching && cmp y x == GT
         in  mapFst ((,) stillSearching) $ if' stillSearching (y,x) (x,y))
      (True, y0)



mapWithIndex :: (Traversable f) => (Int -> a -> b) -> Int -> f a -> f b
mapWithIndex f n = snd . mapAccumL (\k x -> (P.succ k, f k x)) n

removeAt :: (Traversable f) => Int -> T f a -> (a, f a)
removeAt n (Cons x0 xs) =
   mapAccumL (\x (k,y) -> if k<=n then (y,x) else (x,y)) x0 $
   mapWithIndex (,) 1 xs

removeEach :: (Traversable f) => T f a -> T f (a, f a)
removeEach xs  =  mapWithIndex (\n _ -> removeAt n xs) 0 xs

{- |
prop> let takeUntil p xs = NonEmpty.zipWith const xs $ () !: void (takeWhile (not . p) $ NonEmpty.flatten xs) in \k xs -> takeUntil (>=k) xs == NonEmpty.takeUntil (>=(k::Int)) xs
-}
takeUntil :: (a -> Bool) -> T [] a -> T [] a
takeUntil p (Cons x xs) =
   x !: if p x then [] else ListHT.takeUntil p xs

takeUntilAlt :: (a -> Bool) -> T [] a -> T [] a
takeUntilAlt p xs =
   zipWith const xs $ Cons () $ void $ List.takeWhile (P.not . p) $ flatten xs



{-
It is somehow better than the variant in NonEmpty.Mixed,
since it can be applied to nested NonEmptys.

Type @g@ could be fixed to List,
since context (C.Cons g, C.Empty g) means
that @g@ is a supertype of something isomorphic to list.
However, repeatedly prepending an element might be more efficient
than repeated conversion from list to a structure like Sequence.
-}
tails :: (Traversable f, C.Cons g, C.Empty g) => f a -> T f (g a)
tails = scanr C.cons C.empty


{- |
Only advised for structures with efficient appending of single elements
like 'Sequence'.
Alternatively you may consider 'initsRev'.
-}
inits :: (Traversable f, C.Snoc g, C.Empty g) => f a -> T f (g a)
inits = scanl C.snoc C.empty

{-
suggested in
<http://www.haskell.org/pipermail/libraries/2014-July/023291.html>
-}
initsRev ::
   (Traversable f, C.Cons g, C.Empty g, C.Reverse g) =>
   f a -> T f (g a)
initsRev = fmap C.reverse . scanl (flip C.cons) C.empty

{-
Not exported by NonEmpty.
I think the transposeClip function is better.
-}
class TransposeOuter f where
   transpose :: TransposeInner g => f (g a) -> g (f a)

instance TransposeOuter [] where
   transpose =
      let go [] = transposeStart
          go (xs : xss) = zipHeadTail xs $ go xss
      in  go

{-
We cannot define this instance,
because @transpose ([] !: [2] !: []) = [2 !: []]@

instance TransposeOuter f => TransposeOuter (T f) where
   transpose =
      let go (Cons xs xss) = zipHeadTail xs $ go xss
      in  go
-}

class TransposeInner g where
   transposeStart :: g a
   zipHeadTail :: (C.Singleton f, C.Cons f) => g a -> g (f a) -> g (f a)

instance TransposeInner [] where
   transposeStart = []
   zipHeadTail =
      let go (x:xs) (ys:yss) = C.cons x ys : go xs yss
          go [] yss = yss
          go xs [] = fmap C.singleton xs
      in  go

{-
We cannot define this instance,
because @transpose ([] :: [NonEmpty.T [] Int]) = []@,
but in order to satisfy the types it must be ([] !: []).

instance TransposeInner f => TransposeInner (T f) where
   transposeStart = Cons ??? transposeStart
   zipHeadTail (Cons x xs) (Cons ys yss) =
      Cons (C.cons x ys) (zipHeadTail xs yss)
-}

{-
transpose :: [[a]] -> [[a]]
transpose =
   let go [] = []
       go (xs : xss) = zipHeadTail xs $ go xss
   in  go

zipHeadTail :: [a] -> [[a]] -> [[a]]
zipHeadTail (x:xs) (ys:yss) = (x:ys) : zipHeadTail xs yss
zipHeadTail [] yss = yss
zipHeadTail xs [] = fmap (:[]) xs
-}

transposePrelude :: [[a]] -> [[a]]
transposePrelude =
   let go [] = []
       go ([] : xss) = go xss
       go ((x:xs) : xss) =
          case ListHT.unzip $ mapMaybe ListHT.viewL xss of
             (ys, yss) -> (x : ys) : go (xs : yss)
   in  go

propTranspose :: [[P.Int]] -> P.Bool
propTranspose xs =
   List.transpose xs P.== transpose xs

propTransposePrelude :: [[P.Int]] -> P.Bool
propTransposePrelude xs =
   List.transpose xs P.== transposePrelude xs



scanl :: Traversable f => (b -> a -> b) -> b -> f a -> T f b
scanl f b =
   Cons b . snd .
   mapAccumL (\b0 -> (\b1 -> (b1,b1)) . f b0) b

scanr :: Traversable f => (a -> b -> b) -> b -> f a -> T f b
scanr f b =
   uncurry Cons .
   mapAccumR (\b0 -> flip (,) b0 . flip f b0) b

mapAdjacent ::
   (Traversable f) => (a -> a -> b) -> T f a -> f b
mapAdjacent f (Cons x xs) =
   snd $ mapAccumL (\a0 a1 -> (a1, f a0 a1)) x xs

{-
A nice function but not particularly related to NonEmpty.
Maybe move it to Class module?
-}
mapAdjacent1 :: (Traversable f) => (a -> a -> b -> c) -> a -> f (a,b) -> f c
mapAdjacent1 f = (snd.) . mapAccumL (\a0 (a1,b) -> (a1, f a0 a1 b))

{- |
prop> \xs -> mapMaybe EitherHT.maybeLeft (NonEmpty.flatten xs) == either NonEmpty.flatten fst (NonEmpty.partitionEithersLeft (xs::NonEmpty.T[](Either Char Int)))
prop> \xs -> mapMaybe EitherHT.maybeRight (NonEmpty.flatten xs) == either (const []) (NonEmpty.flatten . snd) (NonEmpty.partitionEithersLeft (xs::NonEmpty.T[](Either Char Int)))
prop> \xs -> NonEmpty.partitionEithersRight (fmap EitherHT.swap xs) == EitherHT.mapLeft swap (EitherHT.swap (NonEmpty.partitionEithersLeft (xs::NonEmpty.T[](Either Char Int))))
-}
partitionEithersLeft :: T [] (Either a b) -> Either (T [] a) ([a], T [] b)
partitionEithersLeft (Cons x xs) =
   case (x, ListHT.unzipEithers xs) of
      (Right r, (ls,rs)) -> Right (ls, Cons r rs)
      (Left l, (ls,rs)) ->
         maybe (Left $ Cons l ls) (Right . (,) (l:ls)) $ fetch rs

{- |
prop> \xs -> NonEmpty.partitionEithersLeft (fmap EitherHT.swap xs) == EitherHT.mapRight swap (EitherHT.swap (NonEmpty.partitionEithersRight (xs::NonEmpty.T[](Either Char Int))))
-}
partitionEithersRight :: T [] (Either a b) -> Either (T [] a, [b]) (T [] b)
partitionEithersRight (Cons x xs) =
   case (x, ListHT.unzipEithers xs) of
      (Left l, (ls,rs)) -> Left (Cons l ls, rs)
      (Right r, (ls,rs)) ->
         maybe (Right $ Cons r rs) (Left . flip (,) (r:rs)) $ fetch ls


{- |
prop> forRange $ \b0 -> forRange $ \b1 -> forRange $ \b2 -> let b = FuncHT.unzip $ b0!:b1!:b2!:Empty.Cons in map (Ix.index b) (Ix.range b) == take (Ix.rangeSize b) [0..]
-}
instance C.Ix f => C.Ix (T f) where
   range (Cons l ls, Cons u us) =
      liftA2 Cons (Ix.range (l,u)) (C.range (ls,us))
   inRange (Cons l ls, Cons u us) (Cons i is) =
      Ix.inRange (l,u) i && C.inRange (ls,us) is
   rangeSize (Cons l ls, Cons u us) =
      Ix.rangeSize (l,u) * C.rangeSize (ls,us)
   rangeSizeIndex (Cons l ls, Cons u us) =
      let size = Ix.rangeSize (l,u)
          (subSize, subIndex) = C.rangeSizeIndex (ls,us)
      in (size*subSize,
            \(Cons i is) -> Ix.index (l,u) i * subSize + subIndex is)
   indexHorner (Cons l ls, Cons u us) =
      let size = Ix.rangeSize (l,u)
      in \superOffset (Cons i is) ->
            C.indexHorner (ls,us) (superOffset*size + Ix.index (l,u) i) is

{-
GHC-7.10 requires FlexibleContexts extension for this instance.
-}
instance (C.Ix f, Ix.Ix i, Ord (f i)) => Ix.Ix (T f i) where
   range = C.range
   index = C.index
   inRange = C.inRange
   rangeSize = C.rangeSize
