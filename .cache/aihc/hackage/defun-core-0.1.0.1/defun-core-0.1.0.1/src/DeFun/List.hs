{-# LANGUAGE Trustworthy #-}
-- | List type-families.
--
-- For term-level reflections see [defun-sop package](https://hackage.haskell.org/package/defun-sop).
--
-- Implementation note: It would be great if first-order type families,
-- like 'Append' and 'Concat', were defined already in @base@,
-- e.g. in @Data.Type.List@ module.
-- Higher-order type families, like @Map@, obviously cannot be there
-- as they rely on the defunctorization machinery.
-- Yet, some first-order type families like 'Sequence' and 'Reverse'
-- may also be defined directly, but it's more convenient to define
-- them as special case of an higher-order type family ('Map2' and 'Foldl'
-- respectively), as that makes working with them more convenient.
--
module DeFun.List (
    -- * Append
    Append, AppendSym, AppendSym1,
    -- * Map
    Map, MapSym, MapSym1,
    -- * Concat
    Concat, ConcatSym,
    -- * ConcatMap
    ConcatMap, ConcatMapSym, ConcatMapSym1,
    -- * Map2
    Map2, Map2Sym, Map2Sym1, Map2Sym2,
    -- * Sequence
    Sequence, SequenceSym,
    -- * Foldr
    Foldr, FoldrSym, FoldrSym1, FoldrSym2,
    -- * Foldl
    Foldl, FoldlSym, FoldlSym1, FoldlSym2,
    -- * ZipWith
    ZipWith, ZipWithSym, ZipWithSym1, ZipWithSym2,
    -- * Filter
    Filter, FilterSym, FilterSym1,
    -- * Reverse
    Reverse, ReverseSym,
) where

import Prelude              (Bool (..))

import DeFun.Core
import DeFun.Function

-- $setup
-- >>> import Prelude (Bool (..), Char, Maybe (..), Int, String)
-- >>> import Data.Singletons.Bool (SBool (..))
-- >>> import Numeric.Natural (Natural)
-- >>> import DeFun
-- >>> :set -dppr-cols9999

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

-- | List append.
--
-- >>> :kind! Append [1, 2, 3] [4, 5, 6]
-- Append [1, 2, 3] [4, 5, 6] :: [Natural]
-- = [1, 2, 3, 4, 5, 6]
--
type Append :: [a] -> [a] -> [a]
type family Append xs ys where
    Append '[]       ys = ys
    Append (x ': xs) ys = x ': Append xs ys

type AppendSym :: [a] ~> [a] ~> [a]
data AppendSym xs
type instance App AppendSym xs = AppendSym1 xs

type AppendSym1 :: [a] -> [a] ~> [a]
data AppendSym1 xs ys
type instance App (AppendSym1 xs) ys = Append xs ys

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

-- | List map
--
-- >>> :kind! Map NotSym [True, False]
-- Map NotSym [True, False] :: [Bool]
-- = [False, True]
--
-- >>> :kind! Map (Con1 Just) [1, 2, 3]
-- Map (Con1 Just) [1, 2, 3] :: [Maybe Natural]
-- = [Just 1, Just 2, Just 3]
--
type Map :: (a ~> b) -> [a] -> [b]
type family Map f xs where
    Map f '[]    = '[]
    Map f (x:xs) = f @@ x : Map f xs

type MapSym :: (a ~> b) ~> [a] ~> [b]
data MapSym f
type instance App MapSym f = MapSym1 f

type MapSym1  :: (a ~> b) -> [a] ~> [b]
data MapSym1 f xs
type instance App (MapSym1 f) xs = Map f xs

-------------------------------------------------------------------------------
-- Concat
-------------------------------------------------------------------------------

-- | List concat
--
-- >>> :kind! Concat [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ]
-- Concat [ [1, 2, 3], [4, 5, 6], [7, 8, 9] ] :: [Natural]
-- = [1, 2, 3, 4, 5, 6, 7, 8, 9]
--
type Concat :: [[a]] -> [a]
type family Concat xss where
    Concat '[] = '[]
    Concat (xs : xss) = Append xs (Concat xss)

type ConcatSym :: [[a]] ~> [a]
data ConcatSym xss
type instance App ConcatSym xss = Concat xss

-------------------------------------------------------------------------------
-- ConcatMap
-------------------------------------------------------------------------------

-- | List concatMap
type ConcatMap :: (a ~> [b]) -> [a] -> [b]
type family ConcatMap f xs where
    ConcatMap f '[]    = '[]
    ConcatMap f (x:xs) = Append (f @@ x) (ConcatMap f xs)

type ConcatMapSym :: (a ~> [b]) ~> [a] ~> [b]
data ConcatMapSym f
type instance App ConcatMapSym f = ConcatMapSym1 f

type ConcatMapSym1  :: (a ~> [b]) -> [a] ~> [b]
data ConcatMapSym1 f xs
type instance App (ConcatMapSym1 f) xs = ConcatMap f xs

-------------------------------------------------------------------------------
-- Map2
-------------------------------------------------------------------------------

-- | List binary map. I.e. 'liftA2' for lists.
--
-- Note: this is not 'ZipWith'.
--
-- >>> :kind! Map2 (Con2 '(,)) [1, 2, 3] ['x', 'y']
-- Map2 (Con2 '(,)) [1, 2, 3] ['x', 'y'] :: [(Natural, Char)]
-- = ['(1, 'x'), '(1, 'y'), '(2, 'x'), '(2, 'y'), '(3, 'x'), '(3, 'y')]
--
-- This function is a good example to highlight how to defunctionalize
-- definitions with anonymous functions.
--
-- The simple definition can be written using @concatMap@ and @map@ from
-- "Prelude":
--
-- >>> import Prelude as P (concatMap, map, (.), flip)
-- >>> let map2 f xs ys = P.concatMap (\x -> P.map (f x) ys) xs
-- >>> map2 (,) "abc" "xy"
-- [('a','x'),('a','y'),('b','x'),('b','y'),('c','x'),('c','y')]
--
-- However, to make it easier (arguably) to defunctionalize, the @concatMap@ argument
-- lambda can be written in point-free form using combinators:
--
-- >>> let map2 f xs ys = P.concatMap (P.flip P.map ys P.. f) xs
-- >>> map2 (,) "abc" "xy"
-- [('a','x'),('a','y'),('b','x'),('b','y'),('c','x'),('c','y')]
--
-- Alternatively, we could define a new "top-level" function
--
-- >>> let map2Aux f ys x = P.map (f x) ys
--
-- and use it to define @map2:
--
-- >>> let map2 f xs ys = P.concatMap (map2Aux f ys) xs
-- >>> map2 (,) "abc" "xy"
-- [('a','x'),('a','y'),('b','x'),('b','y'),('c','x'),('c','y')]
--
type Map2 :: (a ~> b ~> c) -> [a] -> [b] -> [c]
type family Map2 f xs ys where
    Map2 f xs ys = ConcatMap (CompSym2 (FlipSym2 MapSym ys) f) xs

type Map2Sym :: (a ~> b ~> c) ~> [a] ~> [b] ~> [c]
data Map2Sym f
type instance App Map2Sym f = Map2Sym1 f

type Map2Sym1 :: (a ~> b ~> c) -> [a] ~> [b] ~> [c]
data Map2Sym1 f xs
type instance App (Map2Sym1 f) xs = Map2Sym2 f xs

type Map2Sym2 :: (a ~> b ~> c) -> [a] -> [b] ~> [c]
data Map2Sym2 f xs ys
type instance App (Map2Sym2 f xs) ys = Map2 f xs ys

-------------------------------------------------------------------------------
-- Sequence
-------------------------------------------------------------------------------

-- | List sequence
--
-- >>> :kind! Sequence [[1,2,3],[4,5,6]]
-- Sequence [[1,2,3],[4,5,6]] :: [[Natural]]
-- = [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4], [3, 5], [3, 6]]
--
type Sequence :: [[a]] -> [[a]]
type family Sequence xss where
    Sequence '[]         = '[ '[] ]
    Sequence (xs ': xss) = Map2 (Con2 '(:)) xs (Sequence xss)

type SequenceSym :: [[a]] ~> [[a]]
data SequenceSym xss
type instance App SequenceSym xss = Sequence xss

-------------------------------------------------------------------------------
-- Foldr
-------------------------------------------------------------------------------

-- | List right fold
--
-- Using 'Foldr' we can define a @Curry@ type family:
--
-- >>> type Curry args res = Foldr (Con2 (->)) args res
-- >>> :kind! Curry String [Int, Bool]
-- Curry String [Int, Bool] :: *
-- = Int -> Bool -> [Char]
--
type Foldr :: (a ~> b ~> b) -> b -> [a] -> b
type family Foldr f z xs where
    Foldr f z '[]      = z
    Foldr f z (x : xs) = f @@ x @@ (Foldr f z xs)

type FoldrSym :: (a ~> b ~> b) ~> b ~> [a] ~> b
data FoldrSym f
type instance App FoldrSym f = FoldrSym1 f

type FoldrSym1 :: (a ~> b ~> b) -> b ~> [a] ~> b
data FoldrSym1 f z
type instance App (FoldrSym1 f) z = FoldrSym2 f z

type FoldrSym2 :: (a ~> b ~> b) -> b -> [a] ~> b
data FoldrSym2 f z xs
type instance App (FoldrSym2 f z) xs = Foldr f z xs

-------------------------------------------------------------------------------
-- Foldl
-------------------------------------------------------------------------------

-- | List left fold
--
type Foldl :: (b ~> a ~> b) -> b -> [a] -> b
type family Foldl f z xs where
    Foldl f z '[]      = z
    Foldl f z (x : xs) = Foldl f (f @@ z @@ x) xs

type FoldlSym :: (b ~> a ~> b) ~> b ~> [a] ~> b
data FoldlSym f
type instance App FoldlSym f = FoldlSym1 f

type FoldlSym1 :: (b ~> a ~> b) -> b ~> [a] ~> b
data FoldlSym1 f z
type instance App (FoldlSym1 f) z = FoldlSym2 f z

type FoldlSym2 :: (b ~> a ~> b) -> b -> [a] ~> b
data FoldlSym2 f z xs
type instance App (FoldlSym2 f z) xs = Foldl f z xs

-------------------------------------------------------------------------------
-- ZipWith
-------------------------------------------------------------------------------

-- | Zip with
--
-- >>> :kind! ZipWith (Con2 '(,)) [1, 2, 3] ['x', 'y']
-- ZipWith (Con2 '(,)) [1, 2, 3] ['x', 'y'] :: [(Natural, Char)]
-- = ['(1, 'x'), '(2, 'y')]
--
type ZipWith :: (a ~> b ~> c) -> [a] -> [b] -> [c]
type family ZipWith f xs ys where
    ZipWith f '[] ys = '[]
    ZipWith f (x : xs) '[] = '[]
    ZipWith f (x : xs) (y : ys) = f @@ x @@ y : ZipWith f xs ys

type ZipWithSym :: (a ~> b ~> c) ~> [a] ~> [b] ~> [c]
data ZipWithSym f
type instance App ZipWithSym f = ZipWithSym1 f

type ZipWithSym1 :: (a ~> b ~> c) -> [a] ~> [b] ~> [c]
data ZipWithSym1 f xs
type instance App (ZipWithSym1 f) xs = ZipWithSym2 f xs

type ZipWithSym2 :: (a ~> b ~> c) -> [a] -> [b] ~> [c]
data ZipWithSym2 f xs ys
type instance App (ZipWithSym2 f xs) ys = ZipWith f xs ys

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

-- | Filter list
type Filter :: (a ~> Bool) -> [a] -> [a]
type family Filter p xs where
    Filter f '[] = '[]
    Filter f (x ': xs) = FilterAux (f @@  x) x f xs

type FilterAux :: Bool -> a -> (a ~> Bool) -> [a] -> [a]
type family FilterAux b x p xs where
    FilterAux 'True  x p xs = x ': Filter p xs
    FilterAux 'False x p xs =      Filter p xs

type FilterSym :: (a ~> Bool) ~> [a] ~> [a]
data FilterSym p
type instance App FilterSym p = FilterSym1 p

type FilterSym1  :: (a ~> Bool) -> [a] ~> [a]
data FilterSym1 p xs
type instance App (FilterSym1 p) xs = Filter p xs

-------------------------------------------------------------------------------
-- Reverse
-------------------------------------------------------------------------------

-- | Reverse list
--
-- >>> :kind! Reverse [1,2,3,4]
-- Reverse [1,2,3,4] :: [Natural]
-- = [4, 3, 2, 1]
--
type Reverse :: [a] -> [a]
type family Reverse xs where
    Reverse xs = Foldl (FlipSym1 (Con2 '(:))) '[] xs

type ReverseSym :: [a] ~> [a]
data ReverseSym xs
type instance App ReverseSym xs = Reverse xs
