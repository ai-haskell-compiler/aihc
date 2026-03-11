module Data.List.Match.Private where

import Data.Maybe    (fromJust, isNothing, )
import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (mapFst, forcePair, )
import Data.Bool.HT  (if', )

import qualified Data.List as List

import Control.Functor.HT (void, )

import Prelude hiding (take, drop, splitAt, replicate, )


-- $setup
-- >>> import qualified Data.List.Match.Private as Match
-- >>> import qualified Data.List as List
-- >>>
-- >>> import qualified Test.QuickCheck as QC
-- >>>
-- >>> newtype List = List [Integer] deriving (Show)
-- >>> instance QC.Arbitrary List where
-- >>>    arbitrary = fmap List QC.arbitrary
-- >>>    shrink (List xs) = map List $ QC.shrink xs
-- >>>
-- >>> newtype Shape = Shape [Ordering] deriving (Show)
-- >>> instance QC.Arbitrary Shape where
-- >>>    arbitrary = fmap Shape QC.arbitrary
-- >>>    shrink (Shape xs) = map Shape $ QC.shrink xs


{- | Make a list as long as another one

prop> \(Shape xs) (List ys) -> Match.take xs ys == List.take (length xs) ys
-}
{-
@flip (zipWith const)@ is not as lazy,
e.g. would be @take [] undefined = undefined@,
but it should be @take [] undefined = []@.
-}
take :: [b] -> [a] -> [a]
take = zipWith (flip const)

{- | Drop as many elements as the first list is long

prop> \(Shape xs) (List ys) -> Match.drop xs ys == List.drop (length xs) ys
prop> \(Shape xs) (List ys) -> Match.take xs ys ++ Match.drop xs ys == ys
-}
drop :: [b] -> [a] -> [a]
drop xs ys0 =
   foldl (\ys _ -> laxTail ys) ys0 xs


-- | prop> \(Shape xs) (List ys) -> Match.drop xs ys == dropRec xs ys
{-
Shares suffix with input,
that is it is more efficient than the implementations below.
-}
dropRec :: [b] -> [a] -> [a]
dropRec (_:xs) (_:ys) = dropRec xs ys
dropRec _ ys = ys

-- | prop> \(Shape xs) (List ys) -> Match.drop xs ys == drop0 xs ys
drop0 :: [b] -> [a] -> [a]
drop0 xs ys =
   -- catMaybes (
   map fromJust (dropWhile isNothing
      (zipWith (toMaybe . null) (iterate laxTail xs) ys))

-- | prop> \(Shape xs) (List ys) -> Match.drop xs ys == drop1 xs ys
drop1 :: [b] -> [a] -> [a]
drop1 xs ys =
   map snd (dropWhile (not . null . fst) (zip (iterate laxTail xs) ys))

-- | prop> \(Shape xs) (List ys) -> Match.drop xs ys == drop2 xs ys
drop2 :: [b] -> [a] -> [a]
drop2 xs ys =
   snd $ head $
   dropWhile (not . null . fst) $
   zip (iterate laxTail xs) (iterate laxTail ys)


{- |
>>> laxTail ""
""
>>> laxTail "a"
""
>>> laxTail "ab"
"b"
-}
laxTail :: [a] -> [a]
laxTail xt = case xt of [] -> []; _:xs -> xs

-- | prop> \(List xs) -> Match.laxTail xs == Match.laxTail0 xs
laxTail0 :: [a] -> [a]
laxTail0 = List.drop 1

{- |
prop> \(Shape xs) (List ys) -> Match.splitAt xs ys == (Match.take xs ys, Match.drop xs ys)
prop> \(Shape xs) (List ys) -> Match.splitAt xs ys == List.splitAt (length xs) ys
-}
splitAt :: [b] -> [a] -> ([a],[a])
splitAt nt xt =
   forcePair $
   case (nt,xt) of
      (_:ns, x:xs) -> mapFst (x:) $ splitAt ns xs
      (_, xs) -> ([],xs)


-- | prop> \(Shape xs) (List ys) -> Match.takeRev xs ys == reverse (Match.take xs (reverse ys))
takeRev :: [b] -> [a] -> [a]
takeRev ys xs = drop (drop ys xs) xs

-- | prop> \(Shape xs) (List ys) -> Match.dropRev xs ys == reverse (Match.drop xs (reverse ys))
dropRev :: [b] -> [a] -> [a]
dropRev ys xs = take (drop ys xs) xs

{- |
Check whether two lists with different element types have equal length.
It holds

prop> \(Shape xs) (List ys) -> equalLength xs ys == (length xs == length ys)

but 'equalLength' is more efficient.
-}
equalLength :: [a] -> [b] -> Bool
equalLength xs ys =
   void xs == void ys

{- |
Compare the length of two lists over different types.
It holds

prop> \(Shape xs) (List ys) -> compareLength xs ys == compare (length xs) (length ys)

but 'compareLength' is more efficient.
-}
compareLength :: [a] -> [b] -> Ordering
compareLength xs ys =
   compare (void xs) (void ys)

{- | this one uses explicit recursion

prop> \(Shape xs) (List ys) -> Match.compareLength xs ys == Match.compareLength0 xs ys
-}
compareLength0 :: [a] -> [b] -> Ordering
compareLength0 =
   let recourse (_:xs) (_:ys) = recourse xs ys
       recourse []     []     = EQ
       recourse (_:_)  []     = GT
       recourse []     (_:_)  = LT
   in  recourse

{- | strict comparison

prop> \(Shape xs) (List ys) -> Match.compareLength xs ys == Match.compareLength1 xs ys
-}
compareLength1 :: [a] -> [b] -> Ordering
compareLength1 xs ys =
   compare (length xs) (length ys)

{- |
@lessOrEqualLength x y@ is almost the same as @compareLength x y <= EQ@,
but

>>> lessOrEqualLength "" undefined
True

whereas @compareLength [] undefined <= EQ  =  undefined@.
-}
lessOrEqualLength :: [a] -> [b] -> Bool
lessOrEqualLength [] _ = True
lessOrEqualLength _ [] = False
lessOrEqualLength (_:xs) (_:ys) = lessOrEqualLength xs ys

{- |
Returns the shorter one of two lists.
It works also for infinite lists as much as possible.
E.g.

>>> shorterList (shorterList (repeat 'a') (repeat 'b')) "abc"
"abc"

The trick is, that the skeleton of the resulting list
is constructed using 'zipWith' without touching the elements.
The contents is then computed (only) if requested.
-}
shorterList :: [a] -> [a] -> [a]
shorterList xs ys =
   let useX = lessOrEqualLength xs ys
   in  zipWith (if' useX) xs ys

{- |
This is lazier than 'shorterList' in a different aspect:
It returns a common prefix
even if it is undefined, which list is the shorter one.
However, it requires a proper 'Eq' instance
and if elements are undefined, it may fail even earlier.

>>> List.take 3 $ shorterListEq ("abc" ++ repeat 'a') ("abcdef" ++ repeat 'b')
"abc"
-}
shorterListEq :: (Eq a) => [a] -> [a] -> [a]
shorterListEq xs ys =
   let useX = lessOrEqualLength xs ys
   in  zipWith (\x y -> if' (x==y || useX) x y) xs ys


{- |
Specialisation of 'Data.Functor.$>'.
-}
replicate :: [a] -> b -> [b]
replicate xs y =
   take xs (repeat y)
