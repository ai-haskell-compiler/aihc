module Data.List.HT.Private where

import Data.List  as List  (find, transpose, unfoldr, isPrefixOf,
                            findIndices, foldl', mapAccumL, )
import Data.Maybe as Maybe (fromMaybe, catMaybes, isJust, mapMaybe, )
import Data.Maybe.HT       (toMaybe, )
import Control.Monad.HT    ((<=<), )
import Control.Monad       (guard, msum, mplus, liftM2, )
import Control.Applicative ((<$>), (<*>), )
import Data.Tuple.HT       (mapPair, mapFst, mapSnd, forcePair, swap, )

import qualified Control.Functor.HT as Func

import qualified Data.List.Key.Private   as Key
import qualified Data.List.Match.Private as Match
import qualified Data.List.Reverse.StrictElement as Rev

import Prelude hiding (unzip, break, span, )


-- $setup
-- >>> import qualified Test.QuickCheck as QC
-- >>> import Test.Utility (forAllPredicates)
-- >>> import Test.QuickCheck (NonNegative(NonNegative), Positive(Positive), NonEmptyList(NonEmpty))
-- >>> import qualified Data.List as List
-- >>> import Data.List (transpose)
-- >>> import Data.Maybe.HT (toMaybe)
-- >>> import Data.Maybe (mapMaybe, isNothing)
-- >>> import Data.Char (isLetter, isUpper, toUpper)
-- >>> import Data.Eq.HT (equating)
-- >>> import Control.Monad (liftM2)
-- >>>
-- >>> divMaybe :: Int -> Int -> Maybe Int
-- >>> divMaybe m n = case divMod n m of (q,0) -> Just q; _ -> Nothing
-- >>>
-- >>> forAllMaybeFn :: (QC.Testable test) => ((Int -> Maybe Int) -> test) -> QC.Property
-- >>> forAllMaybeFn prop = QC.forAll (QC.choose (1,4)) $ prop . divMaybe


-- * Improved standard functions

{- |
This function is lazier than the one suggested in the Haskell 98 report.
It is @inits undefined = [] : undefined@,
in contrast to @Data.List.inits undefined = undefined@.
-}
{-
suggested in
<http://www.haskell.org/pipermail/libraries/2014-July/023291.html>
-}
inits :: [a] -> [[a]]
inits = map reverse . scanl (flip (:)) []

{- |
As lazy as 'inits' but less efficient because of repeated 'map'.
-}
initsLazy :: [a] -> [[a]]
initsLazy xt =
   [] :
   case xt of
      [] -> []
      x:xs -> map (x:) (initsLazy xs)

{- |
Suggested implementation in the Haskell 98 report.
It is not as lazy as possible.
-}
inits98 :: [a] -> [[a]]
inits98 []     = [[]]
inits98 (x:xs) = [[]] ++ map (x:) (inits98 xs)

inits98' :: [a] -> [[a]]
inits98' =
   foldr (\x prefixes -> [] : map (x:) prefixes) [[]]


{- |
This function is lazier than the one suggested in the Haskell 98 report.
It is @tails undefined = ([] : undefined) : undefined@,
in contrast to @Data.List.tails undefined = undefined@.
-}
tails :: [a] -> [[a]]
tails xt =
   uncurry (:) $
   case xt of
      [] -> ([],[])
      _:xs -> (xt, tails xs)

tails' :: [a] -> [[a]]
tails' = fst . breakAfter null . iterate tail

tails98            :: [a] -> [[a]]
tails98 []         = [[]]
tails98 xxs@(_:xs) = xxs : tails98 xs

{- |
This function compares adjacent elements of a list.
If two adjacent elements satisfy a relation then they are put into the same sublist.
Example:

>>> groupBy (<) "abcdebcdef"
["abcde","bcdef"]

In contrast to that 'Data.List.groupBy' compares
the head of each sublist with each candidate for this sublist.
This yields

>>> List.groupBy (<) "abcdebcdef"
["abcdebcdef"]

The second @'b'@ is compared with the leading @'a'@.
Thus it is put into the same sublist as @'a'@.

The sublists are never empty.
Thus the more precise result type would be @[(a,[a])]@.
-}
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = Key.groupBy

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)


{- |
Like standard 'unzip' but more lazy.
It is @Data.List.unzip undefined == undefined@,
but @unzip undefined == (undefined, undefined)@.
-}
unzip :: [(a,b)] -> ([a],[b])
unzip =
   forcePair .
   foldr (\ (x,y) ~(xs,ys) -> (x:xs,y:ys)) ([],[])


{- |
'Data.List.partition' of GHC 6.2.1 fails on infinite lists.
But this one does not.
-}
{-
The lazy pattern match @(y,z)@ is necessary
since otherwise it fails on infinite lists.
-}
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p =
   forcePair .
   foldr
      (\x ~(y,z) ->
         if p x
           then (x : y, z)
           else (y, x : z))
      ([],[])

{- |
It is @Data.List.span f undefined = undefined@,
whereas @span f undefined = (undefined, undefined)@.
-}
span, break :: (a -> Bool) -> [a] -> ([a],[a])
span p =
   let recourse xt =
          forcePair $
          fromMaybe ([],xt) $
          do (x,xs) <- viewL xt
             guard $ p x
             return $ mapFst (x:) $ recourse xs
   in  recourse

break p =  span (not . p)



-- * Split

{- |
Split the list at the occurrences of a separator into sub-lists.
Remove the separators.
This is somehow a generalization of 'lines' and 'words'.
But note the differences:

>>> words "a  a"
["a","a"]
>>> chop (' '==) "a  a"
["a","","a"]

>>> lines "a\n\na"
["a","","a"]
>>> chop ('\n'==) "a\n\na"
["a","","a"]

>>> lines "a\n"
["a"]
>>> chop ('\n'==) "a\n"
["a",""]
-}
chop :: (a -> Bool) -> [a] -> [[a]]
chop p =
   uncurry (:) .
   foldr (\ x ~(y,ys) -> if p x then ([],y:ys) else ((x:y),ys) ) ([],[])

chop' :: (a -> Bool) -> [a] -> [[a]]
chop' p =
   let recourse =
          uncurry (:) .
          mapSnd (switchL [] (const recourse)) .
          break p
   in  recourse


chopAtRun :: (a -> Bool) -> [a] -> [[a]]
chopAtRun p =
   let recourse [] = [[]]
       recourse y =
          let (z,zs) = break p (dropWhile p y)
          in z : recourse zs
   in  recourse


{- |
Like 'break', but splits after the matching element.

prop> forAllPredicates $ \p xs -> uncurry (++) (breakAfter p xs) == xs
-}
breakAfter :: (a -> Bool) -> [a] -> ([a], [a])
breakAfter = breakAfterRec

breakAfterRec :: (a -> Bool) -> [a] -> ([a], [a])
breakAfterRec p =
   let recourse [] = ([],[])
       recourse (x:xs) =
          mapFst (x:) $
          if p x
            then ([],xs)
            else recourse xs
   in  forcePair . recourse

{-
The use of 'foldr' might allow for fusion,
but unfortunately this simple implementation would copy the tail of the list.
-}
-- | prop> forAllPredicates $ \p xs -> breakAfterRec p xs == breakAfterFoldr p xs
breakAfterFoldr :: (a -> Bool) -> [a] -> ([a], [a])
breakAfterFoldr p =
   forcePair .
   foldr
      (\x yzs -> mapFst (x:) $ if p x then ([], uncurry (++) yzs) else yzs)
      ([],[])

-- | prop> forAllPredicates $ \p xs -> breakAfterRec p xs == breakAfterBreak p xs
breakAfterBreak :: (a -> Bool) -> [a] -> ([a], [a])
breakAfterBreak p xs =
   case break p xs of
      (ys, []) -> (ys, [])
      (ys, z:zs) -> (ys++[z], zs)

-- | prop> forAllPredicates $ \p xs -> breakAfterRec p xs == breakAfterTakeUntil p xs
breakAfterTakeUntil :: (a -> Bool) -> [a] -> ([a], [a])
breakAfterTakeUntil p xs =
   forcePair $
   (\ys -> (map fst ys, maybe [] (snd . snd) $ viewR ys)) $
   takeUntil (p . fst) $ zip xs $ tail $ tails xs

{- |
Take all elements until one matches.
The matching element is returned, too.
This is the key difference to @takeWhile (not . p)@.
It holds:

prop> forAllPredicates $ \p xs -> takeUntil p xs == fst (breakAfter p xs)
-}
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x ys -> x : if p x then [] else ys) []


{- |
Split the list after each occurence of a terminator.
Keep the terminator.
There is always a list for the part after the last terminator.
It may be empty.
See package @non-empty@ for more precise result type.

prop> forAllPredicates $ \p xs -> concat (segmentAfter p xs) == xs
prop> forAllPredicates $ \p xs -> length (filter p xs) == length (tail (segmentAfter p xs))
prop> forAllPredicates $ \p -> all (p . last) . init . segmentAfter p
prop> forAllPredicates $ \p -> all (all (not . p) . init) . init . segmentAfter p

This test captures both infinitely many groups and infinitely big groups:

prop> forAllPredicates $ \p x -> flip seq True . (!!100) . concat . segmentAfter p . cycle . (x:)
-}
segmentAfter :: (a -> Bool) -> [a] -> [[a]]
segmentAfter p =
   uncurry (:) .
   foldr
      (\x ~(y,ys) ->
         mapFst (x:) $
         if p x then ([],y:ys) else (y,ys))
      ([],[])

segmentAfter' :: (a -> Bool) -> [a] -> [[a]]
segmentAfter' p =
   foldr (\ x ~yt@(y:ys) -> if p x then [x]:yt else (x:y):ys) [[]]

{- |
Split the list before each occurence of a leading character.
Keep these characters.
There is always a list for the part before the first leading character.
It may be empty.
See package @non-empty@ for more precise result type.

>>> segmentBefore isUpper "AbcdXyz"
["","Abcd","Xyz"]
>>> segmentBefore isUpper "kAbcdXYZ"
["k","Abcd","X","Y","Z"]

prop> forAllPredicates $ \p xs -> concat (segmentBefore p xs) == xs
prop> forAllPredicates $ \p xs -> length (filter p xs) == length (tail (segmentBefore p xs))
prop> forAllPredicates $ \p -> all (p . head) . tail . segmentBefore p
prop> forAllPredicates $ \p -> all (all (not . p) . tail) . tail . segmentBefore p
prop> forAllPredicates $ \p x -> flip seq True . (!!100) . concat . segmentBefore p . cycle . (x:)
-}
segmentBefore :: (a -> Bool) -> [a] -> [[a]]
segmentBefore p =
--   foldr (\ x ~(y:ys) -> (if p x then ([]:) else id) ((x:y):ys)) [[]]
   uncurry (:) .
   foldr
      (\ x ~(y,ys) ->
         let xs = x:y
         in  if p x then ([],xs:ys) else (xs,ys))
      ([],[])

-- | prop> forAllPredicates $ \p xs -> segmentBefore p xs == segmentBefore' p xs
segmentBefore' :: (a -> Bool) -> [a] -> [[a]]
segmentBefore' p =
   uncurry (:) .
   (\xst ->
      fromMaybe ([],xst) $ do
         ((x:xs):xss) <- Just xst
         guard $ not $ p x
         return (x:xs, xss)) .
   groupBy (\_ x -> not $ p x)

-- | prop> forAllPredicates $ \p xs -> segmentBefore p xs == segmentBefore'' p xs
segmentBefore'' :: (a -> Bool) -> [a] -> [[a]]
segmentBefore'' p =
   (\xst ->
      case xst of
         ~(xs:xss) ->
            tail xs : xss) .
   groupBy (\_ x -> not $ p x) .
   (error "segmentBefore: dummy element" :)


{- |
>>> segmentBeforeJust (\c -> toMaybe (isLetter c) (toUpper c)) "123a5345b---"
("123",[('A',"5345"),('B',"---")])
-}
segmentBeforeJust ::
   (a -> Maybe b) ->
   [a] -> ([a], [(b, [a])])
segmentBeforeJust f =
   forcePair .
   foldr
      (\ x ~(y,ys) ->
         case f x of
            Just b -> ([],(b,y):ys)
            Nothing -> (x:y,ys))
      ([],[])

{- |
>>> segmentAfterJust (\c -> toMaybe (isLetter c) (toUpper c)) "123a5345b---"
([("123",'A'),("5345",'B')],"---")
-}
segmentAfterJust ::
   (a -> Maybe b) ->
   [a] -> ([([a], b)], [a])
segmentAfterJust f =
   swap .
   uncurry (mapAccumL (\as0 (b,as1) -> (as1, (as0,b)))) .
   segmentBeforeJust f


{- |
>>> segmentBeforeRight [Left 'a', Right LT, Right GT, Left 'b']
("a",[(LT,""),(GT,"b")])

prop> forAllMaybeFn $ \f xs -> segmentBeforeJust f xs == segmentBeforeRight (map (\x -> maybe (Left x) Right (f x)) xs)
-}
segmentBeforeRight ::
   [Either a b] -> ([a], [(b, [a])])
segmentBeforeRight =
   forcePair .
   foldr
      (\ x ~(y,ys) ->
         case x of
            Right b -> ([],(b,y):ys)
            Left a -> (a:y,ys))
      ([],[])

{- |
>>> segmentAfterRight [Left 'a', Right LT, Right GT, Left 'b']
([("a",LT),("",GT)],"b")

prop> forAllMaybeFn $ \f xs -> segmentAfterJust f xs == segmentAfterRight (map (\x -> maybe (Left x) Right (f x)) xs)
-}
segmentAfterRight ::
   [Either a b] -> ([([a], b)], [a])
segmentAfterRight =
   swap .
   uncurry (mapAccumL (\as0 (b,as1) -> (as1, (as0,b)))) .
   segmentBeforeRight


-- cf. Matroid.hs
{- |
@removeEach xs@ represents a list of sublists of @xs@,
where each element of @xs@ is removed and
the removed element is separated.
It seems to be much simpler to achieve with
@zip xs (map (flip List.delete xs) xs)@,
but the implementation of 'removeEach' does not need the 'Eq' instance
and thus can also be used for lists of functions.

See also the proposal
 <http://www.haskell.org/pipermail/libraries/2008-February/009270.html>

>>> removeEach "abc"
[('a',"bc"),('b',"ac"),('c',"ab")]
>>> removeEach "a"
[('a',"")]
>>> removeEach ""
[]
-}
removeEach :: [a] -> [(a, [a])]
removeEach =
   map (\(ys, pivot, zs) -> (pivot,ys++zs)) . splitEverywhere

{- |
>>> splitEverywhere "abc"
[("",'a',"bc"),("a",'b',"c"),("ab",'c',"")]
>>> splitEverywhere "a"
[("",'a',"")]
>>> splitEverywhere ""
[]
-}
splitEverywhere :: [a] -> [([a], a, [a])]
splitEverywhere xs =
   map
      (\(y, zs0) ->
         case zs0 of
            z:zs -> (y,z,zs)
            [] -> error "splitEverywhere: empty list")
      (init (zip (inits xs) (tails xs)))



--  * inspect ends of a list

{-# DEPRECATED splitLast "use viewR instead" #-}
{- |
It holds @splitLast xs == (init xs, last xs)@,
but 'splitLast' is more efficient
if the last element is accessed after the initial ones,
because it avoids memoizing list.

prop> \(NonEmpty xs) -> splitLast (xs::String)  ==  (init xs, last xs)
-}
splitLast :: [a] -> ([a], a)
splitLast [] = error "splitLast: empty list"
splitLast [x] = ([], x)
splitLast (x:xs) =
   let (xs', lastx) = splitLast xs in (x:xs', lastx)


{- |
Should be prefered to 'head' and 'tail'.
-}
{-# INLINE viewL #-}
viewL :: [a] -> Maybe (a, [a])
viewL (x:xs) = Just (x,xs)
viewL []     = Nothing

{- |
Should be prefered to 'init' and 'last'.

prop> \xs -> maybe True ((init xs, last xs) == ) (viewR (xs::String))
-}
viewR :: [a] -> Maybe ([a], a)
viewR =
   foldr (\x -> Just . forcePair . maybe ([],x) (mapFst (x:))) Nothing

{- |
Should be prefered to 'head' and 'tail'.
-}
{-# INLINE switchL #-}
switchL :: b -> (a -> [a] -> b) -> [a] -> b
switchL n _ [] = n
switchL _ j (x:xs) = j x xs

switchL' :: b -> (a -> [a] -> b) -> [a] -> b
switchL' n j =
   maybe n (uncurry j) . viewL

{- |
Should be prefered to 'init' and 'last'.

prop> \xs -> switchR True (\ixs lxs -> ixs == init xs && lxs == last xs) (xs::String)
-}
{-# INLINE switchR #-}
switchR :: b -> ([a] -> a -> b) -> [a] -> b
switchR n j =
   maybe n (uncurry j) . viewR


-- * List processing starting at the end

{- |
@takeRev n@ is like @reverse . take n . reverse@
but it is lazy enough to work for infinite lists, too.

prop> \n xs -> takeRev n (xs::String) == reverse (take n (reverse xs))
-}
takeRev :: Int -> [a] -> [a]
takeRev n xs = Match.drop (drop n xs) xs

{- |
@dropRev n@ is like @reverse . drop n . reverse@
but it is lazy enough to work for infinite lists, too.

prop> \n xs -> dropRev n (xs::String) == reverse (drop n (reverse xs))
-}
dropRev :: Int -> [a] -> [a]
dropRev n xs = Match.take (drop n xs) xs

{- |
@splitAtRev n xs == (dropRev n xs, takeRev n xs)@.

prop> \n xs -> splitAtRev n (xs::String) == (dropRev n xs, takeRev n xs)
prop> \n xs -> (xs::String) == uncurry (++) (splitAtRev n xs)
-}
splitAtRev :: Int -> [a] -> ([a], [a])
splitAtRev n xs = Match.splitAt (drop n xs) xs


-- * List processing with Maybe and Either

{- |
@maybePrefixOf xs ys@ is @Just zs@ if @xs@ is a prefix of @ys@,
where @zs@ is @ys@ without the prefix @xs@.
Otherwise it is @Nothing@.
It is the same as 'Data.List.stripPrefix'.

>>> maybePrefixOf "abc" "abcdef"
Just "def"
>>> maybePrefixOf "def" "abcdef"
Nothing
-}
maybePrefixOf :: Eq a => [a] -> [a] -> Maybe [a]
maybePrefixOf (x:xs) (y:ys) = guard (x==y) >> maybePrefixOf xs ys
maybePrefixOf [] ys = Just ys
maybePrefixOf _  [] = Nothing

{- |
>>> maybeSuffixOf "abc" "abcdef"
Nothing
>>> maybeSuffixOf "def" "abcdef"
Just "abc"
-}
maybeSuffixOf :: Eq a => [a] -> [a] -> Maybe [a]
maybeSuffixOf xs ys =
   fmap reverse $ maybePrefixOf (reverse xs) (reverse ys)


{- |
Partition a list into elements which evaluate to @Just@ or @Nothing@ by @f@.

prop> forAllMaybeFn $ \f xs -> partitionMaybe f xs == (mapMaybe f xs, filter (isNothing . f) xs)
prop> forAllPredicates $ \p xs -> partition p xs == partitionMaybe (\x -> toMaybe (p x) x) xs
-}
partitionMaybe :: (a -> Maybe b) -> [a] -> ([b], [a])
partitionMaybe f =
   forcePair .
   foldr
      (\x -> maybe (mapSnd (x:)) (\y -> mapFst (y:)) (f x))
      ([],[])

{- |
This is the cousin of 'takeWhile'
analogously to 'catMaybes' being the cousin of 'filter'.

>>> takeWhileJust [Just 'a', Just 'b', Nothing, Just 'c']
"ab"

Example: Keep the heads of sublists until an empty list occurs.

>>> takeWhileJust $ map (fmap fst . viewL) ["abc","def","","xyz"]
"ad"

For consistency with 'takeWhile',
'partitionMaybe' and 'dropWhileNothing' it should have been:

> takeWhileJust_ :: (a -> Maybe b) -> a -> [b]

However, both variants are interchangeable:

> takeWhileJust_ f == takeWhileJust . map f
> takeWhileJust == takeWhileJust_ id
-}
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust =
   foldr (\x acc -> maybe [] (:acc) x) []

dropWhileNothing :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
dropWhileNothing f =
   msum . map (Func.mapFst f <=< viewL) . tails

-- | prop> forAllMaybeFn $ \f xs -> dropWhileNothing f xs == dropWhileNothingRec f xs
dropWhileNothingRec :: (a -> Maybe b) -> [a] -> Maybe (b, [a])
dropWhileNothingRec f =
   let go [] = Nothing
       go (a:xs) = (flip (,) xs <$> f a) `mplus` go xs
   in  go

-- | prop> forAllMaybeFn $ \f xs -> snd (breakJust f xs) == dropWhileNothing f xs
breakJust :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakJust f =
   let go [] = ([], Nothing)
       go (a:xs) =
         case f a of
            Nothing -> mapFst (a:) $ go xs
            Just b -> ([], Just (b, xs))
   in  go

-- memory leak, because xs is hold all the time
-- | prop> forAllMaybeFn $ \f xs -> breakJust f xs == breakJustRemoveEach f xs
breakJustRemoveEach :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakJustRemoveEach f xs =
   switchL (xs, Nothing) const $
   mapMaybe (\(ys,a,zs) -> (\b -> (ys, Just (b,zs))) <$> f a) $
   splitEverywhere xs

-- needs to apply 'f' twice at the end and uses partial functions
-- | prop> forAllMaybeFn $ \f xs -> breakJust f xs == breakJustPartial f xs
breakJustPartial :: (a -> Maybe b) -> [a] -> ([a], Maybe (b, [a]))
breakJustPartial f xs =
   let (ys,zs) = break (isJust . f) xs
   in  (ys,
        mapFst (maybe (error "breakJust: unexpected Nothing") id . f) <$>
            viewL zs)

spanJust :: (a -> Maybe b) -> [a] -> ([b], [a])
spanJust f =
   let go [] = ([], [])
       go xt@(a:xs) =
         case f a of
            Just b -> mapFst (b:) $ go xs
            Nothing -> ([], xt)
   in  go


unzipEithers :: [Either a b] -> ([a], [b])
unzipEithers =
   forcePair .
   foldr (either (\x -> mapFst (x:)) (\y -> mapSnd (y:))) ([],[])


-- * Sieve and slice

{- | keep every k-th value from the list

>>> sieve 6 ['a'..'z']
"agmsy"
-}
sieve, sieve', sieve'', sieve''' :: Int -> [a] -> [a]
sieve k =
   unfoldr (\xs -> toMaybe (not (null xs)) (head xs, drop k xs))

-- | prop> \(Positive n) xs -> sieve n xs == sieve' n (xs::String)
sieve' k = map head . sliceVertical k

-- | prop> \(Positive n) xs -> sieve n xs == sieve'' n (xs::String)
sieve'' k x = map (x!!) [0,k..(length x-1)]

-- | prop> \(Positive n) xs -> sieve n xs == sieve''' n (xs::String)
sieve''' k = map head . takeWhile (not . null) . iterate (drop k)


{-
sliceHorizontal is faster than sliceHorizontal' but consumes slightly more memory
(although it needs no swapping)
-}
{- |
>>> sliceHorizontal 6 ['a'..'z']
["agmsy","bhntz","ciou","djpv","ekqw","flrx"]

prop> \(NonEmpty xs) -> QC.forAll (QC.choose (1, length xs)) $ \n -> sliceHorizontal n xs == transpose (sliceVertical n (xs::String))
prop> \(NonEmpty xs) -> QC.forAll (QC.choose (1, length xs)) $ \n -> sliceVertical  n xs == transpose (sliceHorizontal n (xs::String))

The properties do not hold for empty lists because of:

>>> sliceHorizontal 4 ([]::[Int])
[[],[],[],[]]
-}
sliceHorizontal, sliceHorizontal', sliceHorizontal'', sliceHorizontal''' ::
   Int -> [a] -> [[a]]
sliceHorizontal n =
   map (sieve n) . take n . iterate (drop 1)

-- | prop> \(NonNegative n) xs -> sliceHorizontal n xs == sliceHorizontal' n (xs::String)
sliceHorizontal' n =
   foldr (\x ys -> let y = last ys in Match.take ys ((x:y):ys)) (replicate n [])

-- | prop> \(Positive n) xs -> sliceHorizontal n xs == sliceHorizontal'' n (xs::String)
sliceHorizontal'' n =
   reverse . foldr (\x ~(y:ys) -> ys ++ [x:y]) (replicate n [])

sliceHorizontal''' n =
   take n . transpose . takeWhile (not . null) . iterate (drop n)


{- |
>>> sliceVertical 6 ['a'..'z']
["abcdef","ghijkl","mnopqr","stuvwx","yz"]
-}
sliceVertical, sliceVertical' :: Int -> [a] -> [[a]]
sliceVertical n =
   map (take n) . takeWhile (not . null) . iterate (drop n)
      {- takeWhile must be performed before (map take)
         in order to handle (n==0) correctly -}

-- | prop> \(NonNegative n) xs -> equating (take 100000) (sliceVertical n xs) (sliceVertical' n (xs::String))
sliceVertical' n =
   unfoldr (\x -> toMaybe (not (null x)) (splitAt n x))




-- * Search&replace

search :: (Eq a) => [a] -> [a] -> [Int]
search sub str = findIndices (isPrefixOf sub) (tails str)

{- |
prop> \(NonEmpty xs) ys -> replace xs xs ys == (ys::String)
prop> \(NonEmpty xs) (NonEmpty ys) -> equating (take 1000) (replace xs ys (cycle xs)) (cycle (ys::String))
-}
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace src dst =
   let recourse [] = []
       recourse str@(s:ss) =
          fromMaybe
             (s : recourse ss)
             (fmap ((dst++) . recourse) $
              maybePrefixOf src str)
   in  recourse

markSublists :: (Eq a) => [a] -> [a] -> [Maybe [a]]
markSublists sub ys =
   let ~(hd', rest') =
          foldr (\c ~(hd, rest) ->
                   let xs = c:hd
                   in  case maybePrefixOf sub xs of
                         Just suffix -> ([], Nothing : Just suffix : rest)
                         Nothing -> (xs, rest)) ([],[]) ys
   in  Just hd' : rest'

replace' :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace' src dst xs =
   concatMap (fromMaybe dst) (markSublists src xs)

{- | This is slightly wrong, because it re-replaces things.
     That's also the reason for inefficiency:
        The replacing can go on only when subsequent replacements are finished.
     Thus this functiob fails on infinite lists. -}
replace'' :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace'' src dst =
    foldr (\x xs -> let y=x:xs
                    in  if isPrefixOf src y
                          then dst ++ drop (length src) y
                          else y) []

{- |
prop \src dst xs -> replace src dst xs == multiReplace [(src,dst)] (xs::String)
-}
multiReplace :: Eq a => [([a], [a])] -> [a] -> [a]
multiReplace dict =
   let recourse [] = []
       recourse str@(s:ss) =
          fromMaybe
             (s : recourse ss)
             (msum $
              map (\(src,dst) ->
                      fmap ((dst++) . recourse) $
                      maybePrefixOf src str) dict)
   in  recourse

multiReplace' :: Eq a => [([a], [a])] -> [a] -> [a]
multiReplace' dict =
   let recourse [] = []
       recourse str@(s:ss) =
          maybe
             (s : recourse ss)
             (\(src, dst) -> dst ++ recourse (Match.drop src str))
             (find (flip isPrefixOf str . fst) dict)
   in  recourse


-- * Lists of lists

{- |
Transform

> [[00,01,02,...],          [[00],
>  [10,11,12,...],   -->     [10,01],
>  [20,21,22,...],           [20,11,02],
>  ...]                      ...]

With @concat . shear@ you can perform a Cantor diagonalization,
that is an enumeration of all elements of the sub-lists
where each element is reachable within a finite number of steps.
It is also useful for polynomial multiplication (convolution).
-}
shear :: [[a]] -> [[a]]
shear =
   map catMaybes .
   shearTranspose .
   transposeFill

transposeFill :: [[a]] -> [[Maybe a]]
transposeFill =
   unfoldr (\xs ->
      toMaybe (not (null xs))
         (mapSnd (Rev.dropWhile null) $ unzipCons xs))

unzipCons :: [[a]] -> ([Maybe a], [[a]])
unzipCons =
   unzip .
   map ((\my -> (fmap fst my, maybe [] snd my)) . viewL)

{- |
It's somehow inverse to zipCons,
but the difficult part is,
that a trailing empty list on the right side is suppressed.
-}
unzipConsSkew :: [[a]] -> ([Maybe a], [[a]])
unzipConsSkew =
   let aux [] [] = ([],[])  -- one empty list at the end will be removed
       aux xs ys = mapSnd (xs:) $ prep ys
       prep =
          forcePair .
          switchL ([],[])
             (\y ys ->
                let my = viewL y
                in  mapFst (fmap fst my :) $
                    aux (maybe [] snd my) ys)
   in  prep



shear' :: [[a]] -> [[a]]
shear' xs@(_:_) =
   let (y:ys,zs) = unzip (map (splitAt 1) xs)
       zipConc (a:as) (b:bs) = (a++b) : zipConc as bs
       zipConc [] bs = bs
       zipConc as [] = as
   in  y : zipConc ys (shear' (Rev.dropWhile null zs))
              {- Dropping trailing empty lists is necessary,
                 otherwise finite lists are filled with empty lists. -}
shear' [] = []

{- |
Transform

> [[00,01,02,...],          [[00],
>  [10,11,12,...],   -->     [01,10],
>  [20,21,22,...],           [02,11,20],
>  ...]                      ...]

It's like 'shear' but the order of elements in the sub list is reversed.
Its implementation seems to be more efficient than that of 'shear'.
If the order does not matter, better choose 'shearTranspose'.

prop> \xs -> shearTranspose xs  ==  map reverse (shear (xs::[String]))
-}
shearTranspose :: [[a]] -> [[a]]
shearTranspose =
   foldr zipConsSkew []

zipConsSkew :: [a] -> [[a]] -> [[a]]
zipConsSkew xt yss =
   uncurry (:) $
   case xt of
      x:xs -> ([x], zipCons xs yss)
      [] -> ([], yss)

{- |
zipCons is like @zipWith (:)@ but it keeps lists which are too long
This version works also for @zipCons something undefined@.
-}
zipCons :: [a] -> [[a]] -> [[a]]
zipCons (x:xs) yt =
   let (y,ys) = switchL ([],[]) (,) yt
   in  (x:y) : zipCons xs ys
zipCons [] ys = ys

-- | zipCons' is like @zipWith (:)@ but it keeps lists which are too long
zipCons' :: [a] -> [[a]] -> [[a]]
zipCons' (x:xs) (y:ys) = (x:y) : zipCons' xs ys
zipCons' [] ys = ys
zipCons' xs [] = map (:[]) xs


{- |
Operate on each combination of elements of the first and the second list.
In contrast to the list instance of 'Monad.liftM2'
it holds the results in a list of lists.

prop> \xs ys -> let f x y = (x::Char,y::Int) in concat (outerProduct f xs ys)  ==  liftM2 f xs ys
-}
outerProduct :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outerProduct f xs ys = map (flip map ys . f) xs



-- * Miscellaneous

{- |
Take while first predicate holds,
then continue taking while second predicate holds,
and so on.
-}
takeWhileMulti :: [a -> Bool] -> [a] -> [a]
takeWhileMulti [] _  = []
takeWhileMulti _  [] = []
takeWhileMulti aps@(p:ps) axs@(x:xs) =
   if p x
      then x : takeWhileMulti aps xs
      else takeWhileMulti ps axs

{- |
prop> \ys xs -> let ps = map (<=) ys in takeWhileMulti ps xs == takeWhileMulti' ps (xs::String)
-}
takeWhileMulti' :: [a -> Bool] -> [a] -> [a]
takeWhileMulti' ps xs =
   concatMap fst (tail
      (scanl (flip span . snd) (undefined,xs) ps))

{-
Debug.QuickCheck.quickCheck (propTakeWhileMulti [(<0), (>0), odd, even, ((0::Int)==)])
-}

{- |
This is a combination of 'foldl'' and 'foldr'
in the sense of 'propFoldl'r'.
It is however more efficient
because it avoids storing the whole input list as a result of sharing.
-}
foldl'r, foldl'rStrict, foldl'rNaive ::
   (b -> a -> b) -> b -> (c -> d -> d) -> d -> [(a,c)] -> (b,d)
foldl'r f b0 g d0 =
--   (\(k,d1) -> (k b0, d1)) .
   mapFst ($ b0) .
   foldr (\(a,c) ~(k,d) -> (\b -> k $! f b a, g c d)) (id,d0)

foldl'rStrict f b0 g d0 =
   mapFst ($ b0) .
   foldr (\(a,c) ~(k,d) -> ((,) $! (\b -> k $! f b a)) $! g c d) (id,d0)

foldl'rNaive f b g d xs =
   mapPair (foldl' f b, foldr g d) $ unzip xs

propFoldl'r :: (Eq b, Eq d) =>
   (b -> a -> b) -> b -> (c -> d -> d) -> d -> [(a,c)] -> Bool
propFoldl'r f b g d xs =
   foldl'r f b g d xs == foldl'rNaive f b g d xs

{-
The results in GHCi surprise:

*List.HT> mapSnd last $ foldl'rNaive (+) (0::Integer) (:) "" $ replicate 1000000 (1,'a')
(1000000,'a')
(0.44 secs, 141032856 bytes)

*List.HT> mapSnd last $ foldl'r (+) (0::Integer) (:) "" $ replicate 1000000 (1,'a')
(1000000,'a')
(2.64 secs, 237424948 bytes)
-}

{-
Debug.QuickCheck.quickCheck (\b d -> propFoldl'r (+) (b::Int) (++) (d::[Int]))
-}


{- |
>>> lengthAtLeast 0 ""
True
>>> lengthAtLeast 3 "ab"
False
>>> lengthAtLeast 3 "abc"
True
>>> lengthAtLeast 3 $ repeat 'a'
True
>>> lengthAtLeast 3 $ "abc" ++ undefined
True

prop> \n xs -> lengthAtLeast n (xs::String)  ==  (length xs >= n)
-}
lengthAtLeast :: Int -> [a] -> Bool
lengthAtLeast n =
   if n<=0
     then const True
     else not . null . drop (n-1)

{- |
>>> lengthAtMost 0 ""
True
>>> lengthAtMost 3 "ab"
True
>>> lengthAtMost 3 "abc"
True
>>> lengthAtMost 3 "abcd"
False
>>> lengthAtMost 3 $ repeat 'a'
False
>>> lengthAtMost 3 $ "abcd" ++ undefined
False

prop> \n xs -> lengthAtMost n (xs::String)  ==  (length xs <= n)
-}
lengthAtMost :: Int -> [a] -> Bool
lengthAtMost n =
   if n<0
     then const False
     else null . drop n

{- |
prop> \n xs -> lengthAtMost0 n (xs::String)  ==  (length xs <= n)
-}
lengthAtMost0 :: Int -> [a] -> Bool
lengthAtMost0 n = (n>=) . length . take (n+1)

{-
Iterate until elements start to cycle.
This implementation is inspired by Elements of Programming
but I am still not satisfied
where the iteration actually stops.
-}
iterateUntilCycle :: (Eq a) => (a -> a) -> a -> [a]
iterateUntilCycle f a =
   let as = iterate f a
   in  (a:) $ map fst $
       takeWhile (uncurry (/=)) $
       zip (tail as) (concatMap (\ai->[ai,ai]) as)

{-
iterateUntilCycleQ :: (Eq a) => (a -> a) -> a -> [a]
iterateUntilCycleQ f a =
   let as = tail $ iterate f a
   in  (a:) $ map fst $
       takeWhile (uncurry (/=)) $
       zip as (downsample2 (tail as))
-}

iterateUntilCycleP :: (Eq a) => (a -> a) -> a -> [a]
iterateUntilCycleP f a =
   let as = iterate f a
   in  map fst $
       takeWhile (\(a1,(a20,a21)) -> a1/=a20 && a1/=a21) $
       zip as (pairs (tail as))

pairs :: [t] -> [(t, t)]
pairs [] = []
pairs (_:[]) = error "pairs: odd number of elements"
pairs (x0:x1:xs) = (x0,x1) : pairs xs


{- | rotate left -}
rotate, rotate', rotate'' :: Int -> [a] -> [a]
rotate n x =
   Match.take x (drop (mod n (length x)) (cycle x))

{- | more efficient implementation of rotate'

prop> \n (NonEmpty xs) -> rotate n xs == rotate' n (xs::String)
-}
rotate' n x =
   uncurry (flip (++))
           (splitAt (mod n (length x)) x)

{- |
prop> \(NonNegative n) xs -> rotate n xs == rotate'' n (xs::String)
-}
rotate'' n x =
   Match.take x (drop n (cycle x))

{- |
Given two lists that are ordered
(i.e. @p x y@ holds for subsequent @x@ and @y@)
'mergeBy' them into a list that is ordered, again.

>>> mergeBy (<=) "agh" "begz"
"abegghz"
-}
mergeBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
mergeBy = Key.mergeBy


{- |
>>> allEqual "aab"
False
>>> allEqual "aaa"
True
>>> allEqual "aa"
True
>>> allEqual "a"
True
>>> allEqual ""
True
-}
allEqual :: Eq a => [a] -> Bool
allEqual = and . mapAdjacent (==)

{- |
>>> isAscending "abc"
True
>>> isAscending "abb"
True
>>> isAscending "aba"
False
>>> isAscending "cba"
False
>>> isAscending "a"
True
>>> isAscending ""
True
-}
isAscending :: (Ord a) => [a] -> Bool
isAscending = and . isAscendingLazy

isAscendingLazy :: (Ord a) => [a] -> [Bool]
isAscendingLazy = mapAdjacent (<=)

{- |
This function combines every pair of neighbour elements
in a list with a certain function.

>>> mapAdjacent (<=) ""
[]
>>> mapAdjacent (<=) "a"
[]
>>> mapAdjacent (<=) "aba"
[True,False]
>>> mapAdjacent (,) "abc"
[('a','b'),('b','c')]

prop> \x xs -> mapAdjacent subtract (scanl (+) x xs) == (xs::[Integer])
-}
mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs (tail xs)

{- |
<http://mail.haskell.org/libraries/2016-April/026912.html>

prop> \xs -> mapAdjacent (,) xs == mapAdjacentPointfree (,) (xs::String)
-}
mapAdjacentPointfree :: (a -> a -> b) -> [a] -> [b]
mapAdjacentPointfree f = zipWith f <*> tail


{- |
>>> let f x y z = [x,y]++show(z::Int) in mapAdjacent1 f 'a' [('b',1), ('c',2), ('d',3)]
["ab1","bc2","cd3"]
-}
mapAdjacent1 :: (a -> a -> b -> c) -> a -> [(a,b)] -> [c]
mapAdjacent1 f a xs =
   zipWith (\a0 (a1,b) -> f a0 a1 b) (a : map fst xs) xs


{- |
>>> equalWith (<=) "ab" "bb"
True
>>> equalWith (<=) "aa" "bbb"
False
>>> equalWith (==) "aa" "aaa"
False

prop> \as bs -> let f a b = abs (a-b) <= (10::Int) in equalWith f as bs ==  equalWithRec f as bs
prop> \as bs -> let f a b = abs (a-b) <= (10::Int) in equalWith f as bs ==  equalWithLiftM f as bs
-}
equalWith, equalWithLiftM, equalWithRec ::
   (a -> b -> Bool) -> [a] -> [b] -> Bool
equalWith f as bs =
   and $
   zipWith
      (\ma mb ->
         case (ma,mb) of
            (Just a, Just b) -> f a b
            (Nothing, Nothing) -> True
            _ -> False)
      (map Just as ++ [Nothing])
      (map Just bs ++ [Nothing])

equalWithLiftM f as bs =
   all (Just True ==) $
   zipWith
      (\ma mb ->
         case (ma,mb) of
            (Nothing, Nothing) -> Just True
            _ -> liftM2 f ma mb)
      (map Just as ++ [Nothing])
      (map Just bs ++ [Nothing])

equalWithRec f =
   let go (a:as) (b:bs) = f a b && go as bs
       go [] [] = True
       go _ _ = False
   in go


{- |
Enumerate without Enum context.
For Enum equivalent to enumFrom.

>>> range 0 :: [Integer]
[]
>>> range 1 :: [Integer]
[0]
>>> range 8 :: [Integer]
[0,1,2,3,4,5,6,7]

prop> \(NonNegative n) -> length (range n :: [Integer]) == n
-}
range :: Num a => Int -> [a]
range n = take n (iterate (+1) 0)


{-# INLINE padLeft #-}
padLeft :: a -> Int -> [a] -> [a]
padLeft  c n xs = replicate (n - length xs) c ++ xs


{-# INLINE padRight #-}
padRight, padRight1 :: a -> Int -> [a] -> [a]
padRight  c n xs = take n $ xs ++ repeat c
padRight1 c n xs = xs ++ replicate (n - length xs) c

{- |
For an associative operation @op@ this computes
   @iterateAssociative op a = iterate (op a) a@
but it is even faster than @map (powerAssociative op a a) [0..]@
since it shares temporary results.

The idea is:
From the list @map (powerAssociative op a a) [0,(2*n)..]@
we compute the list @map (powerAssociative op a a) [0,n..]@,
and iterate that until @n==1@.

prop> \x -> equating (take 1000) (List.iterate (x+) x) (iterateAssociative (+) (x::Integer))
-}
iterateAssociative :: (a -> a -> a) -> a -> [a]
iterateAssociative op a =
   foldr (\pow xs -> pow : concatMap (\x -> [x, op x pow]) xs)
         undefined (iterate (\x -> op x x) a)

{- |
This is equal to 'iterateAssociative'.
The idea is the following:
The list we search is the fixpoint of the function:
"Square all elements of the list,
then spread it and fill the holes with successive numbers
of their left neighbour."
This also preserves log n applications per value.
However it has a space leak,
because for the value with index @n@
all elements starting at @div n 2@ must be kept.

prop> \x -> equating (take 1000) (List.iterate (x+) x) (iterateLeaky (+) (x::Integer))
-}
iterateLeaky :: (a -> a -> a) -> a -> [a]
iterateLeaky op x =
   let merge (a:as) b = a : merge b as
       merge _ _ = error "iterateLeaky: an empty list cannot occur"
       sqrs = map (\y -> op y y) z
       z = x : merge sqrs (map (op x) sqrs)
   in  z
