module Data.List
  ( -- @GHC.List@ is re-exported name by name rather than as a module, so
    -- that its list-only @foldl'@ and @length@ can be left out in favour
    -- of the 'Foldable' ones, as in @base@.
    map,
    (++),
    head,
    last,
    tail,
    init,
    uncons,
    unsnoc,
    null,
    length,
    (!!),
    (!?),
    foldl,
    foldl',
    foldl1,
    foldr,
    foldr1,
    scanl,
    scanl1,
    scanl',
    scanr,
    scanr1,
    iterate,
    iterate',
    repeat,
    replicate,
    cycle,
    take,
    drop,
    splitAt,
    takeWhile,
    dropWhile,
    span,
    break,
    reverse,
    zip,
    zip3,
    zipWith,
    zipWith3,
    unzip,
    unzip3,
    intersperse,
    intercalate,
    transpose,
    subsequences,
    permutations,
    foldl1',
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    sum,
    product,
    maximum,
    minimum,
    unfoldr,
    mapAccumL,
    mapAccumR,
    stripPrefix,
    group,
    groupBy,
    inits,
    tails,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
    isSubsequenceOf,
    elem,
    notElem,
    lookup,
    find,
    filter,
    partition,
    elemIndex,
    elemIndices,
    findIndex,
    findIndices,
    zip4,
    zip5,
    zip6,
    zip7,
    zipWith4,
    zipWith5,
    zipWith6,
    zipWith7,
    unzip4,
    unzip5,
    unzip6,
    unzip7,
    nub,
    nubBy,
    delete,
    deleteBy,
    deleteFirstsBy,
    (\\),
    union,
    unionBy,
    intersect,
    intersectBy,
    sort,
    sortBy,
    sortOn,
    insert,
    insertBy,
    maximumBy,
    minimumBy,
    genericLength,
    genericIndex,
    genericTake,
    genericDrop,
    genericSplitAt,
    genericReplicate,
    dropWhileEnd,
    singleton,
    lines,
    unlines,
    words,
    unwords,
  )
where

import Data.Foldable (find, foldl', length, maximumBy, minimumBy)
import Data.Traversable (mapAccumL, mapAccumR)
-- @foldl'@ and @length@ come from 'Data.Foldable' here, as they do in
-- @base@: the rest of the fold vocabulary this module re-exports is already
-- the 'Foldable' method, and only 'GHC.List' keeps a list-only @foldl'@ and
-- @length@.
import GHC.List hiding (all, and, any, concat, concatMap, elem, foldl', length, notElem, or)
import Prelude
  ( Bool (..),
    Eq (..),
    Int,
    Integral (..),
    Maybe (..),
    Num (..),
    Ord (..),
    Ordering (..),
    all,
    and,
    any,
    concat,
    concatMap,
    elem,
    errorWithoutStackTrace,
    flip,
    fromIntegral,
    id,
    lines,
    not,
    notElem,
    or,
    otherwise,
    seq,
    snd,
    unlines,
    unwords,
    words,
    (&&),
  )

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse separator (value : values) = value : prependSeparators separator values

prependSeparators :: a -> [a] -> [a]
prependSeparators _ [] = []
prependSeparators separator (value : values) = separator : value : prependSeparators separator values

intercalate :: [a] -> [[a]] -> [a]
intercalate separator lists = concat (intersperse separator lists)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : rows) = transpose rows
transpose ((value : values) : rows) =
  (value : concatMap rowHead rows) : transpose (values : map rowTail rows)

rowHead :: [a] -> [a]
rowHead [] = []
rowHead (value : _) = [value]

rowTail :: [a] -> [a]
rowTail [] = []
rowTail (_ : rest) = rest

subsequences :: [a] -> [[a]]
subsequences values = [] : nonEmptySubsequences values

nonEmptySubsequences :: [a] -> [[a]]
nonEmptySubsequences [] = []
nonEmptySubsequences (value : values) =
  [value] : foldr (\subsequence rest -> subsequence : (value : subsequence) : rest) [] (nonEmptySubsequences values)

permutations :: [a] -> [[a]]
permutations values = values : permutationsAfter values []

-- Every permutation other than the input itself: pick each element in turn and
-- interleave it into the permutations of the elements picked before it.
permutationsAfter :: [a] -> [a] -> [[a]]
permutationsAfter [] _ = []
permutationsAfter (value : values) taken =
  foldr
    (interleaveEverywhere value values)
    (permutationsAfter values (value : taken))
    (permutations taken)

interleaveEverywhere :: a -> [a] -> [a] -> [[a]] -> [[a]]
interleaveEverywhere value values permutation rest =
  snd (interleaveInto value values id permutation rest)

interleaveInto :: a -> [a] -> ([a] -> [a]) -> [a] -> [[a]] -> ([a], [[a]])
interleaveInto _ values _ [] rest = (values, rest)
interleaveInto value values prefix (first : others) rest =
  (first : rebuilt, prefix (value : first : rebuilt) : expanded)
  where
    (rebuilt, expanded) = interleaveInto value values (\prefixed -> prefix (first : prefixed)) others rest

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr step seed =
  case step seed of
    Nothing -> []
    Just (value, next) -> value : unfoldr step next

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] value = Just value
stripPrefix _ [] = Nothing
stripPrefix (left : lefts) (right : rights) =
  if left == right
    then stripPrefix lefts rights
    else Nothing

group :: (Eq a) => [a] -> [[a]]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy same (value : values) =
  case span (same value) values of
    (matching, rest) -> (value : matching) : groupBy same rest

inits :: [a] -> [[a]]
inits values = [] : initsFrom values

initsFrom :: [a] -> [[a]]
initsFrom [] = []
initsFrom (value : values) = map (value :) (inits values)

tails :: [a] -> [[a]]
tails [] = [[]]
tails values@(_ : rest) = values : tails rest

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (left : lefts) (right : rights) = left == right && isPrefixOf lefts rights

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf suffix value = reverse suffix `isPrefixOf` reverse value

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

-- | 'True' if the elements of the first list occur in the second list in
-- the same order. The elements need not be adjacent.
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf needle@(value : values) (candidate : candidates)
  | value == candidate = isSubsequenceOf values candidates
  | otherwise = isSubsequenceOf needle candidates

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition select = foldr (selectPartition select) ([], [])

selectPartition :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
selectPartition select value (selected, rejected) =
  if select value
    then (value : selected, rejected)
    else (selected, value : rejected)

elemIndex :: (Eq a) => a -> [a] -> Maybe Int
elemIndex value = findIndex (== value)

elemIndices :: (Eq a) => a -> [a] -> [Int]
elemIndices value = findIndices (== value)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex matches values =
  case findIndices matches values of
    [] -> Nothing
    (index : _) -> Just index

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices matches = findIndicesFrom matches 0

findIndicesFrom :: (a -> Bool) -> Int -> [a] -> [Int]
findIndicesFrom _ _ [] = []
findIndicesFrom matches index (value : values) =
  if matches value
    then index : findIndicesFrom matches (index + 1) values
    else findIndicesFrom matches (index + 1) values

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = zipWith4 (\valueOne valueTwo valueThree valueFour -> (valueOne, valueTwo, valueThree, valueFour))

zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 combine (valueOne : onesRest) (valueTwo : twosRest) (valueThree : threesRest) (valueFour : foursRest) =
  combine valueOne valueTwo valueThree valueFour : zipWith4 combine onesRest twosRest threesRest foursRest
zipWith4 _ _ _ _ _ = []

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = zipWith5 (,,,,)

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = zipWith6 (,,,,,)

zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zip7 = zipWith7 (,,,,,,)

zipWith5 :: (a -> b -> c -> d -> e -> r) -> [a] -> [b] -> [c] -> [d] -> [e] -> [r]
zipWith5 combine (a : as) (b : bs) (c : cs) (d : ds) (e : es) =
  combine a b c d e : zipWith5 combine as bs cs ds es
zipWith5 _ _ _ _ _ _ = []

zipWith6 :: (a -> b -> c -> d -> e -> f -> r) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [r]
zipWith6 combine (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) =
  combine a b c d e f : zipWith6 combine as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _ = []

zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> r) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [r]
zipWith7 combine (a : as) (b : bs) (c : cs) (d : ds) (e : es) (f : fs) (g : gs) =
  combine a b c d e f g : zipWith7 combine as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

-- The unzip functions use lazy patterns, as in @base@: each result list
-- is available before the input list ends.

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 =
  foldr
    (\(a, b, c, d) ~(as, bs, cs, ds) -> (a : as, b : bs, c : cs, d : ds))
    ([], [], [], [])

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 =
  foldr
    (\(a, b, c, d, e) ~(as, bs, cs, ds, es) -> (a : as, b : bs, c : cs, d : ds, e : es))
    ([], [], [], [], [])

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 =
  foldr
    (\(a, b, c, d, e, f) ~(as, bs, cs, ds, es, fs) -> (a : as, b : bs, c : cs, d : ds, e : es, f : fs))
    ([], [], [], [], [], [])

unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 =
  foldr
    (\(a, b, c, d, e, f, g) ~(as, bs, cs, ds, es, fs, gs) -> (a : as, b : bs, c : cs, d : ds, e : es, f : fs, g : gs))
    ([], [], [], [], [], [], [])

nub :: (Eq a) => [a] -> [a]
nub = nubBy (==)

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy same = nubByFrom same []

nubByFrom :: (a -> a -> Bool) -> [a] -> [a] -> [a]
nubByFrom _ _ [] = []
nubByFrom same seen (value : values) =
  if any (same value) seen
    then nubByFrom same seen values
    else value : nubByFrom same (value : seen) values

delete :: (Eq a) => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy same target (value : values) =
  if same target value
    then values
    else value : deleteBy same target values

-- | Remove the first match of each element of the second list from the first list.
deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy same = foldl (flip (deleteBy same))

(\\) :: (Eq a) => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

infix 5 \\

union :: (Eq a) => [a] -> [a] -> [a]
union = unionBy (==)

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy same left right =
  left ++ foldl (flip (deleteBy same)) (nubBy same right) left

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect = intersectBy (==)

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _ [] _ = []
intersectBy _ _ [] = []
intersectBy same left right = filter (\value -> any (same value) right) left

sort :: (Ord a) => [a] -> [a]
sort = sortGreater (>)

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn project values =
  map snd (sortBy compareKeys (map (\value -> (project value, value)) values))

compareKeys :: (Ord key) => (key, value) -> (key, value) -> Ordering
compareKeys (leftKey, _) (rightKey, _) = compare leftKey rightKey

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy order = sortGreater (\left right -> order left right == GT)

-- | A stable merge sort, as in GHC base (GHC issues 2143 and 24280). The
-- argument is true when its first operand must come after the second.
-- Equal elements stay in ascending runs, and the sort reverses only strictly
-- descending runs. Each merge keeps the order of equal elements.
--
-- The sort makes each run and each merge of a pass before it continues.
-- It merges four lists at a time. Thus there are fewer passes, and fewer
-- unevaluated merges are live at the same time.
sortGreater :: (a -> a -> Bool) -> [a] -> [a]
sortGreater greater values =
  case values of
    [] -> []
    [a] -> [a]
    [a, b] -> merge [a] [b]
    [a, b, c] -> merge3 [a] [b] [c]
    [a, b, c, d] -> merge4 [a] [b] [c] [d]
    _ -> mergeAll (runs values)
  where
    runs (a : b : rest)
      | greater a b = descending b [a] rest
      | otherwise = ascending b (a :) rest
    runs rest = [rest]

    descending a accumulated (b : rest)
      | greater a b = descending b (a : accumulated) rest
    descending a accumulated rest = (a : accumulated) : runs rest

    ascending a accumulated (b : rest)
      | not (greater a b) = ascending b (\remaining -> accumulated (a : remaining)) rest
    ascending a accumulated rest =
      let run = accumulated [a]
       in run `seq` (run : runs rest)

    mergeAll [run] = run
    mergeAll remaining = mergeAll (reduceOnce remaining)

    reduceOnce [] = []
    reduceOnce [a] = [a]
    reduceOnce [a, b] = [merge a b]
    reduceOnce [a, b, c] = [merge3 a b c]
    reduceOnce [a, b, c, d, e] = [merge a b, merge3 c d e]
    reduceOnce [a, b, c, d, e, f] = [merge3 a b c, merge3 d e f]
    reduceOnce (a : b : c : d : rest) =
      let merged = merge4 a b c d
       in merged `seq` (merged : reduceOnce rest)

    merge as@(a : as') bs@(b : bs')
      | greater a b = b : merge as bs'
      | otherwise = a : merge as' bs
    merge [] bs = bs
    merge as [] = as

    -- merge3 as bs cs is a fused form of merge (merge as bs) cs.
    merge3 as@(a : as') bs@(b : bs') cs
      | greater a b = merge3X b as bs' cs
      | otherwise = merge3X a as' bs cs
    merge3 [] bs cs = merge bs cs
    merge3 as [] cs = merge as cs

    -- x is the head of merge as bs, which is not in as or bs.
    merge3X x as bs cs@(c : cs')
      | greater x c = c : merge3X x as bs cs'
      | otherwise = x : merge3 as bs cs
    merge3X x as bs [] = x : merge as bs

    -- y is the head of cs, which is not in cs.
    merge3Y as@(a : as') y bs cs
      | greater a y = y : merge3 as bs cs
      | otherwise = a : merge3Y as' y bs cs
    merge3Y [] y bs cs = y : merge bs cs

    -- merge4 as bs cs ds is a fused form of merge (merge as bs) (merge cs ds).
    merge4 as@(a : as') bs@(b : bs') cs ds
      | greater a b = merge4X b as bs' cs ds
      | otherwise = merge4X a as' bs cs ds
    merge4 [] bs cs ds = merge3 bs cs ds
    merge4 as [] cs ds = merge3 as cs ds

    merge4X x as bs cs@(c : cs') ds@(d : ds')
      | greater c d = merge4XY x as bs d cs ds'
      | otherwise = merge4XY x as bs c cs' ds
    merge4X x as bs [] ds = merge3X x as bs ds
    merge4X x as bs cs [] = merge3X x as bs cs

    merge4Y as@(a : as') bs@(b : bs') y cs ds
      | greater a b = merge4XY b as bs' y cs ds
      | otherwise = merge4XY a as' bs y cs ds
    merge4Y as [] y cs ds = merge3Y as y cs ds
    merge4Y [] bs y cs ds = merge3Y bs y cs ds

    -- x is the head of merge as bs, and y is the head of merge cs ds.
    merge4XY x as bs y cs ds
      | greater x y = y : merge4X x as bs cs ds
      | otherwise = x : merge4Y as bs y cs ds

insert :: (Ord a) => a -> [a] -> [a]
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ value [] = [value]
insertBy order value (next : rest) =
  case order value next of
    GT -> next : insertBy order value rest
    _ -> value : next : rest

genericLength :: (Num i) => [a] -> i
genericLength [] = 0
genericLength (_ : values) = 1 + genericLength values

-- | The element at an index of any integral type. GHC gives the same
-- error messages.
genericIndex :: (Integral i) => [a] -> i -> a
genericIndex (value : rest) count
  | count > 0 = genericIndex rest (count - 1)
  | count == 0 = value
  | otherwise = errorWithoutStackTrace "List.genericIndex: negative argument."
genericIndex _ _ = errorWithoutStackTrace "List.genericIndex: index too large."

genericTake :: (Integral i) => i -> [a] -> [a]
genericTake count values =
  if count <= 0
    then []
    else case values of
      [] -> []
      (value : rest) -> value : genericTake (count - 1) rest

genericDrop :: (Integral i) => i -> [a] -> [a]
genericDrop count values =
  if count <= 0
    then values
    else case values of
      [] -> []
      (_ : rest) -> genericDrop (count - 1) rest

genericSplitAt :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt count values = (genericTake count values, genericDrop count values)

genericReplicate :: (Integral i) => i -> a -> [a]
genericReplicate count = replicate (fromIntegral count)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd drops = foldr (\value rest -> if drops value && null rest then [] else value : rest) []

singleton :: a -> [a]
singleton value = [value]
