module Data.List
  ( module GHC.List,
    intersperse,
    intercalate,
    transpose,
    subsequences,
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
    stripPrefix,
    group,
    groupBy,
    inits,
    tails,
    isPrefixOf,
    isSuffixOf,
    isInfixOf,
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
    zipWith4,
    nub,
    nubBy,
    delete,
    deleteBy,
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

import GHC.List
import Prelude
  ( Bool (..),
    Eq (..),
    Int,
    Integral (..),
    Maybe (..),
    Num (..),
    Ord (..),
    Ordering (..),
    errorWithoutStackTrace,
    flip,
    fromIntegral,
    lines,
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

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find matches (value : values) =
  if matches value
    then Just value
    else find matches values

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
sort = sortBy compare

sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn project values =
  map snd (sortBy compareKeys (map (\value -> (project value, value)) values))

compareKeys :: (Ord key) => (key, value) -> (key, value) -> Ordering
compareKeys (leftKey, _) (rightKey, _) = compare leftKey rightKey

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [value] = [value]
sortBy order values =
  case splitAt (length values `div` 2) values of
    (left, right) -> mergeBy order (sortBy order left) (sortBy order right)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] right = right
mergeBy _ left [] = left
mergeBy order (left : lefts) (right : rights) =
  case order left right of
    GT -> right : mergeBy order (left : lefts) rights
    _ -> left : mergeBy order lefts (right : rights)

insert :: (Ord a) => a -> [a] -> [a]
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ value [] = [value]
insertBy order value (next : rest) =
  case order value next of
    GT -> next : insertBy order value rest
    _ -> value : next : rest

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [] = errorWithoutStackTrace "List.maximumBy: empty list"
maximumBy order (value : values) = foldl (\best next -> if order best next == LT then next else best) value values

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ [] = errorWithoutStackTrace "List.minimumBy: empty list"
minimumBy order (value : values) = foldl (\best next -> if order best next == GT then next else best) value values

genericLength :: (Num i) => [a] -> i
genericLength [] = 0
genericLength (_ : values) = 1 + genericLength values

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
