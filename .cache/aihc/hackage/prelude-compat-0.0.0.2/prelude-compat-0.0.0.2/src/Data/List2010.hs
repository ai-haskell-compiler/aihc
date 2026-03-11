module Data.List2010 where

import qualified Data.List as List
import Prelude hiding ((++))


(!!) :: [a] -> Int -> a
(!!) = (List.!!)

infixr 5 ++

(++) :: [a] -> [a] -> [a]
(++) = (List.++)

infix 5 \\

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = (List.\\)

all :: (a -> Bool) -> [a] -> Bool
all = List.all

and :: [Bool] -> Bool
and = List.and

any :: (a -> Bool) -> [a] -> Bool
any = List.any

break :: (a -> Bool) -> [a] -> ([a], [a])
break = List.break

concat :: [[a]] -> [a]
concat = List.concat

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap = List.concatMap

cycle :: [a] -> [a]
cycle = List.cycle

delete :: Eq a => a -> [a] -> [a]
delete = List.delete

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy = List.deleteBy

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy = List.deleteFirstsBy

drop :: Int -> [a] -> [a]
drop = List.drop

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = List.dropWhile

elem :: Eq a => a -> [a] -> Bool
elem = List.elem

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex = List.elemIndex

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices = List.elemIndices

filter :: (a -> Bool) -> [a] -> [a]
filter = List.filter

find :: (a -> Bool) -> [a] -> Maybe a
find = List.find

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex = List.findIndex

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices = List.findIndices

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl = List.foldl

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' = List.foldl'

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 = List.foldl1

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' = List.foldl1'

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = List.foldr

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 = List.foldr1

genericDrop :: Integral i => i -> [a] -> [a]
genericDrop = List.genericDrop

genericIndex :: Integral a => [b] -> a -> b
genericIndex = List.genericIndex

genericLength :: Num i => [b] -> i
genericLength = List.genericLength

genericReplicate :: Integral i => i -> a -> [a]
genericReplicate = List.genericReplicate

genericSplitAt :: Integral i => i -> [b] -> ([b], [b])
genericSplitAt = List.genericSplitAt

genericTake :: Integral i => i -> [a] -> [a]
genericTake = List.genericTake

group :: Eq a => [a] -> [[a]]
group = List.group

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy = List.groupBy

head :: [a] -> a
head = List.head

init :: [a] -> [a]
init = List.init

inits :: [a] -> [[a]]
inits = List.inits

insert :: Ord a => a -> [a] -> [a]
insert = List.insert

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy = List.insertBy

intercalate :: [a] -> [[a]] -> [a]
intercalate = List.intercalate

intersect :: Eq a => [a] -> [a] -> [a]
intersect = List.intersect

intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy = List.intersectBy

intersperse :: a -> [a] -> [a]
intersperse = List.intersperse

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf = List.isInfixOf

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf = List.isPrefixOf

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf = List.isSuffixOf

iterate :: (a -> a) -> a -> [a]
iterate = List.iterate

last :: [a] -> a
last = List.last

length :: [a] -> Int
length = List.length

lines :: String -> [String]
lines = List.lines

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup = List.lookup

map :: (a -> b) -> [a] -> [b]
map = List.map

mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL = List.mapAccumL

mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR = List.mapAccumR

maximum :: Ord a => [a] -> a
maximum = List.maximum

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy = List.maximumBy

minimum :: Ord a => [a] -> a
minimum = List.minimum

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy = List.minimumBy

notElem :: Eq a => a -> [a] -> Bool
notElem = List.notElem

nub :: Eq a => [a] -> [a]
nub = List.nub

nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy = List.nubBy

null :: [a] -> Bool
null = List.null

or :: [Bool] -> Bool
or = List.or

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition = List.partition

permutations :: [a] -> [[a]]
permutations = List.permutations

product :: Num a => [a] -> a
product = List.product

repeat :: a -> [a]
repeat = List.repeat

replicate :: Int -> a -> [a]
replicate = List.replicate

reverse :: [a] -> [a]
reverse = List.reverse

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl = List.scanl

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 = List.scanl1

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr = List.scanr

scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 = List.scanr1

sort :: Ord a => [a] -> [a]
sort = List.sort

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy = List.sortBy

span :: (a -> Bool) -> [a] -> ([a], [a])
span = List.span

splitAt :: Int -> [a] -> ([a], [a])
splitAt = List.splitAt

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix = List.stripPrefix

subsequences :: [a] -> [[a]]
subsequences = List.subsequences

sum :: Num a => [a] -> a
sum = List.sum

tail :: [a] -> [a]
tail = List.tail

tails :: [a] -> [[a]]
tails = List.tails

take :: Int -> [a] -> [a]
take = List.take

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = List.takeWhile

transpose :: [[a]] -> [[a]]
transpose = List.transpose

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr = List.unfoldr

union :: Eq a => [a] -> [a] -> [a]
union = List.union

unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy = List.unionBy

unlines :: [String] -> String
unlines = List.unlines

unwords :: [String] -> String
unwords = List.unwords

unzip :: [(a, b)] -> ([a], [b])
unzip = List.unzip

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = List.unzip3

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = List.unzip4

unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 = List.unzip5

unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 = List.unzip6

unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 = List.unzip7

words :: String -> [String]
words = List.words

zip :: [a] -> [b] -> [(a, b)]
zip = List.zip

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 = List.zip3

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = List.zip4

zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = List.zip5

zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = List.zip6

zip7 ::
  [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] ->
  [(a, b, c, d, e, f, g)]
zip7 = List.zip7

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = List.zipWith

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 = List.zipWith3

zipWith4 ::
  (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 = List.zipWith4

zipWith5 ::
  (a -> b -> c -> d -> e -> f) ->
  [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 = List.zipWith5

zipWith6 ::
  (a -> b -> c -> d -> e -> f -> g) ->
  [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 = List.zipWith6

zipWith7 ::
  (a -> b -> c -> d -> e -> f -> g -> h) ->
  [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 = List.zipWith7
