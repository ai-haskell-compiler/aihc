-- foldl' and length keep their arguments: the arity pass reads the arity
-- from the body, and an eta-reduced alias is not inlined.
{-# HLINT ignore foldl' "Eta reduce" #-}
{-# HLINT ignore length "Eta reduce" #-}
module GHC.List
  ( build,
    map,
    (++),
    filter,
    concat,
    concatMap,
    head,
    last,
    tail,
    init,
    uncons,
    unsnoc,
    null,
    length,
    (!!),
    foldl,
    foldl',
    foldl1,
    foldl1',
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
    and,
    or,
    any,
    all,
    elem,
    notElem,
    lookup,
    sum,
    product,
    maximum,
    minimum,
    zip,
    zip3,
    zipWith,
    zipWith3,
    unzip,
    unzip3,
  )
where

import GHC.Base (build)
import GHC.Internal.Foldable (listFoldl', listLength)
import Prelude hiding (all, and, any, concat, concatMap, elem, foldl', length, notElem, or)

-- The list-specialised versions GHC's "GHC.List" exports; "Prelude" and
-- "Data.List" export the 'Foldable' ones.
concat :: [[a]] -> [a]
concat = foldr (++) []

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap function = foldr (\value rest -> function value ++ rest) []

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

any :: (a -> Bool) -> [a] -> Bool
any predicate = foldr (\value rest -> predicate value || rest) False

all :: (a -> Bool) -> [a] -> Bool
all predicate = foldr (\value rest -> predicate value && rest) True

elem :: (Eq a) => a -> [a] -> Bool
elem target = foldr (\value rest -> value == target || rest) False

infix 4 `elem`

notElem :: (Eq a) => a -> [a] -> Bool
notElem target values = not (target `elem` values)

infix 4 `notElem`

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (value : values) = Just (value, values)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc values = Just (init values, last values)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' combine initial values = listFoldl' combine initial values

length :: [a] -> Int
length values = listLength values

foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = errorWithoutStackTrace "Prelude.foldl1': empty list"
foldl1' combine (value : values) = foldl' combine value values

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' combine initial values =
  initial
    `seq` ( initial
              : ( case values of
                    [] -> []
                    (value : rest) -> scanl' combine (combine initial value) rest
                )
          )

iterate' :: (a -> a) -> a -> [a]
iterate' next value =
  let following = next value
   in value : (following `seq` iterate' next following)
