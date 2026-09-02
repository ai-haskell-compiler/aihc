module GHC.List
  ( map,
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

import Prelude

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (value : values) = Just (value, values)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc values = Just (init values, last values)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ initial [] = initial
foldl' combine initial (value : values) =
  let next = combine initial value
   in next `seq` foldl' combine next values

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
