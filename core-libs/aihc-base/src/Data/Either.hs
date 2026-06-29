module Data.Either
  ( Either (..),
    either,
    fromLeft,
    fromRight,
    isLeft,
    isRight,
    lefts,
    rights,
    partitionEithers,
  )
where

import Prelude (Bool (..), Either (..))

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g value =
  case value of
    Left x -> f x
    Right y -> g y

fromLeft :: a -> Either a b -> a
fromLeft fallback value =
  case value of
    Left x -> x
    Right _ -> fallback

fromRight :: b -> Either a b -> b
fromRight fallback value =
  case value of
    Left _ -> fallback
    Right y -> y

isLeft :: Either a b -> Bool
isLeft value =
  case value of
    Left _ -> True
    Right _ -> False

isRight :: Either a b -> Bool
isRight value =
  case value of
    Left _ -> False
    Right _ -> True

lefts :: [Either a b] -> [a]
lefts values =
  case values of
    [] -> []
    value : xs ->
      case value of
        Left x -> x : lefts xs
        Right _ -> lefts xs

rights :: [Either a b] -> [b]
rights values =
  case values of
    [] -> []
    value : xs ->
      case value of
        Left _ -> rights xs
        Right y -> y : rights xs

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers values =
  case values of
    [] -> ([], [])
    value : xs ->
      case value of
        Left x ->
          case partitionEithers xs of
            (ls, rs) -> (x : ls, rs)
        Right y ->
          case partitionEithers xs of
            (ls, rs) -> (ls, y : rs)
