module Data.List
  ( intersperse,
    isSuffixOf,
    stripPrefix,
  )
where

import Prelude (Bool (..), Eq (..), Maybe (..), reverse, (&&))

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse separator (value : values) = value : prependSeparators separator values

prependSeparators :: a -> [a] -> [a]
prependSeparators _ [] = []
prependSeparators separator (value : values) = separator : value : prependSeparators separator values

isSuffixOf :: (Eq a) => [a] -> [a] -> Bool
isSuffixOf suffix value = reverse suffix `isPrefixOf` reverse value

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (left : lefts) (right : rights) = left == right && isPrefixOf lefts rights

stripPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
stripPrefix [] value = Just value
stripPrefix _ [] = Nothing
stripPrefix (left : lefts) (right : rights) =
  if left == right
    then stripPrefix lefts rights
    else Nothing
