module Data.Function
  ( id,
    const,
    (.),
    flip,
    ($),
    (&),
    on,
    fix,
    applyWhen,
  )
where

import Prelude (Bool (..), const, flip, id, ($), (.))

(&) :: a -> (a -> b) -> b
value & function = function value

infixl 1 &

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on combine project left right = combine (project left) (project right)

infixl 0 `on`

fix :: (a -> a) -> a
fix function = let result = function result in result

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True function value = function value
applyWhen False _ value = value
