module Data.Maybe.HT where

import Data.Maybe (fromMaybe, )
import Control.Monad (msum, )

{- $setup
>>> import Control.Monad (guard)
-}

{-
It was proposed as addition to Data.Maybe and rejected at that time.
<http://www.haskell.org/pipermail/libraries/2004-July/002381.html>
-}
{- | Returns 'Just' if the precondition is fulfilled.
prop> \b x ->  (guard b >> x)  ==  (toMaybe b =<< (x::Maybe Char))
-}
{-# INLINE toMaybe #-}
toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x


infixl 6 ?->

{- |
This is an infix version of 'fmap'
for writing 'Data.Bool.HT.select' style expressions
using test functions, that produce 'Maybe's.

The precedence is chosen to be higher than '(:)',
in order to allow:

> alternatives default $
>    checkForA ?-> (\a -> f a) :
>    checkForB ?-> (\b -> g b) :
>    []

The operation is left associative
in order to allow to write

> checkForA ?-> f ?-> g

which is equivalent to

> checkForA ?-> g . f

due to the functor law.
-}
(?->) :: Maybe a -> (a -> b) -> Maybe b
(?->) = flip fmap

alternatives :: a -> [Maybe a] -> a
alternatives deflt = fromMaybe deflt . msum
