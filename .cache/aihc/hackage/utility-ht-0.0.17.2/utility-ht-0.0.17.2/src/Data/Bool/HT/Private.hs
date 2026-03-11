module Data.Bool.HT.Private where

import Data.List  as List  (find, )
import Data.Maybe as Maybe (fromMaybe, )

{- |
@if-then-else@ as function.

Example:

> if' (even n) "even" $
> if' (isPrime n) "prime" $
> "boring"
-}
{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

{-|
The same as 'if'', but the name is chosen
such that it can be used for GHC-7.0's rebindable if-then-else syntax.
-}
{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse = if'


{-|
From a list of expressions choose the one,
whose condition is true.

Example:

> select "boring" $
>   (even n, "even") :
>   (isPrime n, "prime") :
>   []
-}
{-# INLINE select #-}
select, select0, select1 :: a -> [(Bool, a)] -> a
select  def = maybe def snd . find fst
select0 def = fromMaybe def . lookup True
select1     = foldr (uncurry if')


zipIf :: [Bool] -> [a] -> [a] -> [a]
zipIf = zipWith3 if'

infixr 1 ?:

{- |
Like the @?@ operator of the C progamming language.

>>> True ?: ("yes", "no")
"yes"
>>> False ?: ("yes", "no")
"no"
-}
{-# INLINE (?:) #-}
(?:) :: Bool -> (a,a) -> a
(?:) = uncurry . if'


-- precedence below (||) and (&&)
infixr 1 `implies`

{- |
Logical operator for implication.

Funnily because of the ordering of 'Bool' it holds:

prop> \a b -> implies a b == (a<=b)
-}
{-# INLINE implies #-}
implies :: Bool -> Bool -> Bool
implies prerequisite conclusion =
   not prerequisite || conclusion
