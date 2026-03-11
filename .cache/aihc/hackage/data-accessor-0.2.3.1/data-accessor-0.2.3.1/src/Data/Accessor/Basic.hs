{- |
This module defines the @Accessor@ type.
It should be imported with qualification.
-}
module Data.Accessor.Basic (
   T, fromSetGet, fromLens, fromWrapper,
   self, null, result,
   set, (^=), compose,
   get, (^.),
   modify, (^:),
   (.>), (<.),
   ($%),
   merge,
   ) where

import qualified Data.Accessor.Private as A
import Data.Accessor.Private (T(..), )
import Prelude hiding (null)


-- * Define and construct accessors

fromSetGet :: (a -> r -> r) -> (r -> a) -> T r a
fromSetGet setF getF =
   Cons $ \r -> (getF r, flip setF r)

fromLens :: (r -> (a, a -> r)) -> T r a
fromLens = Cons

{- |
If an object is wrapped in a @newtype@,
you can generate an @Accessor@ to the unwrapped data
by providing a wrapper and an unwrapper function.
The set function is simpler in this case,
since no existing data must be kept.
Since the information content of the wrapped and unwrapped data is equivalent,
you can swap wrapper and unwrapper.
This way you can construct an @Accessor@
that treats a record field containing an unwrapped object
like a field containing a wrapped object.

> newtype A = A {unA :: Int}
>
> access :: Accessor.T A Int
> access = fromWrapper A unA

We could also have called this function @fromBijection@,
since it must hold @wrap . unwrap = id@ and @unwrap . wrap = id@.
-}
fromWrapper :: (b -> a) -> (a -> b) -> T a b
fromWrapper wrap unwrap =
   fromSetGet (const . wrap) unwrap

{- test whether the example can be compiled
newtype A = A {unA :: Int}

access :: T A Int
access = fromWrapper A unA
-}


-- Simple accessors

{- |
Access the record itself
-}
self :: T r r
self = A.self
-- self = fromSetGet const id

{- |
Access a (non-existing) element of type @()@
-}
null :: T r ()
null = fromSetGet (flip const) (const ())

{- |
@result a@ accesses the value of a function for argument @a@.
It is not very efficient to build a function
from setting all of its values using this accessor,
since every access to a function adds another @if-then-else@.

Also see semantic editor combinators,
that allow to modify all function values of a function at once.
Cf. <http://conal.net/blog/posts/semantic-editor-combinators/>
-}
result :: Eq a => a -> T (a -> b) b
result ai =
   fromSetGet (\r f a -> if a==ai then r else f a) ($ ai)


-- * Apply accessors, similar to State methods

{- | Set the value of a field. -}
set :: T r a -> a -> r -> r
set f a r = snd (decons f r) a


infixr 5 ^=, ^:


{- |
'set' as infix operator.
This lets us write @first ^= 2+3 $ second ^= 5+7 $ record@.
-}
(^=) :: T r a -> a -> (r -> r)
(^=) = set

{-
{- | Set many fields at once.

This function could also be used for initialisation of record,
if record value with undefined fields is provided.

Drawback:
Since all types in a list must have the same type,
you can set only values of the same type.
-}
setMany :: [r -> (a, r)] -> r -> r
setMany = flip (foldl (\x f -> snd (f x)))
-}

{- |
This is a general function,
but it is especially useful for setting many values of different type at once.
-}
compose :: [r -> r] -> r -> r
compose = flip (foldl (flip id))

{- | Get the value of a field. -}
get :: T r a -> r -> a
get f = fst . decons f

infixl 8 ^.

{- |
'get' as infix operator.
This lets us write @record^.field^.subfield@.
This imitates Modula II syntax.
-}
(^.) :: r -> T r a -> a
(^.) = flip get


{- | Transform the value of a field by a function. -}
modify :: T r a -> (a -> a) -> (r -> r)
modify f g rOld =
   let (a,rSetA) = decons f rOld
   in  rSetA (g a)


{- |
'modify' as infix operator.
This lets us write
@field^:subfield^:(2*) $ record@,
@record$%field^:subfield^:(2*)@
or @record$%field^:subfield^:(const 1)@.
-}
(^:) :: T r a -> (a -> a) -> (r -> r)
(^:) = modify


infixl 0 $%

{- |
Flipped version of '($)'.
-}
-- ToDo: could be re-exported from utility-ht
($%) :: a -> (a -> b) -> b
($%) = flip ($)


-- * Accessor combinators

infixl 9 .>

{- |
Accessor composition:
Combine an accessor with an accessor to a sub-field.
Speak \"stack\".
-}
(.>) :: T a b -> T b c -> T a c
(.>) = A.compose

{-
This could be used for a Category instance of T.
-}


infixr 9 <.

{- |
Accessor composition the other direction.

> (<.) = flip (.>)

You may also use the @(.)@ operator from Category class.
-}
(<.) :: T b c -> T a b -> T a c
(<.) = flip A.compose


{- |
Merge the accessors to two independent fields.

Independency means, it must hold:

> set (merge accA accB) (a,b) = set (merge accB accA) (b,a)

You may construct smart accessors
by composing a merged accessor with a @fromWrapper@ accessor.

This is a special case of the more general @Point@ concept
in the package @fclabels@.
-}
merge :: T a b -> T a c -> T a (b,c)
merge accB accC =
   fromSetGet
      (\(b,c) -> set accB b . set accC c)
      (\a -> (get accB a, get accC a))
