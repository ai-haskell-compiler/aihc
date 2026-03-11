module Data.Accessor.Private where

{- |
The accessor function we use,
has a record value as first argument
and returns the content of a specific record field
and a function that allows to overwrite that field with a new value.

In former version of a package
we used a function that resembled the state monad.
However this required to use an 'undefined'
in the implementation of the @get@ function.
-}
newtype T r a  =  Cons {decons :: r -> (a, a -> r)}

compose :: T a b -> T b c -> T a c
compose f g = Cons $ \ aOld ->
   let (bOld, aSetB) = decons f aOld
       (cOld, bSetC) = decons g bOld
   in  (cOld, aSetB . bSetC)

self :: T r r
self = Cons $ \r -> (r, id)
