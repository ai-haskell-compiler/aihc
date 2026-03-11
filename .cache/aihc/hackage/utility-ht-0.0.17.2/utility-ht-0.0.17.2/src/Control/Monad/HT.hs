module Control.Monad.HT where

import qualified Control.Monad as M
import qualified Data.List as List
import Prelude hiding (repeat, until, )


infixr 1 <=<

{- |
Also present in newer versions of the 'base' package.
-}
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(<=<) f g = (f =<<) . g


{- |
Monadic 'List.repeat'.
-}
repeat :: (Monad m) => m a -> m [a]
repeat x =
   let go = lift2 (:) x go in go

nest :: (Monad m) => Int -> (a -> m a) -> a -> m a
nest n f x0 = M.foldM (\x () -> f x) x0 (List.replicate n ())

{-# DEPRECATED untilM "use M.until" #-}
{- | repeat action until result fulfills condition -}
until, untilM :: (Monad m) => (a -> Bool) -> m a -> m a
untilM = until
until p m =
   let go =
         do x <- m
            if p x
              then return x
              else go
   in  go

{-# DEPRECATED iterateLimitM "use M.iterateLimit" #-}
{- | parameter order equal to that of 'nest' -}
iterateLimit, iterateLimitM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateLimitM = iterateLimit
iterateLimit m f =
   let aux n x =
          lift (x:) $
          if n==0
            then return []
            else aux (n-1) =<< f x
   in  aux m

{- |
I think this makes only sense in a lazy monad
like @Trans.State.Lazy@ or @IO.Lazy@.
-}
iterate :: Monad m => (a -> m a) -> a -> m [a]
iterate f =
   let go x = lift (x:) $ go =<< f x
   in  go

{- |
Lazy monadic conjunction.
That is, when the first action returns @False@,
then @False@ is immediately returned, without running the second action.
-}
andLazy :: (Monad m) => m Bool -> m Bool -> m Bool
andLazy m0 m1 =
   m0 >>= \b ->
   if b
     then m1
     else return False

{- |
Lazy monadic disjunction.
That is, when the first action returns @True@,
then @True@ is immediately returned, without running the second action.
-}
orLazy :: (Monad m) => m Bool -> m Bool -> m Bool
orLazy m0 m1 =
   m0 >>= \b ->
   if b
     then return True
     else m1


void :: (Monad m) => m a -> m ()
void = lift (const ())

for :: Monad m => [a] -> (a -> m b) -> m [b]
for = M.forM

map :: Monad m => (a -> m b) -> [a] -> m [b]
map = M.mapM

zipWith :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWith = M.zipWithM

chain :: (Monad m) => [a -> m a] -> (a -> m a)
chain = foldr (flip (<=<)) return

-- there is also mfilter, but this should be part of Control.Monad.Plus
filter :: Monad m => (a -> m Bool) -> [a] -> m [a]
filter = M.filterM

replicate :: Monad m => Int -> m a -> m [a]
replicate = M.replicateM

lift :: Monad m => (a -> r) -> m a -> m r
lift = M.liftM

lift2 ::
   Monad m => (a -> b -> r) -> m a -> m b -> m r
lift2 = M.liftM2

lift3 ::
   Monad m => (a -> b -> c -> r) -> m a -> m b -> m c -> m r
lift3 = M.liftM3

lift4 ::
   Monad m =>
   (a -> b -> c -> d -> r) -> m a -> m b -> m c -> m d -> m r
lift4 = M.liftM4

lift5 ::
   Monad m =>
   (a -> b -> c -> d -> e -> r) ->
   m a -> m b -> m c -> m d -> m e -> m r
lift5 = M.liftM5


{-
that's just (=<<)

liftJoin :: (Monad m) => (a -> m b) -> m a -> m b
liftJoin f ma =
   join (lift f ma)
-}

liftJoin2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoin2 f ma mb =
   M.join (lift2 f ma mb)

liftJoin3 :: (Monad m) => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
liftJoin3 f ma mb mc =
   M.join (lift3 f ma mb mc)

liftJoin4 ::
   (Monad m) =>
   (a -> b -> c -> d -> m e) ->
   m a -> m b -> m c -> m d -> m e
liftJoin4 f ma mb mc md =
   M.join (lift4 f ma mb mc md)

liftJoin5 ::
   (Monad m) =>
   (a -> b -> c -> d -> e -> m f) ->
   m a -> m b -> m c -> m d -> m e -> m f
liftJoin5 f ma mb mc md me =
   M.join (lift5 f ma mb mc md me)

{-
Add functions with restricted types?
Shall their element types be monoids?
Should we add these functions to a Foldable.HT module
in order to save the underscore?

(>>)
mapM_
zipWithM_
sequence_
...
-}
