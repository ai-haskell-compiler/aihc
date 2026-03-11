{-# OPTIONS -fno-warn-orphans #-}

module Data.Function.Eq
    (
    ) where

import Data.Countable
import Data.Empty
import Data.Expression
import Data.Foldable hiding (find)
import Data.Function.Traversable ()
import Data.Searchable
import Data.Traversable
import Prelude

instance (Searchable a, Eq b) => Eq (a -> b) where
    p == q = forevery (\a -> p a == q a)

instance (Finite a, Countable b) => Countable (a -> b) where
    countPrevious =
        case isoCountableFn of
            MkIsoCountable encode decode -> (fmap decode) . countPrevious . encode
    countMaybeNext =
        case isoCountableFn of
            MkIsoCountable encode decode -> (fmap decode) . countMaybeNext . (fmap encode)

instance (Finite a, AtLeastOneCountable b) => AtLeastOneCountable (a -> b) where
    countFirst = \_ -> countFirst

setpair :: (Eq a) => (a, b) -> (a -> b) -> (a -> b)
setpair (a', b') _ a
    | a == a' = b'
setpair _ ab a = ab a

data IsoCountable x =
    forall l. (Countable l) =>
                  MkIsoCountable (x -> l)
                                 (l -> x)

isoCountableFn :: (Finite a, Countable b) => IsoCountable (a -> b)
isoCountableFn = makeFromList allValues
  where
    makeFromList :: (Eq a, Countable b) => [a] -> IsoCountable (a -> b)
    makeFromList [] = MkIsoCountable (\_ -> ()) (\a -> seq a undefined)
    makeFromList (a:as) =
        case makeFromList as of
            MkIsoCountable encode decode ->
                MkIsoCountable (\ab -> (ab a, encode ab)) (\(b, l) -> setpair (a, b) (decode l))

data IsoInfiniteCountable x =
    forall l. (InfiniteCountable l) =>
                  MkIsoInfiniteCountable (x -> l)
                                         (l -> x)

isoInfiniteCountableFn :: (Finite a, AtLeastOneCountable a, InfiniteCountable b) => IsoInfiniteCountable (a -> b)
isoInfiniteCountableFn = makeFromList allValues
  where
    makeFromList :: (Eq a, InfiniteCountable b) => [a] -> IsoInfiniteCountable (a -> b)
    makeFromList [] = undefined
    makeFromList [a] = MkIsoInfiniteCountable (\ab -> ab a) (\b -> setpair (a, b) (\a' -> seq a' undefined))
    makeFromList (a:as) =
        case makeFromList as of
            MkIsoInfiniteCountable encode decode ->
                MkIsoInfiniteCountable (\ab -> (ab a, encode ab)) (\(b, l) -> setpair (a, b) (decode l))

instance (Finite a, AtLeastOneCountable a, InfiniteCountable b) => InfiniteCountable (a -> b) where
    countNext =
        case isoInfiniteCountableFn of
            MkIsoInfiniteCountable encode decode -> decode . countNext . (fmap encode)

instance (Finite a, Finite b) => Finite (a -> b) where
    allValues = sequenceA (\_ -> allValues)
    assemble abfr =
        runValueExpression
            (Data.Foldable.foldr assemble1 (\ab -> ClosedExpression (abfr ab)) allValues (\_ -> error "missing value"))
      where
            -- assemble1 :: a -> ((a -> b) -> Expression a b f r) -> (a -> b) -> Expression a b f r
        assemble1 a0 aber x =
            OpenExpression
                a0
                (assemble
                     (\b0 ->
                          aber
                              (\a ->
                                   if a == a0
                                       then b0
                                       else x a)))

instance (AtLeastOneCountable a, Finite a, Empty b) => Empty (a -> b) where
    never ab = never (ab countFirst)
