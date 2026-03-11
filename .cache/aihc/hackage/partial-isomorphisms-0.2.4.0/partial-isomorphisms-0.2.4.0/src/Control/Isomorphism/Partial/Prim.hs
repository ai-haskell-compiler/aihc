{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Isomorphism.Partial.Prim
  ( Iso ()
  , inverse
  , apply
  , unapply
  , IsoFunctor ((<$>))
  , ignore
  , (***)
  , (|||)
  , associate
  , commute
  , unit
  , element
  , subset
  , iterate
  , distribute
  ) where

import Prelude ()

import Control.Monad (liftM2, (>=>), fmap, mplus)
import Control.Category (Category (id, (.)))

import Data.Bool (Bool, otherwise)
import Data.Either (Either (Left, Right))
import Data.Eq (Eq ((==)))
import Data.Maybe (Maybe (Just, Nothing))

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))

inverse :: Iso alpha beta -> Iso beta alpha
inverse (Iso f g) = Iso g f

apply :: Iso alpha beta -> alpha -> Maybe beta
apply (Iso f _) = f

unapply  ::  Iso alpha beta -> beta -> Maybe alpha
unapply  =   apply . inverse

instance Category Iso where
  g . f  =  Iso  (apply f >=> apply g)
                 (unapply g >=> unapply f)
  id     =  Iso  Just Just

infix 5 <$>

class IsoFunctor f where
  (<$>) :: Iso alpha beta -> (f alpha -> f beta)

ignore :: alpha -> Iso alpha ()
ignore x = Iso f g where
  f _   =  Just ()
  g ()  =  Just x

-- | the product type constructor `(,)` is a bifunctor from
-- `Iso` $\times$ `Iso` to `Iso`, so that we have the
-- bifunctorial map `***` which allows two separate isomorphisms
-- to work on the two components of a tuple.
(***) :: Iso alpha beta -> Iso gamma delta -> Iso (alpha, gamma) (beta, delta)
i *** j = Iso f g where
  f (a, b) = liftM2 (,) (apply i a) (apply j b)
  g (c, d) = liftM2 (,) (unapply i c) (unapply j d)

-- | The mediating arrow for sums constructed with `Either`.
-- This is not a proper partial isomorphism because of `mplus`.
(|||) :: Iso alpha gamma -> Iso beta gamma -> Iso (Either alpha beta) gamma
i ||| j = Iso f g where
  f (Left x) = apply i x
  f (Right x) = apply j x
  g y = (Left `fmap` unapply i y) `mplus` (Right `fmap` unapply j y)


-- | Nested products associate.
associate :: Iso (alpha, (beta, gamma)) ((alpha, beta), gamma)
associate = Iso f g where
  f (a, (b, c)) = Just ((a, b), c)
  g ((a, b), c) = Just (a, (b, c))

-- | Products commute.
commute :: Iso (alpha, beta) (beta, alpha)
commute = Iso f f where
  f (a, b) = Just (b, a)

-- | `()` is the unit element for products.
unit :: Iso alpha (alpha, ())
unit = Iso f g where
  f a = Just (a, ())
  g (a, ()) = Just a

-- | Products distribute over sums.
distribute  ::  Iso (alpha, Either beta gamma) (Either (alpha, beta) (alpha, gamma))
distribute  =   Iso f g where
  f (a, Left   b)    =  Just (Left   (a, b))
  f (a, Right  c)    =  Just (Right  (a, c))
  g (Left   (a, b))  =  Just (a,  Left   b)
  g (Right  (a, b))  =  Just (a,  Right  b)

-- | `element x` is the partial isomorphism between `()` and the
-- singleton set which contains just `x`.
element :: Eq alpha => alpha -> Iso () alpha
element x = Iso
  (\_ -> Just x)
  (\b -> if x == b then Just () else Nothing)

-- | For a predicate `p`, `subset p` is the identity isomorphism
-- restricted to elements matching the predicate.
subset :: (alpha -> Bool) -> Iso alpha alpha
subset p = Iso f f where
  f x | p x = Just x | otherwise = Nothing

iterate :: Iso alpha alpha -> Iso alpha alpha
iterate step = Iso f g where
  f = Just . driver (apply step)
  g = Just . driver (unapply step)

  driver :: (alpha -> Maybe alpha) -> (alpha -> alpha)
  driver step' state
    =  case step' state of
         Just state'  ->  driver step' state'
         Nothing      ->  state
