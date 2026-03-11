module Iri.Optics.Basics where

import Iri.Prelude

type Lens s t a b = forall f. (Functor f) => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

type Prism' s a = Prism s s a a

type Traversal s t a b = forall f. (Applicative f) => (a -> f b) -> s -> f t

type Traversal' s a = Traversal s s a a

type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Iso' s a = Iso s s a a

{-# INLINE prism #-}
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt seta =
  dimap seta (either pure (fmap bt)) . right'

{-# INLINE prism' #-}
prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a
prism' as sma =
  prism as (\s -> maybe (Left s) Right (sma s))

{-# INLINE lens #-}
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s =
  sbt s <$> afb (sa s)
