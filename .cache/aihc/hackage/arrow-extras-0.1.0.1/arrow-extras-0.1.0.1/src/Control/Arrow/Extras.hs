module Control.Arrow.Extras where

import Control.Arrow

infixl 0 >*>
infixr 1 $>>, >>$
infixr 1 $<<, <<$

-- | Feed the output of second arrow into the output of the first arrow.
-- Can be used to emulate Applicative <*> like usage:
--
-- @
-- foo :: Appplicative f => f a -> f b -> f (a, b)
-- (,) <$> a <*> b
-- @
--
-- The arrow version is:
--
-- @
-- foo :: Arrow p => p x a -> p x b -> p x (a, b)
-- arr (const (,)) >*> a >*> b
-- @
(>*>) :: Arrow p => p a (b -> c)
    -> p a b
    -> p a c
af >*> ab = af &&& ab >>^ (fst <*> snd)

-- | precomposition with a pure value
(<<$) :: Arrow a => a c d -> c -> a x d
p <<$ v = p <<^ const v

-- | postcomposition with a pure value
(>>$) :: Arrow a => a b c -> d -> a b d
p >>$ v = p >>^ const v

-- | postcomposition with a pure value (right-to-left variant)
($<<) :: Arrow a => d -> a b c -> a b d
v $<< p = const v ^<< p

-- | precomposition with a pure value (right-to-left variant)
($>>) :: Arrow a => c -> a c d -> a x d
v $>> p = const v ^>> p
