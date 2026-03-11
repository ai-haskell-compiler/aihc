module Data.Function.Contravariant.Syntax where

(-.) :: (a -> b)
     -> (b -> c)
     -> a -> c
(-.) = flip (.)

infixr 8 -.

(-.:) :: (b -> c)
      -> (a -> c -> d)
      -> a -> b -> d
(-.:) p q a b = q a $ p b

infixr 8 -.:

(-.*) = (-.:)

infixr 8 -.*

(-.:.) :: (c -> d)
       -> (a -> b -> d -> e)
       -> a -> b -> c -> e
(-.:.) p q a b c = q a b $ p c

infixr 8 -.:.

(-.**) = (-.:.)

infixr 8 -.**

(-.::) :: (d -> e)
       -> (a -> b -> c -> e -> f)
       -> a -> b -> c -> d -> f
(-.::) p q a b c d = q a b c $ p d

infixr 8 -.::

(-.***) = (-.::)

infixr 8 -.***

(-.::.) :: (e -> f)
        -> (a -> b -> c -> d -> f -> g)
        -> a -> b -> c -> d -> e -> g
(-.::.) p q a b c d e = q a b c d $ p e

infixr 8 -.::.

(-.****) = (-.::.)

infixr 8 -.****

(-.:::) :: (f -> g)
        -> (a -> b -> c -> d -> e -> g -> h)
        -> a -> b -> c -> d -> e -> f -> h
(-.:::) p q a b c d e f = q a b c d e $ p f

infixr 8 -.:::

(-.*****) = (-.:::)

infixr 8 -.*****

(-.:::.) :: (g -> h)
         -> (a -> b -> c -> d -> e -> f -> h -> i)
         -> a -> b -> c -> d -> e -> f -> g -> i
(-.:::.) p q a b c d e f g = q a b c d e f $ p g

infixr 8 -.:::.

(-.******) = (-.:::.)

infixr 8 -.******

(-.::::) :: (h -> i)
         -> (a -> b -> c -> d -> e -> f -> g -> i -> j)
         -> a -> b -> c -> d -> e -> f -> g -> h -> j
(-.::::) p q a b c d e f g h = q a b c d e f g $ p h

infixr 8 -.::::

(-.*******) = (-.::::)

infixr 8 -.*******

(-.::::.) :: (i -> j)
          -> (a -> b -> c -> d -> e -> f -> g -> h -> j -> k)
          -> a -> b -> c -> d -> e -> f -> g -> h -> i -> k
(-.::::.) p q a b c d e f g h i = q a b c d e f g h $ p i

infixr 8 -.::::.

(-.********) = (-.::::.)

infixr 8 -.********
