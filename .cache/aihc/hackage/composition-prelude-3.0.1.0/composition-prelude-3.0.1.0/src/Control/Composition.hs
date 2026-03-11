module Control.Composition
    ( -- * Postcomposition
      (.*)
    , (.**)
    , (.***)
    , (.****)
    , (.*****)
    , (.******)
    -- * Precomposition
    , (-.)
    , (.@)
    , (.@@)
    , (.@@@)
    , (.@@@@)
    , (.@@@@@)
    -- * Monadic postcomposition
    , (<=*<)
    , (<=**<)
    , (>=**>)
    , (>=*>)
    -- * Monadic precomposition
    , (<-=*<)
    , (>-=*>)
    , (<-=**<)
    , (>-=**>)
    -- * Between combinators
    , between
    , (~@~)
    , betweenM
    , (<~@~<)
    -- * Fancy function application
    , (-$)
    -- * Monadic helpers
    , bisequence'
    , (.$)
    , onM
    -- * Monadic actions
    , axe
    , biaxe
    -- * Composition with lists of functions
    , thread
    , threadM
    -- * Tuple helpers
    , both
    , dup
    , (+>)
    -- * J inspired
    , (&:)
    -- * Reëxports from base
    , (<=<)
    , (>=>)
    , (<**>)
    , (&)
    , (<&>)
    , fix
    , on
    ) where

import           Control.Applicative
import           Control.Arrow       ((***))
import           Control.Monad
import           Data.Foldable
import           Data.Function       (fix, on, (&))
import           Data.Functor        ((<&>))
import           Data.Traversable
import           Prelude             hiding (foldr)

infixr 8 .*
infixr 8 .**
infixr 8 .***
infixr 8 .****
infixr 8 .*****
infixr 8 .******
infixr 8 -.
infixr 8 .@
infixr 8 .@@
infixr 8 .@@@
infixr 8 .@@@@
infixr 8 .@@@@@
infixl 0 &:
infixl 3 .$
infixl 8 -$
infixl 8 ~@~
infixl 8 <~@~<
infixr 1 <=*<
infixr 1 >=*>
infixr 1 <=**<
infixr 1 >=**>
infixr 1 <-=*<
infixr 1 >-=*>
infixr 1 <-=**<
infixr 1 >-=**>
infixr 6 +>

axe :: (Traversable t, Applicative f) => t (a -> f ()) -> a -> f ()
axe = sequenceA_ .* sequenceA

bisequence' :: (Traversable t, Applicative f) => t (a -> b -> f c) -> a -> b -> t (f c)
bisequence' = sequenceA .* sequenceA

biaxe :: (Traversable t, Applicative f) => t (a -> b -> f ()) -> a -> b -> f ()
biaxe = sequenceA_ .** bisequence'

-- | Pronounced \'appose\'. Synonym for 'on'
--
-- @since 3.0.0.0
(&:) :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(&:) = on

-- | Infix synonym for 'both'
--
-- @since 3.0.0.0
(+>) :: (a -> b) -> (a, a) -> (b, b)
(+>) = both

both :: (a -> b) -> (a, a) -> (b, b)
both = join (***)

-- | @since 2.0.1.0
dup :: a -> (a, a)
dup = join (,)

-- | Infix version of 'join'
--
-- As an example, one could use this to rewrite
--
-- > between (char '"') (char '"')
--
-- to
--
-- > between .$ (char '"')
--
-- Or
--
-- > fromEither :: Either a a -> a
-- > fromEither = either id id
--
-- to
--
-- > fromEither :: Either a a -> a
-- > fromEither = either .$ id
--
-- @since 2.0.2.0
(.$) :: Monad m => m (m a) -> m a
(.$) = join

-- | Backwards function application. This is an infix synonym for 'flip'
(-$) :: (a -> b -> c) -> b -> a -> c
(-$) = flip

(.*) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.*) f g = \x y -> f (g x y)

(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) f g = \x y z -> f (g x y z)

(.***) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.***) f g = \w x y z -> f (g w x y z)

-- | @since 1.0.0.0
(.****) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.****) f g = \v w x y z -> f (g v w x y z)

-- | @since 2.0.5.0
(.*****) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.*****) f g = \u v w x y z -> f (g u v w x y z)

-- | @since 2.0.5.0
(.******) :: (h -> i) -> (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> i
(.******) f g = \t u v w x y z -> f (g t u v w x y z)

-- | A monadic version of '.*'. Compare '<=<'.
--
-- As an example, one could use this to rewrite
--
-- > \x y z -> f (g x y z) z
--
-- to
--
-- > f <=*< g
--
-- @since 1.5.2.0
(<=*<) :: Monad m => (c -> m d) -> (a -> b -> m c) -> a -> b -> m d
(<=*<) f g = \x y -> f =<< g x y

-- | The bleeding fish operator
--
-- @since 1.5.2.0
(<=**<) :: Monad m => (d -> m e) -> (a -> b -> c -> m d) -> a -> b -> c -> m e
(<=**<) f g = \x y z -> f =<< g x y z

-- | Compare '>=>'.
--
-- @since 1.5.2.0
(>=*>) :: Monad m => (a -> b -> m c) -> (c -> m d) -> a -> b -> m d
(>=*>) g f = \x y -> f =<< g x y

-- | @since 1.5.2.0
(>=**>) :: Monad m => (a -> b -> c -> m d) -> (d -> m e) -> a -> b -> c -> m e
(>=**>) g f = \x y z -> f =<< g x y z

-- | @since 1.5.2.0
(<-=*<) :: Monad m => (b -> m c) -> (a -> c -> m d) -> a -> b -> m d
(<-=*<) f g = \x y -> g x =<< f y

-- | @since 1.5.2.0
(>-=*>) :: Monad m => (a -> c -> m d) -> (b -> m c) -> a -> b -> m d
(>-=*>) g f = \x y -> g x =<< f y

-- | @since 1.5.2.0
(<-=**<) :: Monad m => (c -> m d) -> (a -> b -> d -> m e) -> a -> b -> c -> m e
(<-=**<) f g = \x y z -> g x y =<< f z

-- | @since 1.5.2.0
(>-=**>) :: Monad m => (a -> b -> d -> m e) -> (c -> m d) -> a -> b -> c -> m e
(>-=**>) g f = \x y z -> g x y =<< f z

-- | @since 2.0.3.0
(.@) :: (b -> c) -> (a -> c -> d) -> a -> b -> d
(.@) f g = \x y -> g x (f y)

-- | @since 2.0.3.0
(.@@) :: (c -> d) -> (a -> b -> d -> e) -> a -> b -> c -> e
(.@@) f g = \x y z -> g x y (f z)

-- | @since 2.0.3.0
(.@@@) :: (d -> e) -> (a -> b -> c -> e -> f) -> a -> b -> c -> d -> f
(.@@@) f g = \w x y z -> g w x y (f z)

-- | @since 2.0.3.0
(.@@@@) :: (e -> f) -> (a -> b -> c -> d -> f -> g) -> a -> b -> c -> d -> e -> g
(.@@@@) f g = \v w x y z -> g v w x y (f z)

-- | @since 3.0.0.0
(.@@@@@) :: (f -> g) -> (a -> b -> c -> d -> e -> g -> h) -> a -> b -> c -> d -> e -> f -> h
(.@@@@@) f g = \u v w x y z -> g u v w x y (f z)

-- | Backwards function composition. This is a specialization of '<&>', but it
-- has a different fixity.
(-.) :: (a -> b) -> (b -> c) -> a -> c
(-.) = (<&>)

-- infixl 1
-- (.&) :: a -> b -> (a -> b -> c) -> c
--
-- infixr 0
-- (.$) :: (a -> b -> c) -> a -> b -> c

-- | @since 1.1.0.1
thread :: Foldable t => t (a -> a) -> a -> a
thread = foldr (.) id
{-# INLINE thread #-}

threadM :: (Monad m, Foldable t, Applicative m) => t (a -> m a) -> a -> m a
threadM = foldr (<=<) pure
{-# INLINE threadM #-}

-- | Can be used to rewrite
--
-- > \g -> f . g . h
--
-- to
--
-- > between f h
--
-- @since 1.5.3.0
between :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
between f g = (f .) . (. g)

-- @since 1.5.3.0
betweenM :: Monad m => (c -> m d) -> (a -> m b) -> (b -> m c) -> a -> m d
betweenM f g = (f <=<) . (<=< g)

-- @since 1.5.3.0
(~@~) :: (c -> d) -> (a -> b) -> (b -> c) -> a -> d
(~@~) = between

-- @since 1.5.3.0
(<~@~<) :: Monad m => (c -> m d) -> (a -> m b) -> (b -> m c) -> a -> m d
(<~@~<) = betweenM

-- @since 3.0.1.0
onM :: Monad m => (b -> b -> m c) -> (a -> m b) -> a -> a -> m c
onM g f x y = do {x' <- f x; y' <- f y; g x' y'}
