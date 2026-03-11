-- this is also used by synthesizer-dimensional and synthesizer-inference
module Synthesizer.ApplicativeUtility where

import Control.Arrow (Arrow, (<<<), )
import Control.Monad.Fix (fix, )
import Control.Applicative (Applicative, (<*>), (<$>), liftA2, )
import Data.Traversable (Traversable, sequenceA, )


{-# INLINE liftA4 #-}
liftA4 :: Applicative f =>
   (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

{-# INLINE liftA5 #-}
liftA5 :: Applicative f =>
   (a -> b -> c -> d -> e -> g) -> f a -> f b -> f c -> f d -> f e -> f g
liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

{-# INLINE liftA6 #-}
liftA6 :: Applicative f =>
   (a -> b -> c -> d -> e -> g -> h) -> f a -> f b -> f c -> f d -> f e -> f g -> f h
liftA6 f a b c d e g = f <$> a <*> b <*> c <*> d <*> e <*> g


{- |
Create a loop (feedback) from one node to another one.
That is, compute the fix point of a process iteration.
-}
{-# INLINE loop #-}
loop :: (Functor f) =>
      f (a -> a)  {-^ process chain that shall be looped -}
   -> f a
loop = fmap fix


infixl 0 $:, $::, $^, $#
infixr 9 .:, .^

{- |
This corresponds to 'Control.Applicative.<*>'
-}
{-# INLINE ($:) #-}
($:) :: (Applicative f) => f (a -> b) -> f a -> f b
($:) = (<*>)

{- |
Instead of @mixMulti $:: map f xs@
the caller should write @mixMulti $: mapM f xs@
in order to save the user from learning another infix operator.
-}
{-# INLINE ($::) #-}
($::) :: (Applicative f, Traversable t) =>
   f (t a -> b) -> t (f a) -> f b
($::) f arg = f $: sequenceA arg
-- ($::) f arg sr = f sr (map ($sr) arg)

{-# INLINE (.:) #-}
(.:) :: (Applicative f, Arrow arrow) =>
   f (arrow b c) -> f (arrow a b) -> f (arrow a c)
(.:) = liftA2 (<<<)
-- (.:) f g sr x = f sr (g sr x)
-- (.:) f g sr x = ($:) f (flip g x) sr

{-# INLINE ($^) #-}
($^) :: (Functor f) => (a -> b) -> f a -> f b
($^) = fmap
-- ($^) = (.)
-- ($^) f x = pure f $: x

{-# INLINE (.^) #-}
(.^) :: (Functor f, Arrow arrow) =>
   arrow b c -> f (arrow a b) -> f (arrow a c)
(.^) f = fmap (f <<<)
-- (.^) f = (.:) (pure f)

{-# INLINE ($#) #-}
($#) :: (Functor f) => f (a -> b) -> a -> f b
($#) f x = fmap ($ x) f
-- ($#) f x = f $: pure x


{- |
Our signal processors have types like @f (a -> b -> c)@.
They could also have the type @a -> b -> f c@
or @f a -> f b -> f c@.
We did not choose the last variant for reduction of redundancy in type signatures
and for simplifying sharing,
and we did not choose the second variant for easy composition of processors.
However the forms are freely convertible,
and if you prefer the last one because you do not want to sprinkle '($:)' in your code,
then you may want to convert the processors using the following functions,
that can be defined purely in the 'Control.Applicative.Applicative' class.
-}

{-# INLINE liftP #-}
liftP :: (Applicative f) =>
   f (a -> b) -> f a -> f b
liftP = ($:)

{-# INLINE liftP2 #-}
liftP2 :: (Applicative f) =>
   f (a -> b -> c) -> f a -> f b -> f c
liftP2 f a b = f $: a $: b

{-# INLINE liftP3 #-}
liftP3 :: (Applicative f) =>
   f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftP3 f a b c = f $: a $: b $: c

{-# INLINE liftP4 #-}
liftP4 :: (Applicative f) =>
   f (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftP4 f a b c d = f $: a $: b $: c $: d
