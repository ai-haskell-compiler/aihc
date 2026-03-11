{-# LANGUAGE EmptyCase, UndecidableSuperClasses #-}
module Control.Subcategory.Applicative.Class  (CApplicative(..)) where
import Control.Subcategory.Functor

import qualified Control.Applicative as App

infixl 4 <.>
class CFunctor f => CApplicative f where
  pair :: (Dom f a, Dom f b, Dom f (a, b)) => f a -> f b -> f (a, b)
  default pair :: (Applicative f) => f a -> f b -> f (a, b)
  pair = App.liftA2 (,)
  (<.>) :: (Dom f a, Dom f b, Dom f (a -> b)) => f (a -> b) -> f a -> f b
  default (<.>) :: (Applicative f) => f (a -> b) -> f a -> f b
  (<.>) = (<*>)
  (.>) :: (Dom f a, Dom f b) => f a -> f b -> f b
  default (.>) :: Applicative f
               => f a -> f b -> f b
  (.>) = (*>)
  (<.) :: (Dom f a, Dom f b) => f a -> f b -> f a
  default (<.) :: Applicative f
               => f a -> f b -> f a
  (<.) = (<*)
