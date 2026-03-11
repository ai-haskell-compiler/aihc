module Lens.Family.State.Zoom where

import Control.Monad (liftM)

newtype Zooming m c a = Zooming { unZooming :: m (c, a) }

instance Monad m => Functor (Zooming m c) where
  fmap f (Zooming m) = Zooming (liftM (fmap f) m)

instance (Monoid c, Monad m) => Applicative (Zooming m c) where
  pure a = Zooming (return (mempty, a))
  Zooming f <*> Zooming x = Zooming $ do
    (a, f') <- f
    (b, x') <- x
    return (a <> b, f' x')
