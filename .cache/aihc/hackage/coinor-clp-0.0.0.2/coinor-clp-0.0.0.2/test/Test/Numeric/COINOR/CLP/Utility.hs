module Test.Numeric.COINOR.CLP.Utility where

import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, traverse_)

import qualified Control.Monad.Trans.State as MS

import Data.Tuple.HT (double)


traverse_Lag ::
   (Foldable t, Monad m) =>
   b -> (b -> a -> m b) -> t a -> m ()
traverse_Lag b0 f =
   flip MS.evalStateT b0 .
   traverse_ (\a -> MS.StateT $ \b -> fmap double $ f b a)

traverseLag ::
   (Traversable t, Monad m) =>
   b -> (b -> a -> m b) -> t a -> m (t b)
traverseLag b0 f =
   flip MS.evalStateT b0 .
   traverse (\a -> MS.StateT $ \b -> fmap double $ f b a)
