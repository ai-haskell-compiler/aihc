module Data.TypeRig.Productable where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Functor.Invariant
import Data.Kind
import Data.Semigroup
import Prelude hiding ((.), id)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

infixr 3 <***>, ***>, <***

-- | Composability via type product '(,)' and unit type '()'.
type Productable :: (Type -> Type) -> Constraint
class Invariant f => Productable f where
    rUnit :: f ()
    default rUnit :: Applicative f => f ()
    rUnit = pure ()
    (<***>) :: f a -> f b -> f (a, b)
    default (<***>) :: Applicative f => f a -> f b -> f (a, b)
    (<***>) = liftA2 (,)
    (***>) :: f () -> f a -> f a
    fu ***> fa = invmap (\((), a) -> a) (\a -> ((), a)) $ fu <***> fa
    (<***) :: f a -> f () -> f a
    fa <*** fu = invmap (\(a, ()) -> a) (\a -> (a, ())) $ fa <***> fu

instance Productable Endo where
    rUnit = Endo id
    Endo p <***> Endo q = Endo $ \(a, b) -> (p a, q b)

instance Productable m => Productable (Kleisli m a) where
    rUnit = Kleisli $ \_ -> rUnit
    Kleisli p <***> Kleisli q = Kleisli $ \a -> p a <***> q a

instance Productable ReadPrec.ReadPrec
