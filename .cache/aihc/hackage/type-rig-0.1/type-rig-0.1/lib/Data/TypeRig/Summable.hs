module Data.TypeRig.Summable where

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Either
import Data.Functor
import Data.Functor.Invariant
import Data.Kind
import Data.Semigroup
import Data.Void
import Prelude hiding ((.), id)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

infixr 2 <+++>

-- | Composability via type sum 'Either' and empty type 'Void'.
type Summable :: (Type -> Type) -> Constraint
class Invariant f => Summable f where
    rVoid :: f Void
    default rVoid :: Alternative f => f Void
    rVoid = empty
    (<+++>) :: f a -> f b -> f (Either a b)
    default (<+++>) :: Alternative f => f a -> f b -> f (Either a b)
    fa <+++> fb = (fmap Left fa) <|> (fmap Right fb)

instance Summable Endo where
    rVoid = Endo id
    Endo p <+++> Endo q =
        Endo $ \case
            Left a -> Left $ p a
            Right b -> Right $ q b

instance Summable m => Summable (Kleisli m a) where
    rVoid = Kleisli $ \_ -> rVoid
    Kleisli p <+++> Kleisli q = Kleisli $ \a -> p a <+++> q a

instance Summable ReadPrec.ReadPrec where
    ra <+++> rb = fmap Left ra ReadPrec.<++ fmap Right rb
