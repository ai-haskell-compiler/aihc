module Data.TypeRig.Riggable where

import Control.Arrow
import Data.Either
import Data.Functor
import Data.Functor.Invariant
import Data.Kind
import Data.List.NonEmpty
import Data.Maybe
import Data.Semigroup
import Data.TypeRig.Productable
import Data.TypeRig.Summable
import Prelude hiding ((.), id)
import qualified Text.ParserCombinators.ReadP as ReadP
import qualified Text.ParserCombinators.ReadPrec as ReadPrec

-- | Composability via a [rig](https://ncatlab.org/nlab/show/rig) of types.
type Riggable :: (Type -> Type) -> Constraint
class (Productable f, Summable f) => Riggable f where
    rOptional :: forall a. f a -> f (Maybe a)
    rOptional fa = let
        eitherToMaybe :: Either a () -> Maybe a
        eitherToMaybe (Left a) = Just a
        eitherToMaybe (Right ()) = Nothing
        maybeToEither :: Maybe a -> Either a ()
        maybeToEither (Just a) = Left a
        maybeToEither Nothing = Right ()
        in invmap eitherToMaybe maybeToEither $ fa <+++> rUnit
    rList1 :: f a -> f (NonEmpty a)
    rList1 fa = let
        pairToNonEmpty :: (a, [a]) -> NonEmpty a
        pairToNonEmpty (a, as) = a :| as
        nonEmptyToPair :: NonEmpty a -> (a, [a])
        nonEmptyToPair (a :| as) = (a, as)
        in invmap pairToNonEmpty nonEmptyToPair $ fa <***> rList fa
    rList :: f a -> f [a]
    rList fa = let
        eitherToList :: Either (NonEmpty a) () -> [a]
        eitherToList (Left (a :| aa)) = a : aa
        eitherToList (Right ()) = []
        listToEither :: [a] -> Either (NonEmpty a) ()
        listToEither (a:aa) = Left $ a :| aa
        listToEither [] = Right ()
        in invmap eitherToList listToEither $ rList1 fa <+++> rUnit

instance Riggable Endo where
    rOptional (Endo f) = Endo $ fmap f
    rList1 (Endo f) = Endo $ fmap f
    rList (Endo f) = Endo $ fmap f

instance Riggable m => Riggable (Kleisli m a) where
    rOptional (Kleisli f) = Kleisli $ \a -> rOptional $ f a
    rList1 (Kleisli f) = Kleisli $ \a -> rList1 $ f a
    rList (Kleisli f) = Kleisli $ \a -> rList $ f a

instance Riggable ReadPrec.ReadPrec where
    rOptional ra = ReadPrec.readP_to_Prec $ \prec -> ReadP.option Nothing $ fmap Just $ ReadPrec.readPrec_to_P ra prec
    rList ra = ReadPrec.readP_to_Prec $ \prec -> ReadP.many $ ReadPrec.readPrec_to_P ra prec
