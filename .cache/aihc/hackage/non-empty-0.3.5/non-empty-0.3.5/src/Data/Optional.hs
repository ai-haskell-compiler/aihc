module Data.Optional (
   T(Nil, Cons),
   (?:),
   fromEmpty,
   fromNonEmpty,
   ) where

import qualified Data.NonEmpty.Class as C
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.NonEmptyPrivate (Aux(Aux), snoc)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import Control.Applicative (pure, liftA2, )
import Control.DeepSeq (NFData, rnf, )

import qualified Test.QuickCheck as QC

import Control.Monad (return, )
import Data.Functor (fmap, )
import Data.Function (($), (.), )
import Data.Ord (Ord, Ordering(GT), (>), )
import qualified Prelude as P
import Prelude (Eq, uncurry, )


data T f a = Nil | Cons a (f a)
   deriving (Eq, Ord)

fromEmpty :: Empty.T a -> T f a
fromEmpty Empty.Cons = Nil

fromNonEmpty :: NonEmpty.T f a -> T f a
fromNonEmpty (NonEmpty.Cons x xs) = Cons x xs


instance (C.NFData f, NFData a) => NFData (T f a) where
   rnf = C.rnf

instance (C.NFData f) => C.NFData (T f) where
   rnf Nil = ()
   rnf (Cons x xs) = rnf (x, C.rnf xs)


instance (C.Show f, P.Show a) => P.Show (T f a) where
   showsPrec = C.showsPrec

instance (C.Show f) => C.Show (T f) where
   showsPrec _ Nil = P.showString "Nil"
   showsPrec p (Cons x xs) =
      P.showParen (p>5) $
      P.showsPrec 6 x . P.showString "?:" . C.showsPrec 5 xs


infixr 5 ?:

(?:) :: a -> f a -> T f a
(?:) = Cons


instance P.Functor f => P.Functor (T f) where
   fmap _ Nil = Nil
   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance (Fold.Foldable f) => Fold.Foldable (T f) where
   foldr _ y Nil = y
   foldr f y (Cons x xs) = f x (Fold.foldr f y xs)

instance (Trav.Traversable f) => Trav.Traversable (T f) where
   sequenceA Nil = pure Nil
   sequenceA (Cons x xs) = liftA2 Cons x (Trav.sequenceA xs)


instance (C.Arbitrary f, QC.Arbitrary a) => QC.Arbitrary (T f a) where
   arbitrary = arbitrary
   shrink = shrink

instance (C.Arbitrary f) => C.Arbitrary (T f) where
   arbitrary = arbitrary
   shrink = shrink

arbitrary :: (C.Arbitrary f, QC.Arbitrary a) => QC.Gen (T f a)
arbitrary = QC.oneof [return Nil, liftA2 Cons QC.arbitrary C.arbitrary]

shrink :: (C.Arbitrary f, QC.Arbitrary a) => T f a -> [T f a]
shrink Nil = []
shrink (Cons x xs) = P.map (\(y, Aux ys) -> Cons y ys) (QC.shrink (x, Aux xs))

instance (C.Gen f) => C.Gen (T f) where
   genOf gen = do
      b <- QC.arbitrary
      if b then liftA2 Cons gen $ C.genOf gen else return Nil


instance C.Empty (T f) where
   empty = Nil

instance (C.Cons f, C.Empty f) => C.Cons (T f) where
   cons x Nil = Cons x C.empty
   cons x0 (Cons x1 xs) = Cons x0 $ C.cons x1 xs

instance (C.Repeat f) => C.Repeat (T f) where
   repeat x = Cons x $ C.repeat x

instance (C.Iterate f) => C.Iterate (T f) where
   iterate f x = Cons x $ C.iterate f (f x)

instance C.Zip f => C.Zip (T f) where
   zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (C.zipWith f xs ys)
   zipWith _ _ _ = Nil

instance (Trav.Traversable f, C.Reverse f) => C.Reverse (T f) where
   reverse Nil = Nil
   reverse (Cons x xs) =
      fromNonEmpty (snoc (C.reverse xs) x)

instance (NonEmpty.Insert f, C.Sort f) => C.Sort (T f) where
   sort Nil = Nil
   sort (Cons x xs) =
      fromNonEmpty $ NonEmpty.insert x $ C.sort xs

instance (NonEmpty.InsertBy f, C.SortBy f) => C.SortBy (T f) where
   sortBy _ Nil = Nil
   sortBy f (Cons x xs) =
      fromNonEmpty $ NonEmpty.insertBy f x $ C.sortBy f xs

instance (NonEmpty.Insert f) => NonEmpty.Insert (T f) where
   insert y xt =
      uncurry NonEmpty.Cons $
      case xt of
         Nil -> (y, xt)
         Cons x xs ->
            case P.compare y x of
               GT -> (x, fromNonEmpty $ NonEmpty.insert y xs)
               _ -> (y, xt)

instance (NonEmpty.InsertBy f) => NonEmpty.InsertBy (T f) where
   insertBy f y xt =
      uncurry NonEmpty.Cons $
      case xt of
         Nil -> (y, xt)
         Cons x xs ->
            case f y x of
               GT -> (x, fromNonEmpty $ NonEmpty.insertBy f y xs)
               _ -> (y, xt)
