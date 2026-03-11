module Data.Type.Witness.Specific.FixedList where

import Data.PeanoNat
import Data.Type.Witness.General.Representative
import Data.Type.Witness.Specific.PeanoNat
import Data.Type.Witness.Specific.Some
import Import

type FixedList :: PeanoNat -> Type -> Type
type role FixedList nominal representational
data FixedList n a where
    NilFixedList :: FixedList 'Zero a
    ConsFixedList :: a -> FixedList n a -> FixedList ('Succ n) a

instance Functor (FixedList n) where
    fmap _ NilFixedList = NilFixedList
    fmap ab (ConsFixedList a l) = ConsFixedList (ab a) $ fmap ab l

instance Is PeanoNatType n => Applicative (FixedList n) where
    pure = let
        pure' :: PeanoNatType n' -> a -> FixedList n' a
        pure' ZeroType _ = NilFixedList
        pure' (SuccType n) a = ConsFixedList a $ pure' n a
        in pure' representative
    (<*>) = let
        ap' :: PeanoNatType n' -> FixedList n' (a -> b) -> FixedList n' a -> FixedList n' b
        ap' ZeroType NilFixedList NilFixedList = NilFixedList
        ap' (SuccType n) (ConsFixedList ab lab) (ConsFixedList a la) = ConsFixedList (ab a) $ ap' n lab la
        in ap' representative

instance Foldable (FixedList n) where
    foldMap _ NilFixedList = mempty
    foldMap am (ConsFixedList a l) = am a <> foldMap am l

instance Traversable (FixedList n) where
    sequenceA NilFixedList = pure NilFixedList
    sequenceA (ConsFixedList fa l) = liftA2 ConsFixedList fa $ sequenceA l

instance Eq a => Eq (FixedList n a) where
    NilFixedList == NilFixedList = True
    (ConsFixedList a la) == (ConsFixedList b lb) =
        if a == b
            then la == lb
            else False

fixedListLength :: FixedList n a -> PeanoNatType n
fixedListLength NilFixedList = ZeroType
fixedListLength (ConsFixedList _ l) = SuccType $ fixedListLength l

fixedListGenerate :: Applicative m => PeanoNatType n -> m a -> m (FixedList n a)
fixedListGenerate ZeroType _ = pure NilFixedList
fixedListGenerate (SuccType n) ma = liftA2 ConsFixedList ma $ fixedListGenerate n ma

fixedFromList :: [a] -> (forall n. PeanoNatType n -> FixedList n a -> r) -> r
fixedFromList [] call = call ZeroType NilFixedList
fixedFromList (a:aa) call = fixedFromList aa $ \n l -> call (SuccType n) $ ConsFixedList a l

fixedListArrowSequence_ ::
       forall arrow n a. Arrow arrow
    => FixedList n (arrow a ())
    -> arrow (FixedList n a) ()
fixedListArrowSequence_ NilFixedList = arr $ \_ -> ()
fixedListArrowSequence_ (ConsFixedList x1 xr) =
    proc a1r -> do
        x1 -<
            case a1r of
                ConsFixedList a1 _ -> a1
        fixedListArrowSequence_ xr -<
            case a1r of
                ConsFixedList _ ar -> ar

fixedListArrowSequence ::
       forall arrow n a b. Arrow arrow
    => FixedList n (arrow a b)
    -> arrow (FixedList n a) (FixedList n b)
fixedListArrowSequence NilFixedList = arr $ \_ -> NilFixedList
fixedListArrowSequence (ConsFixedList x1 xr) =
    proc a1r -> do
        b1 <-
            x1 -<
                case a1r of
                    ConsFixedList a1 _ -> a1
        br <-
            fixedListArrowSequence xr -<
                case a1r of
                    ConsFixedList _ ar -> ar
        returnA -< ConsFixedList b1 br

fixedListElement :: Some (Greater n) -> FixedList n a -> a
fixedListElement (MkSome (MkGreater ZeroGreaterEqual)) (ConsFixedList a _) = a
fixedListElement (MkSome (MkGreater (SuccGreaterEqual n))) (ConsFixedList _ l) =
    fixedListElement (MkSome $ MkGreater n) l
