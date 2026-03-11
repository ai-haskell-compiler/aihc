{-# LANGUAGE AllowAmbiguousTypes #-} -- for predicate reification
{-# LANGUAGE UndecidableInstances #-} -- for KnownPredicateName in Arbitrary

module Rerefined.Refine
  (
  -- * @Refined@
    type Refined
  , refine
  , unrefine
  , reifyPredicate
  , unsafeRefine
  , unsafeRerefine

  -- * @Refined1@
  , type Refined1
  , refine1
  , unrefine1
  , reifyPredicate1
  , squashRefined1
  , unsafeRefine1
  , unsafeRerefine1

  -- * Errors
  , type RefineFailure
  , prettyRefineFailure
  , prettyRefineFailure'
  ) where

import Rerefined.Predicate
import Language.Haskell.TH.Syntax ( Lift )
import GHC.Exts ( proxy# )
import Data.Text ( Text )
import Data.Text.Builder.Linear qualified as TBL

import Test.QuickCheck ( Arbitrary(arbitrary) )
import Test.QuickCheck.Gen qualified as QC

-- | @a@ refined with predicate @p@.
newtype Refined p a = Refined a
    deriving stock (Lift, Eq, Ord, Show)

-- | Strip the refinement from a 'Refined'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined'@ output.
unrefine :: Refined p a -> a
unrefine (Refined a) = a

-- | @f a@ refined with predicate @p@.
--
-- We may derive legal 'Functor', 'Traversable' instances for this as
-- 'Rerefined.Predicate.Refine1' guarantees that the predicate only applies to
-- the functor structure. That is, you /may/ alter a 'Refined1' without
-- re-asserting its predicate, provided your changes are made without altering
-- the structure/shape of @f@ (e.g. 'fmap', 'traverse').
newtype Refined1 p f a = Refined1 (f a)
    deriving stock (Functor, Foldable, Traversable, Lift, Eq, Ord, Show)

-- | Strip the refinement from a 'Refined1'.
--
-- This is kept as a separate function for prettier @'Show' 'Refined1'@ output.
unrefine1 :: Refined1 p f a -> f a
unrefine1 (Refined1 fa) = fa

-- | Squash a 'Refined1' into a 'Refined'. Essentially forget the @f@.
squashRefined1 :: Refined1 p f a -> Refined p (f a)
squashRefined1 = Refined . unrefine1

-- | Refine @a@ with predicate @p@.
refine
    :: forall p a. Refine p a
    => a -> Either RefineFailure (Refined p a)
refine a =
    case validate (proxy# @p) a of
      Nothing -> Right (Refined a)
      Just e  -> Left e

-- | Construct a 'Refined' without validating the predicate @p@.
--
-- Unsafe. Use only when you can manually prove that the predicate holds.
unsafeRefine :: a -> Refined p a
unsafeRefine = Refined

-- | Replace a 'Refined''s predicate without validating the new predicate
--   @pNew@.
--
-- Unsafe. Use only when you can manually prove that the new predicate holds.
unsafeRerefine :: forall pNew pOld a. Refined pOld a -> Refined pNew a
unsafeRerefine = Refined . unrefine

-- | Reify a predicate.
reifyPredicate :: forall p a. Refine p a => a -> Bool
reifyPredicate a = case refine @p a of Right{} -> True; Left{} -> False

-- | Refine @f a@ with functor predicate @p@.
refine1
    :: forall p f a. Refine1 p f
    => f a -> Either RefineFailure (Refined1 p f a)
refine1 fa =
    case validate1 (proxy# @p) fa of
      Nothing -> Right (Refined1 fa)
      Just e  -> Left e

-- | Construct a 'Refined1' without validating the predicate @p@.
--
-- Unsafe. Use only when you can manually prove that the predicate holds.
unsafeRefine1 :: f a -> Refined1 p f a
unsafeRefine1 = Refined1

-- | Replace a 'Refined1''s predicate without validating the new predicate
--   @pNew@.
--
-- Unsafe. Use only when you can manually prove that the new predicate holds.
unsafeRerefine1 :: forall pNew pOld f a. Refined1 pOld f a -> Refined1 pNew f a
unsafeRerefine1 = Refined1 . unrefine1

-- | Reify a functor predicate.
reifyPredicate1 :: forall p f a. Refine1 p f => f a -> Bool
reifyPredicate1 fa = case refine1 @p fa of Right{} -> True; Left{} -> False

{- TODO
* got an extra \n at start oops
* make it look better lol
* make tail-call recursive? need to ferry more indents around though
-}
prettyRefineFailure :: RefineFailure -> Text
prettyRefineFailure = TBL.runBuilder . go (0 :: Int) . (\e -> [e])
  where
    go n = \case
      []     -> mempty
      (e:es) ->
        let bPred   = TBL.fromChar '\n' <> indent n <> refineFailurePredicate e
            bDetail = TBL.fromChar '\n' <> indent (n+2) <> refineFailureDetail e
         in bPred <> bDetail <> go (n+2) (refineFailureInner e) <> go n es
    indent = \case
      0 -> mempty
      n -> TBL.fromChar ' ' <> indent (n-1)

-- TODO this requires switching inner errors so that last is first lol x)
-- also we only <> right. maybe that's useful for perf
-- to remove newline at start we need to start in a special mode so we know not
-- to add the first newline at bPred. but I cba it will be messier and I want to
-- replace this ASAP when someone comes up with a good pretty error format
prettyRefineFailure' :: RefineFailure -> Text
prettyRefineFailure' = \e -> TBL.runBuilder $ go mempty [(0 :: Int, e)]
  where
    go b = \case
      []            -> b
      ((n, e) : es) ->
        let bPred   = TBL.fromChar '\n' <> indent n <> refineFailurePredicate e
            bDetail = TBL.fromChar '\n' <> indent (n+2) <> refineFailureDetail e
            b'      = b <> bPred <> bDetail
         in go b' (idk (n+2) es (refineFailureInner e))
    indent = \case
      0 -> mempty
      n -> TBL.fromChar ' ' <> indent (n-1)
    idk n rs = \case
      []   -> rs
      l:ls -> idk n ((n, l):rs) ls

-- | Generate a refined term by generating an unrefined term and asserting the
--   predicate.
--
-- Will runtime error if it fails to find a satisfying term (based on size).
instance (Arbitrary a, Refine p a, KnownPredicateName p)
  => Arbitrary (Refined p a) where
    arbitrary = QC.suchThatMaybe arbitrary (reifyPredicate @p) >>= \case
      Just a  -> pure $ Refined a
      Nothing -> error $
           "rerefined: couldn't generate a value satisfying predicate: "
        <> predicateName @p

-- | Generate a refined term by generating an unrefined term and asserting the
--   functor predicate.
--
-- Will runtime error if it fails to find a satisfying term (based on size).
instance (Arbitrary (f a), Refine1 p f, KnownPredicateName p)
  => Arbitrary (Refined1 p f a) where
    arbitrary = QC.suchThatMaybe arbitrary (reifyPredicate1 @p) >>= \case
      Just fa -> pure $ Refined1 fa
      Nothing -> error $
           "rerefined: couldn't generate a value satisfying predicate: "
        <> predicateName @p
