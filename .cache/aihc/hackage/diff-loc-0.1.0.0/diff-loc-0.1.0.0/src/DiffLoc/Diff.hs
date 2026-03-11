{-# LANGUAGE
  AllowAmbiguousTypes,
  DerivingStrategies,
  FlexibleContexts,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  MultiParamTypeClasses,
  ScopedTypeVariables,
  StandaloneDeriving,
  TypeApplications,
  TypeFamilies,
  UndecidableInstances #-}

-- | Mapping intervals across diffs
module DiffLoc.Diff
  ( -- * Types
    ADiff()

    -- * Operations
  , emptyDiff
  , addDiff
  , mapDiff
  , comapDiff
  , listToDiff
  ) where

import Data.Coerce
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.FingerTree (FingerTree)
import Text.Show.Combinators (showCon, (@|))
import qualified Data.FingerTree as FT

import DiffLoc.Shift

-- $setup
-- >>> import Control.Monad ((<=<))
-- >>> import Test.QuickCheck
-- >>> import Test.QuickCheck.HigherOrder
-- >>> import DiffLoc
-- >>> import DiffLoc.Unsafe
-- >>> import DiffLoc.Test
-- >>> type NN' = Colline N N'
-- >>> quickCheck = quickCheckWith' stdArgs{maxSuccess=3000}

-- | A diff represents a transformation from one file to another.
--
-- Example diff between "abcdefgh" and "appcfgzzh":
--
-- > source ab cdefg  h
-- >      -  b  de
-- >      +  pp     zz
-- > target appc  fgzzh
--
-- It consists of three replacements:
--
-- 1. replace "b" with "pp" at location 1, @mkReplace 1 1 2@;
-- 2. replace "de" with "" at location 3, @mkReplace 3 2 0@;
-- 3. replace "" with "zz" at location 7, @mkReplace 7 0 2@.
--
-- >>> :{
--   let d :: Diff N
--       d = addDiff (Replace 1 (offset 1) (offset 2))  -- at location 1, replace "b" (length 1) with "pp" (length 2)
--         $ addDiff (Replace 3 (offset 2) (offset 0))  -- at location 3, replace "de" with ""
--         $ addDiff (Replace 7 (offset 0) (offset 2))  -- at location 7, replace "" with "zz"
--         $ emptyDiff
--   -- N.B.: replacements should be inserted right to left.
-- :}
--
-- 'ADiff' is an abstract representation to be instantiated with
-- a concrete representation of atomic replacements.
--
-- == __Internal details__
--
-- Internally, a diff is a sequence of /disjoint/ and /nonempty/ replacements,
-- /ordered/ by their source locations.
-- The monoid annotation in the fingertree gives the endpoints of the replacements.
newtype ADiff r = ADiff (FingerTree (Maybe r) (R r))
  deriving Eq

instance Show r => Show (ADiff r) where
  showsPrec = flip $ \d -> showCon "listToDiff" @| diffToList d

-- | The empty diff.
emptyDiff :: Semigroup r => ADiff r
emptyDiff = ADiff FT.empty

-- | A newtype to carry a 'FT.Measured' instance.
newtype R r = R r
  deriving newtype (Eq, Show)

instance Semigroup r => FT.Measured (Maybe r) (R r) where
  measure (R r) = Just r

coshiftR' :: Shift r => Maybe r -> r -> r
coshiftR' Nothing = id
coshiftR' (Just r) = fromMaybe (error "failed to shift disjoint intervals") . coshiftR r

addDiffL :: forall r. Shift r => r -> ADiff r -> ADiff r
addDiffL r (ADiff d0) = case FT.viewl d0 of
  FT.EmptyL -> ADiff (FT.singleton (R r))
  R s FT.:< d | src r `distantlyPrecedes` src s -> ADiff (R r FT.<| d0)
              | otherwise -> addDiffL (r <> s) (ADiff d)

-- | Add a replacement to a diff. The replacement is performed /after/ the diff.
--
-- === Properties
--
-- prop> not (isZeroLength x) ==> mapDiff (addDiff r d) x == (shiftBlock r <=< mapDiff (d :: Diff N)) x
-- prop> not (isZeroLength x) ==> comapDiff (addDiff r d) x == (comapDiff d <=< coshiftBlock (r :: Replace N)) x
addDiff :: forall r. Shift r => r -> ADiff r -> ADiff r
addDiff r (ADiff d) = case FT.search (\r1 _-> r1 `notPrecedes_` r) d of
  FT.Position d1 s d2 -> coerce (d1 <>) (addDiffL (coshiftR' (FT.measure d1) r) (ADiff (s FT.<| d2)))
  FT.OnLeft -> addDiffL r (ADiff d)
  FT.OnRight -> ADiff (d FT.|> R (coshiftR' (FT.measure d) r))
  FT.Nowhere -> error "Broken invariant"
  where
    notPrecedes_ Nothing _ = False
    notPrecedes_ (Just r1) i = not (tgt r1 `distantlyPrecedes` tgt i)
    -- Using distantlyPrecedes here and in addDiffL lets us merge adjacent intervals.

-- $hidden
-- prop> not (isZeroLength x) ==> mapDiff (addDiff r d) x == (shiftBlock r <=< mapDiff (d :: Diff NN')) x
-- prop> not (isZeroLength x) ==> comapDiff (addDiff r d) x == (comapDiff d <=< coshiftBlock (r :: Replace NN')) x

-- | Translate a span in the source of a diff to a span in its target.
-- @Nothing@ if the span overlaps with a replacement.
--
-- For exaple, given the following 'ADiff' (or 'DiffLoc.Interval.Replace') from "aAacCc" to "aAabbbcCc":
--
-- > source aAa   cCc
-- >      - 
-- >      +    bbb
-- > target aAabbbcCc
--
-- >>> r0 = Replace 3 (offset 0) (offset 3) :: Replace N
-- >>> d0 = addDiff r0 emptyDiff
--
-- The span of \"A\" remains unchanged.
--
-- >>> mapDiff d0 (1 :.. offset 1)
-- Just (1 :.. offset 1)
-- >>> shiftBlock r0 (1 :.. offset 1)
-- Just (1 :.. offset 1)
-- >>> comapDiff d0 (1 :.. offset 1)
-- Just (1 :.. offset 1)
-- >>> coshiftBlock r0 (1 :.. offset 1)
-- Just (1 :.. offset 1)
--
-- The span of \"C\" is shifted by 3 characters.
--
-- >>> mapDiff d0 (4 :.. offset 1)
-- Just (7 :.. offset 1)
-- >>> shiftBlock r0 (4 :.. offset 1)
-- Just (7 :.. offset 1)
-- >>> comapDiff d0 (7 :.. offset 1)
-- Just (4 :.. offset 1)
-- >>> coshiftBlock r0 (7 :.. offset 1)
-- Just (4 :.. offset 1)
--
-- The span of "ac" overlaps with the replacement, so the mapping is undefined.
--
-- >>> mapDiff d0 (2 :.. offset 2)
-- Nothing
-- >>> shiftBlock r0 (2 :.. offset 2)
-- Nothing
-- >>> comapDiff d0 (2 :.. offset 5)
-- Nothing
-- >>> coshiftBlock r0 (2 :.. offset 5)
-- Nothing
--
-- === Properties
--
-- prop> \(FSN d s) -> not (isZeroLength s) ==> partialSemiInverse (mapDiff d) (comapDiff d) s
-- prop> \(FSN d s) -> not (isZeroLength s) ==> partialSemiInverse (comapDiff d) (mapDiff d) s
--
-- where @partialSemiInverse f g x@ is the property
--
-- > if   f x == Just y   -- for some y
-- > then g y == Just x
mapDiff :: Shift r => ADiff r -> Block r -> Maybe (Block r)
mapDiff = mapDiff_ Cov

-- $hidden
--
-- prop> \(FSV d s) -> not (isZeroLength s) ==> partialSemiInverse (mapDiff d) (comapDiff d) s
-- prop> \(FSV d s) -> not (isZeroLength s) ==> partialSemiInverse (comapDiff d) (mapDiff d) s

-- | Translate a span in the target of a diff to a span in its source.
-- @Nothing@ if the span overlaps with a replacement.
--
-- See also 'mapDiff'.
comapDiff :: Shift r => ADiff r -> Block r -> Maybe (Block r)
comapDiff = mapDiff_ Contrav

data Variance = Cov | Contrav

srcV :: Shift r => Variance -> r -> Block r
srcV Cov = src
srcV Contrav = tgt

shiftBlockV' :: Shift r => Variance -> Maybe r -> Block r -> Block r
shiftBlockV' _ Nothing = id
shiftBlockV' Cov (Just r) = fromMaybe (error "failed to shift disjoint intervals") . shiftBlock r
shiftBlockV' Contrav (Just r) = fromMaybe (error "failed to shift disjoint intervals") . coshiftBlock r

mapDiff_ :: forall r. Shift r => Variance -> ADiff r -> Block r -> Maybe (Block r)
mapDiff_ v (ADiff d) i = case FT.search (\r1 _ -> r1 `notPrecedes_` i) d of
  FT.Position d1 (R s) _
    | j `precedes` (srcV v s) -> Just i'
    | otherwise -> Nothing
    where i' = shiftBlockV' v (FT.measure d1) i
          j = case v of Cov -> i ; Contrav -> i'
  FT.OnLeft -> Just i
  FT.OnRight -> Just (shiftBlockV' v (FT.measure d) i)
  FT.Nowhere -> error "Broken invariant"
  where
    notPrecedes_ Nothing _ = False
    notPrecedes_ (Just r1) i1 = not (srcV v r1 `precedes` i1)

-- |
--
-- @
-- 'listToDiff' = foldr 'addDiff' 'emptyDiff'
-- @
listToDiff :: Shift r => [r] -> ADiff r
listToDiff = foldr addDiff emptyDiff

diffToList :: ADiff r -> [r]
diffToList (ADiff d) = coerce (toList d)
