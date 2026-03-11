{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Type.List.Sublist
-- Copyright   : (c) Justin Le 2023
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Witnesses regarding sublists of lists.
module Data.Type.List.Sublist (
  -- * Prefix and Suffix

  -- ** Prefix
  Prefix (..),
  IsPrefix,
  autoPrefix,
  takeRec,
  prefixLens,
  takeIndex,
  weakenIndex,
  prefixShape,

  -- ** Suffix
  Suffix (..),
  IsSuffix,
  autoSuffix,
  dropRec,
  suffixLens,
  dropIndex,
  shiftIndex,

  -- * Append
  Append (..),
  IsAppend,
  autoAppend,
  withAppend,
  prefixToAppend,
  suffixToAppend,
  appendToPrefix,
  appendToSuffix,
  splitAppend,
  appendShape,

  -- ** Application
  splitRec,
  appendRec,
  splitRecIso,
  splitIndex,

  -- ** Witnesses

  -- *** Singletons
  pattern AppendWit,
  appendWit,
  implyAppend,
  unAppendWit,

  -- *** Vinyl
  pattern AppendWitV,
  appendWitV,
  implyAppendV,
  unAppendWitV,

  -- *** Both
  pattern AppendWit',
  convertAppends,
  AppendedTo,

  -- * Interleave
  Interleave (..),
  IsInterleave,
  autoInterleave,
  interleaveRec,
  unweaveRec,
  interleaveRecIso,
  injectIndexL,
  injectIndexR,
  unweaveIndex,
  interleavedIxes,
  swapInterleave,
  interleaveShapes,

  -- * Subset
  Subset (..),
  IsSubset,
  autoSubset,
  subsetComplement,
  interleaveRToSubset,
  interleaveLToSubset,
  subsetToInterleaveL,
  subsetToInterleaveR,
  subsetRec,
  getSubset,
  subsetShapes,
  subsetIxes,
  weakenSubsetIndex,
  strengthenSubsetIndex,
) where

import Data.Bifunctor
import Data.Functor.Compose
import Data.Kind
import Data.List.Singletons (SList (..), type (++))
import Data.Profunctor
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Sigma
import Data.Type.Functor.Product
import Data.Type.Predicate
import Data.Type.Predicate.Auto
import Data.Type.Predicate.Param
import Data.Vinyl hiding ((:~:))
import qualified Data.Vinyl.Recursive as VR
import qualified Data.Vinyl.TypeLevel as V
import GHC.Generics ((:+:) (..))
import Lens.Micro hiding ((%~))
import Lens.Micro.Extras

-- | A @'Prefix' as bs@ witnesses that @as@ is a prefix of @bs@.
--
-- Some examples:
--
-- @
-- PreZ                    :: Prefix '[]      '[1,2,3]
-- PreS PreZ               :: Prefix '[1]     '[1,2,3]
-- PreS (PreS PreZ)        :: Prefix '[1,2]   '[1,2,3]
-- PreS (PreS (PreS PreZ)) :: Prefix '[1,2,3] '[1,2,3]
-- @
--
-- Rule of thumb for construction: the number of 'PreS' is the number of
-- items in the prefix.
--
-- This is essentially the first half of an 'Append', but is conceptually
-- easier to work with.
data Prefix :: [k] -> [k] -> Type where
  PreZ :: Prefix '[] as
  PreS :: Prefix as bs -> Prefix (a ': as) (a ': bs)

deriving instance Show (Prefix as bs)

-- | A lens into the prefix of a 'Rec'.
prefixLens :: Prefix as bs -> Lens' (Rec f bs) (Rec f as)
prefixLens p = prefixToAppend p $ \a -> splitRecIso a . _1

-- | Take items from a 'Rec' corresponding to a given 'Prefix'.
takeRec :: Prefix as bs -> Rec f bs -> Rec f as
takeRec p = view (prefixLens p)

-- | A type-level predicate that a given list has @as@ as a prefix.
--
-- @since 0.1.2.0
type IsPrefix as = TyPred (Prefix as)

instance Auto (IsPrefix '[]) bs where
  auto = PreZ

instance Auto (IsPrefix as) bs => Auto (IsPrefix (a ': as)) (a ': bs) where
  auto = PreS (auto @_ @(IsPrefix as) @bs)

instance (SDecide k, SingI (as :: [k])) => Decidable (IsPrefix as) where
  decide = case sing @as of
    SNil -> \_ -> Proved PreZ
    x `SCons` (Sing :: Sing as') -> \case
      SNil -> Disproved $ \case {}
      y `SCons` (ys :: Sing bs') -> case x %~ y of
        Proved Refl -> case decide @(IsPrefix as') ys of
          Proved p -> Proved (PreS p)
          Disproved v -> Disproved $ \case
            PreS p -> v p
        Disproved v -> Disproved $ \case
          PreS _ -> v Refl

-- | Automatically generate a 'Prefix' if @as@ and @bs@ are known
-- statically.
--
-- @since 0.1.2.0
autoPrefix :: forall as bs. Auto (IsPrefix as) bs => Prefix as bs
autoPrefix = auto @_ @(IsPrefix as) @bs

-- | Get the 'Shape' associated with a 'Prefix'.
--
-- @since 0.1.3.0
prefixShape ::
  Prefix as bs ->
  Shape [] as
prefixShape = \case
  PreZ -> RNil
  PreS p -> Proxy :& prefixShape p

-- | A @'Suffix' as bs@ witnesses that @as@ is a suffix of @bs@.
--
-- Some examples:
--
-- @
-- SufZ                    :: Suffix '[1,2,3] '[1,2,3]
-- SufS SufZ               :: Suffix   '[2,3] '[1,2,3]
-- SufS (SufS SufZ)        :: Suffix     '[3] '[1,2,3]
-- SufS (SufS (SufS SufZ)) :: Suffix      '[] '[1,2,3]
-- @
--
-- Rule of thumb for construction: the number of 'SufS' is the number of
-- items to "drop" before getting the suffix.
--
-- This is essentially the second half of an 'Append', but is conceptually
-- easier to work with.
data Suffix :: [k] -> [k] -> Type where
  SufZ :: Suffix as as
  SufS :: Suffix as bs -> Suffix as (b ': bs)

deriving instance Show (Suffix as bs)

-- | A type-level predicate that a given list has @as@ as a suffix.
--
-- @since 0.1.2.0
type IsSuffix as = TyPred (Suffix as)

instance Auto (IsSuffix '[]) '[] where
  auto = SufZ

instance {-# OVERLAPPABLE #-} Auto (IsSuffix (a ': as)) (a ': as) where
  auto = SufZ

instance {-# OVERLAPPABLE #-} Auto (IsSuffix as) bs => Auto (IsSuffix as) (b ': bs) where
  auto = SufS (auto @_ @(IsSuffix as) @bs)

instance (SDecide k, SingI (as :: [k])) => Decidable (IsSuffix as) where
  decide = \case
    SNil -> case sing @as of
      SNil -> Proved SufZ
      _ `SCons` _ -> Disproved $ \case {}
    _ `SCons` ys -> case decide @(IsSuffix as) ys of
      Proved s -> Proved $ SufS s
      Disproved v -> Disproved $ \case
        SufZ -> error "help me"
        SufS s -> v s

-- | Automatically generate a 'Suffix' if @as@ and @bs@ are known
-- statically.
--
-- @since 0.1.2.0
autoSuffix :: forall as bs. Auto (IsSuffix as) bs => Suffix as bs
autoSuffix = auto @_ @(IsSuffix as) @bs

-- | A lens into the suffix of a 'Rec'.
suffixLens :: Suffix as bs -> Lens' (Rec f bs) (Rec f as)
suffixLens p = suffixToAppend p $ \a -> splitRecIso a . _2

-- | Drop items from a 'Rec' corresponding to a given 'Suffix'.
dropRec :: Suffix as bs -> Rec f bs -> Rec f as
dropRec p = view (suffixLens p)

-- | An @'Append' as bs cs@ witnesses that @cs@ is the result of appending
-- @as@ and @bs@.
--
-- Some examples:
--
-- @
-- AppZ                     :: Append '[]  '[1,2]   '[1,2]
-- AppZ                     :: Append '[]  '[1,2,3] '[1,2,3]
-- AppS AppZ                :: Append '[0] '[1,2]   '[0,1,2]
-- @
--
-- Rule of thumb for construction: the number of 'AppS' is the number of
-- items in the /first/ list.
--
-- This basically combines 'Prefix' and 'Suffix'.
data Append :: [k] -> [k] -> [k] -> Type where
  AppZ :: Append '[] as as
  AppS :: Append as bs cs -> Append (a ': as) bs (a ': cs)

deriving instance Show (Append as bs cs)

-- | A type-level predicate that a given list is the result of appending of
-- @as@ and @bs@.
--
-- @since 0.1.2.0
type IsAppend as bs = TyPred (Append as bs)

-- | A parameterized predicate that you can use with 'select': With an
-- @'AppendedTo' as@, you can give @bs@ and get @cs@ in return, where @cs@
-- is the appending of @as@ and @bs@.
--
-- Run it with:
--
-- @
-- 'selectTC' :: SingI as => Sing bs -> 'Σ' [k] ('IsAppend' as bs)
-- @
--
-- 'select' for 'AppendedTo' is pretty much just 'withAppend'.
--
-- @since 0.1.2.0
type AppendedTo as = TyPP (Append as)

instance Auto (IsAppend '[] as) as where
  auto = AppZ

instance Auto (IsAppend as bs) cs => Auto (IsAppend (a ': as) bs) (a ': cs) where
  auto = AppS (auto @_ @(IsAppend as bs) @cs)

instance (SDecide k, SingI (as :: [k]), SingI bs) => Decidable (IsAppend as bs) where
  decide = case sing @as of
    SNil -> \cs -> case sing @bs %~ cs of
      Proved Refl -> Proved AppZ
      Disproved v -> Disproved $ \case
        AppZ -> v Refl
    x `SCons` (Sing :: Sing as') -> \case
      SNil -> Disproved $ \case {}
      y `SCons` (ys :: Sing bs') -> case x %~ y of
        Proved Refl -> case decide @(IsAppend as' bs) ys of
          Proved p -> Proved (AppS p)
          Disproved v -> Disproved $ \case
            AppS p -> v p
        Disproved v -> Disproved $ \case
          AppS _ -> v Refl

instance SingI as => Decidable (Found (AppendedTo as))
instance SingI as => Provable (Found (AppendedTo as)) where
  prove ys = withAppend (singProd (sing @as)) (singProd ys) $ \s x -> prodSing s :&: x

-- | Automatically generate an 'Append' if @as@, @bs@ and @cs@ are known
-- statically.
--
-- @since 0.1.2.0
autoAppend :: forall as bs cs. Auto (IsAppend as bs) cs => Append as bs cs
autoAppend = auto @_ @(IsAppend as bs) @cs

-- | Witness that @'Append' as bs cs@ implies @(as ++ bs) ~ cs@, using
-- @++@ from "Data.Singletons.Prelude.List".
--
-- @since 0.1.2.0
appendWit :: Append as bs cs -> (as ++ bs) :~: cs
appendWit = \case
  AppZ -> Refl
  AppS a -> case appendWit a of
    Refl -> Refl

-- | The inverse of 'appendWit': if we know @(as ++ bs) ~ cs@ (using @++@
-- from "Data.Singletons.Prelude.List"), we can create an @'Append' as bs
-- cs@ given structure witnesses 'Sing'.
--
-- @since 0.1.2.0
unAppendWit ::
  (as ++ bs) ~ cs =>
  Rec f as ->
  Rec f bs ->
  Append as bs cs
unAppendWit = \case
  RNil -> \_ -> AppZ
  _ :& xs -> AppS . unAppendWit xs

-- | A useful pattern synonym for using 'Append' with @++@ from
-- "Data.Singletons.Prelude.List".
--
-- As a /pattern/, this brings @(as ++ bs) ~ cs@ into the context whenever
-- you use it to match on an @'Append' as bs cs@.
--
-- As an /expression/, this constructs an @'Append' as bs cs@ as long as
-- you have @(as ++ bs) ~ cs@ in the context.
--
-- @since 0.1.2.0
pattern AppendWit ::
  forall as bs cs. (RecApplicative as, RecApplicative bs) => (as ++ bs) ~ cs => Append as bs cs
pattern AppendWit <- (appendWit @as @bs @cs -> Refl)
  where
    AppendWit = unAppendWit @as @bs @cs pureShape pureShape

{-# COMPLETE AppendWit #-}

-- | 'appendWit' stated as a 'Predicate' implication.
--
-- @since 0.1.2.0
implyAppend :: IsAppend as bs --> EqualTo (as ++ bs)
implyAppend _ = appendWit

-- | Witness that @'Append' as bs cs@ implies @(as ++ bs) ~ cs@, using
-- @++@ from "Data.Vinyl.TypeLevel".
--
-- @since 0.1.2.0
appendWitV :: Append as bs cs -> (as V.++ bs) :~: cs
appendWitV = \case
  AppZ -> Refl
  AppS a -> case appendWitV a of
    Refl -> Refl

-- | The inverse of 'appendWitV': if we know @(as ++ bs) ~ cs@ (using @++@
-- from "Data.Vinyl.TypeLevel"), we can create an @'Append' as bs cs@ given
-- structure witnesses 'Sing'.
--
-- @since 0.1.2.0
unAppendWitV ::
  (as V.++ bs) ~ cs =>
  Rec f as ->
  Rec f bs ->
  Append as bs cs
unAppendWitV = \case
  RNil -> \_ -> AppZ
  _ :& xs -> AppS . unAppendWitV xs

-- | A useful pattern synonym for using 'Append' with @++@ from
-- "Data.Vinyl.TypeLevel".
--
-- As a /pattern/, this brings @(as ++ bs) ~ cs@ into the context whenever
-- you use it to match on an @'Append' as bs cs@.
--
-- As an /expression/, this constructs an @'Append' as bs cs@ as long as
-- you have @(as ++ bs) ~ cs@ in the context.
--
-- @since 0.1.2.0
pattern AppendWitV ::
  forall as bs cs. (RecApplicative as, RecApplicative bs) => (as V.++ bs) ~ cs => Append as bs cs
pattern AppendWitV <- (appendWitV @as @bs @cs -> Refl)
  where
    AppendWitV = unAppendWitV @as @bs @cs pureShape pureShape

{-# COMPLETE AppendWitV #-}

-- | Combine the powers of 'AppendWit' and 'AppendWitV' by matching on an
-- 'Append' to witness @(as ++ bs) ~ cs@ for /both/ @++@ from
-- "Data.Singletons.Prelude.List" and "Data.Vinyl.TypeLevel".  This also
-- witnesses that @(as ++ bs) ~ (as ++ bs)@ (for the two different @++@s)
-- by transitive property.
--
-- @since 0.1.2.0
pattern AppendWit' ::
  forall as bs cs.
  (RecApplicative as, RecApplicative bs) =>
  ((as ++ bs) ~ cs, (as V.++ bs) ~ cs) =>
  Append as bs cs
pattern AppendWit' <- ((\a -> (a, a)) -> (AppendWit, AppendWitV))
  where
    AppendWit' = AppendWit

{-# COMPLETE AppendWitV #-}

-- | 'appendWitV' stated as a 'Predicate' implication.
--
-- @since 0.1.2.0
implyAppendV :: IsAppend as bs --> EqualTo (as V.++ bs)
implyAppendV _ = appendWitV

-- | Given a witness @'Append' as bs cs@, prove that singleton's @++@ from
-- "Data.Singletons.Prelude.List" is the same as vinyl's @++@
-- "Data.Vinyl.TypeLevel".
convertAppends :: Append as bs cs -> (as ++ bs) :~: (as V.++ bs)
convertAppends a = case appendWit a of
  Refl -> case appendWitV a of
    Refl -> Refl

-- | Given @as@ and @bs@, create an @'Append' as bs cs@ with, with @cs@
-- existentially quantified
withAppend :: Rec f as -> Rec f bs -> (forall cs. Rec f cs -> Append as bs cs -> r) -> r
withAppend = \case
  RNil -> \ys f -> f ys AppZ
  x :& xs -> \ys f -> withAppend xs ys $ \zs a ->
    f (x :& zs) (AppS a)

-- | Get the 'Shape' associated with an 'Append''s prefix.
--
-- @since 0.1.3.0
appendShape ::
  Append as bs cs ->
  Shape [] as
appendShape = \case
  AppZ -> RNil
  AppS a -> Proxy :& appendShape a

-- | Witness an isomorphism between 'Rec' and two parts that compose it.
--
-- Read this type signature as:
--
-- @
-- 'splitRecIso'
--     :: Append as  bs  cs
--     -> Iso' (Rec f cs) (Rec f as, Rec f bs)
-- @
--
-- This can be used with the combinators from the lens library.
--
-- The 'Append' tells the point to split the 'Rec' at.
splitRecIso ::
  (Profunctor p, Functor f) =>
  Append as bs cs ->
  p (Rec g as, Rec g bs) (f (Rec g as, Rec g bs)) ->
  p (Rec g cs) (f (Rec g cs))
splitRecIso a = dimap (splitRec a) ((fmap . uncurry) (appendRec a))

-- | Split a 'Rec' into a prefix and suffix.  Basically 'takeRec'
-- and 'dropRec' combined.
splitRec ::
  Append as bs cs ->
  Rec f cs ->
  (Rec f as, Rec f bs)
splitRec = \case
  AppZ -> (RNil,)
  AppS a -> \case
    x :& xs -> first (x :&) . splitRec a $ xs

-- | Append two 'Rec's together according to an 'Append'.
appendRec ::
  Append as bs cs ->
  Rec f as ->
  Rec f bs ->
  Rec f cs
appendRec = \case
  AppZ -> \_ -> id
  AppS a -> \case
    x :& xs -> (x :&) . appendRec a xs

-- | Convert a 'Prefix' to an 'Append', with an existential @bs@.
prefixToAppend ::
  Prefix as cs ->
  (forall bs. Append as bs cs -> r) ->
  r
prefixToAppend = \case
  PreZ -> ($ AppZ)
  PreS p -> \f -> prefixToAppend p (f . AppS)

-- | Convert a 'Suffix' to an 'Append', with an existential @as@.
suffixToAppend ::
  Suffix bs cs ->
  (forall as. Append as bs cs -> r) ->
  r
suffixToAppend = \case
  SufZ -> ($ AppZ)
  SufS s -> \f -> suffixToAppend s (f . AppS)

-- | Split an 'Append' into a 'Prefix' and 'Suffix'.  Basically
-- 'appendToPrefix' and 'appendToSuffix' at the same time.
splitAppend ::
  Append as bs cs ->
  (Prefix as cs, Suffix bs cs)
splitAppend = \case
  AppZ -> (PreZ, SufZ)
  AppS a -> bimap PreS SufS . splitAppend $ a

-- | Convert an 'Append' to a 'Prefix', forgetting the suffix.
appendToPrefix :: Append as bs cs -> Prefix as cs
appendToPrefix = \case
  AppZ -> PreZ
  AppS a -> PreS . appendToPrefix $ a

-- | Convert an 'Append' to a 'Suffix', forgetting the prefix
appendToSuffix :: Append as bs cs -> Suffix bs cs
appendToSuffix = \case
  AppZ -> SufZ
  AppS a -> SufS . appendToSuffix $ a

-- | Split an 'Index' by an 'Append'.  If the 'Index' was in the first part
-- of the list, it'll return 'Left'.  If it was in the second part, it'll
-- return 'Right'.
--
-- This is essentially 'takeIndex' and 'dropIndex' at the same time.
splitIndex ::
  Append as bs cs ->
  Index cs x ->
  Either (Index as x) (Index bs x)
splitIndex = \case
  AppZ -> Right
  AppS a -> \case
    IZ -> Left IZ
    IS i -> first IS . splitIndex a $ i

-- | Shave off the final inhabitants of an 'Index', keeping only indices
-- a part of a given prefix.  If the index is out of range, 'Nothing' will
-- be returned.
--
-- This is essentially 'splitIndex', but taking only 'Left' results.
takeIndex ::
  Prefix as bs ->
  Index bs x ->
  Maybe (Index as x)
takeIndex p i =
  prefixToAppend p $
    either Just (const Nothing)
      . (`splitIndex` i)

-- | Shave off the initial inhabitants of an 'Index', keeping only indices
-- a part of a given suffix  If the index is out of range, 'Nothing' will
-- be returned.
--
-- This is essentially 'splitIndex', but taking only 'Right' results.
dropIndex ::
  Suffix as bs ->
  Index bs x ->
  Maybe (Index as x)
dropIndex s i =
  suffixToAppend s $
    either (const Nothing) Just
      . (`splitIndex` i)

-- | An index pointing to a given item in a prefix is also an index
-- pointing to the same item in the full list.  This "weakens" the bounds
-- of an index, widening the list at the end but preserving the original
-- index.  This is the inverse of 'takeIndex'.
weakenIndex ::
  Prefix as bs ->
  Index as x ->
  Index bs x
weakenIndex = \case
  PreZ -> \case {}
  PreS p -> \case
    IZ -> IZ
    IS i -> IS (weakenIndex p i)

-- | An index pointing to a given item in a suffix can be transformed into
-- an index pointing to the same item in the full list.  This is the
-- inverse of 'dropIndex'.
shiftIndex ::
  Suffix as bs ->
  Index as x ->
  Index bs x
shiftIndex = \case
  SufZ -> id
  SufS s -> IS . shiftIndex s

-- | A @'Interleave' as bs cs@ witnesses that @cs@ is @as@ interleaved with
-- @bs@. It is constructed by selectively zipping items from @as@ and @bs@
-- together, like mergesort or riffle shuffle.
--
-- You construct an 'Interleave' from @as@ and @bs@ by picking "which item" from
-- @as@ and @bs@ to add to @cs@.
--
-- Some examples:
--
-- @
-- IntL (IntL (IntR (IntR IntZ))) :: Interleave '[1,2] '[3,4] '[1,2,3,4]
-- IntR (IntR (IntL (IntL IntZ))) :: Interleave '[1,2] '[3,4] '[3,4,1,2]
-- IntL (IntR (IntL (IntR IntZ))) :: Interleave '[1,2] '[3,4] '[1,3,2,4]
-- IntR (IntL (IntR (IntL IntZ))) :: Interleave '[1,2] '[3,4] '[3,1,4,2]
-- @
--
-- @since 0.1.1.0
data Interleave :: [k] -> [k] -> [k] -> Type where
  IntZ :: Interleave '[] '[] '[]
  IntL :: Interleave as bs cs -> Interleave (a ': as) bs (a ': cs)
  IntR :: Interleave as bs cs -> Interleave as (b ': bs) (b ': cs)

deriving instance Show (Interleave as bs cs)

-- | A type-level predicate that a given list is the "interleave" of @as@
-- and @bs@.
--
-- @since 0.1.2.0
type IsInterleave as bs = TyPred (Interleave as bs)

instance Auto (IsInterleave '[] '[]) '[] where
  auto = IntZ

-- | Prefers 'IntL' if there is an ambiguity.
instance Auto (IsInterleave as bs) cs => Auto (IsInterleave (a ': as) bs) (a ': cs) where
  auto = IntL (auto @_ @(IsInterleave as bs) @cs)

instance {-# INCOHERENT #-} Auto (IsInterleave as bs) cs => Auto (IsInterleave as (b ': bs)) (b ': cs) where
  auto = IntR (auto @_ @(IsInterleave as bs) @cs)

instance (SDecide k, SingI (as :: [k]), SingI bs) => Decidable (IsInterleave as bs) where
  decide = case sing @as of
    SNil -> case sing @bs of
      SNil -> \case
        SNil -> Proved IntZ
        _ `SCons` _ -> Disproved $ \case {}
      y `SCons` (Sing :: Sing bs') -> \case
        z `SCons` (zs :: Sing cs') -> case y %~ z of
          Proved Refl -> case decide @(IsInterleave '[] bs') zs of
            Proved i -> Proved $ IntR i
            Disproved v -> Disproved $ \case
              IntR i -> v i
          Disproved v -> Disproved $ \case
            IntR _ -> v Refl
        SNil -> Disproved $ \case {}
    x `SCons` (Sing :: Sing as') -> case sing @bs of
      SNil -> \case
        z `SCons` (zs :: Sing cs') -> case x %~ z of
          Proved Refl -> case decide @(IsInterleave as' '[]) zs of
            Proved i -> Proved $ IntL i
            Disproved v -> Disproved $ \case
              IntL i -> v i
          Disproved v -> Disproved $ \case
            IntL _ -> v Refl
        SNil -> Disproved $ \case {}
      y `SCons` (Sing :: Sing bs') -> \case
        SNil -> Disproved $ \case {}
        z `SCons` (zs :: Sing cs') -> case x %~ z of
          Proved Refl -> case decide @(IsInterleave as' bs) zs of
            Proved i -> Proved $ IntL i
            Disproved v -> case y %~ z of
              Proved Refl -> case decide @(IsInterleave as bs') zs of
                Proved i -> Proved $ IntR i
                Disproved u -> Disproved $ \case
                  IntL i -> v i
                  IntR i -> u i
              Disproved u -> Disproved $ \case
                IntL i -> v i
                IntR _ -> u Refl
          Disproved v -> case y %~ z of
            Proved Refl -> case decide @(IsInterleave as bs') zs of
              Proved i -> Proved $ IntR i
              Disproved u -> Disproved $ \case
                IntL _ -> v Refl
                IntR i -> u i
            Disproved u -> Disproved $ \case
              IntL _ -> v Refl
              IntR _ -> u Refl

-- | Automatically generate an 'Interleave' if @as@, @bs@, and @cs@ are
-- known statically.
--
-- Prefers the left side if there is an ambiguity:
--
-- @
-- IntL (IntR (IntR IntZ) :: Interleave '[1] '[1,2] '[1,1,2]
-- IntR (IntL (IntR IntZ) :: Interleave '[1] '[1,2] '[1,1,2]
--
-- autoInterleave @'[1] @'[1,2] '[1,1,2] == IntL (IntR (IntR IntZ)
-- @
--
-- @since 0.1.2.0
autoInterleave :: forall as bs cs. Auto (IsInterleave as bs) cs => Interleave as bs cs
autoInterleave = auto @_ @(IsInterleave as bs) @cs

-- | Given two 'Rec's, interleave the two to create a combined 'Rec'.
--
-- @since 0.1.1.0
interleaveRec :: Interleave as bs cs -> Rec f as -> Rec f bs -> Rec f cs
interleaveRec = \case
  IntZ -> \case
    RNil -> \case
      RNil -> RNil
  IntL m -> \case
    x :& xs -> \ys -> x :& interleaveRec m xs ys
  IntR m -> \xs -> \case
    y :& ys -> y :& interleaveRec m xs ys

-- | Given a 'Rec', disinterleave it into two 'Rec's corresponding to an
-- 'Interleave'.
--
-- @since 0.1.1.0
unweaveRec :: Interleave as bs cs -> Rec f cs -> (Rec f as, Rec f bs)
unweaveRec = \case
  IntZ -> \case
    RNil -> (RNil, RNil)
  IntL m -> \case
    x :& xs -> first (x :&) . unweaveRec m $ xs
  IntR m -> \case
    x :& xs -> second (x :&) . unweaveRec m $ xs

-- | Turn an 'Interleave' into a 'Rec' of indices from either sublist.
--
-- Warning: O(n^2)
--
-- @since 0.1.2.0
interleavedIxes :: Interleave as bs cs -> Rec (Index as :+: Index bs) cs
interleavedIxes = \case
  IntZ -> RNil
  IntL i ->
    L1 IZ
      :& VR.rmap
        (\case L1 i' -> L1 (IS i'); R1 j -> R1 j)
        (interleavedIxes i)
  IntR j ->
    R1 IZ
      :& VR.rmap
        (\case L1 i -> L1 i; R1 j' -> R1 (IS j'))
        (interleavedIxes j)

-- | Interleave an 'Index' on @as@ into a full index on @cs@, which is @as@
-- interleaved with @bs@.
--
-- @since 0.1.1.0
injectIndexL :: Interleave as bs cs -> Index as a -> Index cs a
injectIndexL = \case
  IntZ -> \case {}
  IntL m -> \case
    IZ -> IZ
    IS i -> IS (injectIndexL m i)
  IntR m -> IS . injectIndexL m

-- | Interleave an 'Index' on @bs@ into a full index on @cs@, which is @as@
-- interleaved with @bs@.
--
-- @since 0.1.1.0
injectIndexR :: Interleave as bs cs -> Index bs b -> Index cs b
injectIndexR = \case
  IntZ -> \case {}
  IntL m -> IS . injectIndexR m
  IntR m -> \case
    IZ -> IZ
    IS i -> IS (injectIndexR m i)

-- | Given an index on @cs@, disinterleave it into either an index on @as@
-- or on @bs@.
--
-- @since 0.1.1.0
unweaveIndex :: Interleave as bs cs -> Index cs c -> Either (Index as c) (Index bs c)
unweaveIndex = \case
  IntZ -> \case {}
  IntL m -> \case
    IZ -> Left IZ
    IS i -> first IS $ unweaveIndex m i
  IntR m -> \case
    IZ -> Right IZ
    IS i -> second IS $ unweaveIndex m i

-- | Witness an isomorphism between 'Rec' and two parts that interleave it.
--
-- Read this type signature as:
--
-- @
-- 'interleaveRecIso'
--     :: Interleave as  bs  cs
--     -> Iso' (Rec f cs) (Rec f as, Rec f bs)
-- @
--
-- This can be used with the combinators from the lens library.
--
-- The 'Interleave' tells how to unweave the 'Rec'.
--
-- @since 0.1.1.0
interleaveRecIso ::
  (Profunctor p, Functor f) =>
  Interleave as bs cs ->
  p (Rec g as, Rec g bs) (f (Rec g as, Rec g bs)) ->
  p (Rec g cs) (f (Rec g cs))
interleaveRecIso m = dimap (unweaveRec m) ((fmap . uncurry) (interleaveRec m))

-- | Swap the two halves of an 'Interleave'.
--
-- @since 0.1.3.0
swapInterleave ::
  Interleave as bs cs ->
  Interleave bs as cs
swapInterleave = \case
  IntZ -> IntZ
  IntL i -> IntR $ swapInterleave i
  IntR i -> IntL $ swapInterleave i

-- | Get the 'Shape's associated with an 'Interleave'.
--
-- @since 0.1.3.0
interleaveShapes ::
  Interleave as bs cs ->
  (Shape [] as, Shape [] bs, Shape [] cs)
interleaveShapes = \case
  IntZ -> (RNil, RNil, RNil)
  IntL i ->
    let (as, bs, cs) = interleaveShapes i
     in (Proxy :& as, bs, Proxy :& cs)
  IntR i ->
    let (as, bs, cs) = interleaveShapes i
     in (as, Proxy :& bs, Proxy :& cs)

-- | A @'Subset' as bs@ witnesses that @as@ is some subset of @bs@, with
-- items in the same order.  It is constructed by specifying
-- what item to include or exclude in @bs@ from @as@.  It is essentially
-- 'Interleave', but without one of the two initial parameters.
--
-- You construct an 'Subset' from @cs@ by picking "which item" from
-- @bs@ to add to @as@.
--
-- Some examples:
--
-- @
-- SubsetNo  (SubsetNo  (SubsetNo  SubsetNil)) :: Subset '[]      '[1,2,3]
-- SubsetYes (SubsetNo  (SubsetNo  SubsetNil)) :: Subset '[1]     '[1,2,3]
-- SubsetNo  (SubsetNo  (SubsetYes SubsetNil)) :: Subset '[3]     '[1,2,3]
-- SubsetYes (SubsetNo  (SubsetYes SubsetNil)) :: Subset '[1,3]   '[1,2,3]
-- SubsetYes (SubsetYes (SubsetYes SubsetNil)) :: Subset '[1,2,3] '[1,2,3]
-- @
--
-- @since 0.1.3.0
data Subset :: [k] -> [k] -> Type where
  SubsetNil :: Subset '[] '[]
  SubsetNo :: Subset as bs -> Subset as (b ': bs)
  SubsetYes :: Subset as bs -> Subset (a ': as) (a ': bs)

-- | Drop the right side of an 'Interleave', leaving only the left side.
interleaveLToSubset :: Interleave as bs cs -> Subset as cs
interleaveLToSubset = \case
  IntZ -> SubsetNil
  IntL i -> SubsetYes . interleaveLToSubset $ i
  IntR i -> SubsetNo . interleaveLToSubset $ i

-- | Drop the left side of an 'Interleave', leaving only the right side.
interleaveRToSubset :: Interleave as bs cs -> Subset bs cs
interleaveRToSubset = \case
  IntZ -> SubsetNil
  IntL i -> SubsetNo . interleaveRToSubset $ i
  IntR i -> SubsetYes . interleaveRToSubset $ i

-- | Convert a 'Subset' into an left 'Interleave', recovering the dropped
-- items.
subsetToInterleaveL ::
  Subset as cs ->
  (forall bs. Interleave as bs cs -> r) ->
  r
subsetToInterleaveL = \case
  SubsetNil -> \f -> f IntZ
  SubsetNo s -> \f -> subsetToInterleaveL s (f . IntR)
  SubsetYes s -> \f -> subsetToInterleaveL s (f . IntL)

-- | Convert a 'Subset' into an right 'Interleave', recovering the dropped
-- items.
subsetToInterleaveR ::
  Subset bs cs ->
  (forall as. Interleave as bs cs -> r) ->
  r
subsetToInterleaveR = \case
  SubsetNil -> \f -> f IntZ
  SubsetNo s -> \f -> subsetToInterleaveR s (f . IntL)
  SubsetYes s -> \f -> subsetToInterleaveR s (f . IntR)

-- | @as@ is a subset of @cs@; this function recovers @bs@, the subset of
-- @cs@ that is not @as@.
subsetComplement ::
  Subset as cs ->
  (forall bs. Subset bs cs -> r) ->
  r
subsetComplement = \case
  SubsetNil -> \f -> f SubsetNil
  SubsetNo s -> \f -> subsetComplement s (f . SubsetYes)
  SubsetYes s -> \f -> subsetComplement s (f . SubsetNo)

deriving instance Show (Subset as bs)

-- | A type-level predicate that a given list is a "superset" of @as@, in
-- correct order
--
-- @since 0.1.2.0
type IsSubset as = TyPred (Subset as)

instance Auto (IsSubset '[]) '[] where
  auto = SubsetNil

instance {-# OVERLAPPING #-} Auto (IsSubset as) bs => Auto (IsSubset as) (b ': bs) where
  auto = SubsetNo (auto @_ @(IsSubset as) @bs)

instance {-# OVERLAPPING #-} Auto (IsSubset as) bs => Auto (IsSubset (a ': as)) (a ': bs) where
  auto = SubsetYes (auto @_ @(IsSubset as) @bs)

instance (SDecide k, SingI (as :: [k])) => Decidable (IsSubset as) where
  decide = case sing @as of
    SNil -> \case
      SNil -> Proved SubsetNil
      _ `SCons` ys -> case decide @(IsSubset '[]) ys of
        Proved s -> Proved $ SubsetNo s
        Disproved v -> Disproved $ \case
          SubsetNo s -> v s
    x `SCons` (Sing :: Sing as') -> \case
      SNil -> Disproved $ \case {}
      y `SCons` ys -> case x %~ y of
        Proved Refl -> case decide @(IsSubset as') ys of
          Proved s -> Proved $ SubsetYes s
          Disproved v -> case decide @(IsSubset as) ys of
            Proved s -> Proved $ SubsetNo s
            Disproved u -> Disproved $ \case
              SubsetNo s -> u s
              SubsetYes s -> v s
        Disproved v -> case decide @(IsSubset as) ys of
          Proved s -> Proved $ SubsetNo s
          Disproved u -> Disproved $ \case
            SubsetNo s -> u s
            SubsetYes _ -> v Refl

-- | Automatically generate an 'Subset' if @as@ and @bs@ are known
-- statically.
autoSubset :: forall as bs. Auto (IsSubset as) bs => Subset as bs
autoSubset = auto @_ @(IsSubset as) @bs

-- | A lens into a subset of a record, indicated by a 'Subset'.
subsetRec :: Subset as bs -> Lens' (Rec f bs) (Rec f as)
subsetRec = \case
  SubsetNil -> id
  SubsetNo s -> \f -> \case
    x :& xs -> (x :&) <$> subsetRec s f xs
  SubsetYes s -> \f -> \case
    x :& xs ->
      fmap (uncurry (:&))
        . getCompose
        . subsetRec s (Compose . fmap (\(y :& ys) -> (y, ys)) . f . (x :&))
        $ xs

-- | Take a subset out of a 'Rec'.  An alias for @'view' ('subsetRec' s)@.
getSubset ::
  Subset as bs ->
  Rec f bs ->
  Rec f as
getSubset = view . subsetRec

-- | Get all of the indices of all the items in a 'Subset'.
subsetIxes ::
  Subset as bs ->
  Rec (Index bs) as
subsetIxes s = getSubset s . imapProd const $ sp
  where
    (_, sp) = subsetShapes s

-- | Get the 'Shape's associated with a 'Subset'.
subsetShapes ::
  Subset as bs ->
  (Shape [] as, Shape [] bs)
subsetShapes = \case
  SubsetNil -> (RNil, RNil)
  SubsetNo s -> second (Proxy :&) $ subsetShapes s
  SubsetYes s -> bimap (Proxy :&) (Proxy :&) $ subsetShapes s

-- | Because @as@ is a subset of @bs@, an index into @as@ should also be an
-- index into @bs@.  This performs that transformation.
--
-- This is like a version of 'injectIndexL' or 'injectIndexR', for
-- 'Subset'.
weakenSubsetIndex ::
  Subset as bs ->
  Index as a ->
  Index bs a
weakenSubsetIndex = \case
  SubsetNil -> \case {}
  SubsetNo s -> IS . weakenSubsetIndex s
  SubsetYes s -> \case
    IZ -> IZ
    IS i -> IS $ weakenSubsetIndex s i

-- | Because @as@ is a subset of @bs@, we can /sometimes/ transform an
-- index into @bs@ into an index into @as@.  This performs that
-- transformation.  If it succeeds, it means that the index in @bs@ also
-- exists in @as@; otherwise, it means that the index in @bs@ was excluded
-- from @as@.
--
-- Note that if the index into @a@ was excluded from @as@, it doesn't
-- necessarily mean that there is no @a@ in @bs@ --- @bs@ could contain
-- a duplicate that was included into @as@.  This converts into an index to
-- the exact same item (positionlly) in the list, if it is possible.
--
-- This is like a version of 'unweaveIndex', but for 'Subset'.
strengthenSubsetIndex ::
  Subset as bs ->
  Index bs a ->
  Maybe (Index as a)
strengthenSubsetIndex = \case
  SubsetNil -> \case {}
  SubsetNo s -> \case
    IZ -> Nothing
    IS i -> strengthenSubsetIndex s i
  SubsetYes s -> \case
    IZ -> Just IZ
    IS i -> IS <$> strengthenSubsetIndex s i
