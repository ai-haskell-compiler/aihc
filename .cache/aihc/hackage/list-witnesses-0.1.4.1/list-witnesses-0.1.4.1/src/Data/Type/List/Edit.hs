{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Data.Type.List.Edit
-- Copyright   : (c) Justin Le 2023
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Witnesses regarding single-item edits of lists.
module Data.Type.List.Edit (
  -- * Simple edits
  Insert (..),
  autoInsert,
  Delete (..),
  autoDelete,
  insToDel,
  delToIns,
  Substitute (..),
  autoSubstitute,
  flipSub,
  subToDelIns,

  -- ** Predicates
  IsInsert,
  InsertedInto,
  IsDelete,
  DeletedFrom,
  IsSubstitute,

  -- ** Singletons
  SInsert (..),
  SDelete (..),
  SSubstitute (..),

  -- * Compound edits
  Edit (..),
  compEdit,
  flipEdit,

  -- * Rec
  insertRec,
  deleteRec,
  deleteGetRec,
  recLens,
  substituteRec,

  -- * Index

  -- ** Manipulating indices
  insertIndex,
  DeletedIx (..),
  deleteIndex,
  deleteIndex_,
  SubstitutedIx (..),
  substituteIndex,
  substituteIndex_,

  -- ** Converting from indices
  withDelete,
  withInsert,
  withInsertAfter,

  -- * Type-Level
  InsertIndex,
  sInsertIndex,
  SDeletedIx (..),
  DeleteIndex,
  sDeleteIndex,
  SSubstitutedIx (..),
  SubstituteIndex,
  sSubstituteIndex,

  -- ** Defunctionalization Symbols
  InsertIndexSym0,
  InsertIndexSym,
  DeleteIndexSym0,
  DeleteIndexSym,
  SubstituteIndexSym0,
  SubstituteIndexSym,
) where

import qualified Control.Category as C
import Data.Function.Singletons (IdSym0)
import Data.Kind
import Data.List.Singletons (SList (..))
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Sigma
import Data.Type.Functor.Product
import Data.Type.Predicate
import Data.Type.Predicate.Auto
import Data.Type.Predicate.Param
import Lens.Micro hiding ((%~))

-- | An @'Insert' as bs x@ is a witness that you can insert @x@ into some
-- position in list @as@ to produce list @bs@.  It is essentially 'Delete'
-- flipped.
--
-- Some examples:
--
-- @
-- InsZ                   :: Insert '[1,2,3] '[4,1,2,3] 4
-- InsS InsZ              :: Insert '[1,2,3] '[1,4,2,3] 4
-- InsS (InsS InsZ)       :: Insert '[1,2,3] '[1,2,4,3] 4
-- InsS (InsS (InsS InsZ) :: Insert '[1,2,3] '[1,2,3,4] 4
-- @
--
-- @bs@ will always be exactly one item longer than @as@.
data Insert :: [k] -> [k] -> k -> Type where
  InsZ :: Insert as (x ': as) x
  InsS :: Insert as bs x -> Insert (a ': as) (a ': bs) x

deriving instance Show (Insert as bs x)

-- | A type-level predicate that a given value can be used as an insertion
-- to change @as@ to @bs@.
--
-- @since 0.1.2.0
type IsInsert as bs = TyPred (Insert as bs)

-- | Prefers the "earlier" insert if there is ambiguity
instance Auto (IsInsert as (x ': as)) x where
  auto = InsZ

instance {-# INCOHERENT #-} Auto (IsInsert as bs) x => Auto (IsInsert (a ': as) (a ': bs)) x where
  auto = InsS (auto @_ @(IsInsert as bs) @x)

instance (SDecide k, SingI (as :: [k]), SingI bs) => Decidable (IsInsert as bs) where
  decide z = case sing @bs of
    SNil -> Disproved $ \case {}
    y `SCons` (ys@Sing :: Sing bs') -> case y %~ z of
      Proved Refl -> case sing @as %~ ys of
        Proved Refl -> Proved InsZ
        Disproved v -> case sing @as of
          SNil -> Disproved $ \case
            InsZ -> v Refl
          x `SCons` (Sing :: Sing as') -> case x %~ y of
            Proved Refl -> case decide @(IsInsert as' bs') z of
              Proved i -> Proved $ InsS i
              Disproved u -> Disproved $ \case
                InsZ -> u InsZ
                InsS i -> u i
            Disproved u -> Disproved $ \case
              InsZ -> v Refl
              InsS _ -> u Refl
      Disproved v -> case sing @as of
        SNil -> Disproved $ \case
          InsZ -> v Refl
        x `SCons` (Sing :: Sing as') -> case x %~ y of
          Proved Refl -> case decide @(IsInsert as' bs') z of
            Proved i -> Proved $ InsS i
            Disproved u -> Disproved $ \case
              InsZ -> u InsZ
              InsS i -> u i
          Disproved u -> Disproved $ \case
            InsZ -> v Refl
            InsS _ -> u Refl

-- | If @bs@ satisfies @'InsertedInto' as@, it means that there exists some
-- element @x@ such that @'IsInsert' as bs \@\@ x@: you can get @bs@ by
-- inserting @x@ into @as@ somewhere.
--
-- In other words, @'InsertedInto' as@ is satisfied by @bs@ if you can turn
-- @as@ into @bs@ by inserting one individual item.
--
-- You can find this element (if it exists) using 'search', or the
-- 'Decidable' instance of @'Found' ('InsertedInto' as)@:
--
-- @
-- 'searchTC' :: SingI as => Sing bs -> 'Decision' ('Σ' k ('IsInsert' as bs))
-- @
--
-- This will find you the single element you need to insert into @as@ to
-- get @bs@, if it exists.
--
-- @since 0.1.2.0
type InsertedInto (as :: [k]) = (TyPP (Insert as) :: ParamPred [k] k)

instance (SDecide k, SingI (as :: [k])) => Decidable (Found (InsertedInto as)) where
  decide = \case
    SNil -> Disproved $ \(_ :&: i) -> case i of {}
    y `SCons` ys -> case sing @as %~ ys of
      Proved Refl -> Proved $ y :&: InsZ
      Disproved v -> case sing @as of
        SNil -> Disproved $ \(_ :&: i) -> case i of
          InsZ -> v Refl
        x `SCons` (Sing :: Sing as') -> case x %~ y of
          Proved Refl -> case decide @(Found (InsertedInto as')) ys of
            Proved (z :&: i) -> Proved $ z :&: InsS i
            Disproved u -> Disproved $ \(z :&: i) -> case i of
              InsZ -> u $ z :&: InsZ
              InsS i' -> u $ z :&: i'
          Disproved u -> Disproved $ \(_ :&: i) -> case i of
            InsZ -> v Refl
            InsS _ -> u Refl

-- | Automatically generate an 'Insert' if @as@, @bs@ and @x@ are known
-- statically.
--
-- Prefers the earlier insertion if there is an ambiguity.
--
-- @
-- InsS InsZ        :: Insert '[1,2] '[1,2,2] 2
-- InsS (InsS InsZ) :: Insert '[1,2] '[1,2,2] 2
-- autoInsert @'[1,2] @'[1,2,2] @2 == InsS InsZ
-- @
--
-- @since 0.1.2.0
autoInsert :: forall as bs x. Auto (IsInsert as bs) x => Insert as bs x
autoInsert = auto @_ @(IsInsert as bs) @x

-- | Kind-indexed singleton for 'Insert'.
data SInsert (as :: [k]) (bs :: [k]) (x :: k) :: Insert as bs x -> Type where
  SInsZ :: SInsert as (x ': as) x 'InsZ
  SInsS :: SInsert as bs x ins -> SInsert (a ': as) (a ': bs) x ('InsS ins)

deriving instance Show (SInsert as bs x del)

-- | Flip an insertion.
insToDel :: Insert as bs x -> Delete bs as x
insToDel = \case
  InsZ -> DelZ
  InsS i -> DelS (insToDel i)

-- | A @'Delete' as bs x@ is a witness that you can delete item @x@ from
-- @as@ to produce the list @bs@.  It is essentially 'Insert' flipped.
--
-- Some examples:
--
-- @
-- DelZ             :: Delete '[1,2,3] '[2,3] 1
-- DelS DelZ        :: Delete '[1,2,3] '[2,3] 2
-- DelS (DelS DelZ) :: Delete '[1,2,3] '[1,2] 3
-- @
--
-- @bs@ will always be exactly one item shorter than @as@.
data Delete :: [k] -> [k] -> k -> Type where
  DelZ :: Delete (x ': as) as x
  DelS :: Delete as bs x -> Delete (a ': as) (a ': bs) x

deriving instance Show (Delete as bs x)

-- | A type-level predicate that a given value can be used as a deletion
-- to change @as@ to @bs@.
--
-- @since 0.1.2.0
type IsDelete as bs = TyPred (Delete as bs)

-- | Prefers the "earlier" delete if there is ambiguity
instance Auto (IsDelete (x ': as) as) x where
  auto = DelZ

instance {-# INCOHERENT #-} Auto (IsDelete as bs) x => Auto (IsDelete (a ': as) (a ': bs)) x where
  auto = DelS (auto @_ @(IsDelete as bs) @x)

instance (SDecide k, SingI (as :: [k]), SingI bs) => Decidable (IsDelete as bs) where
  decide = mapDecision insToDel delToIns . decide @(IsInsert bs as)

-- | If @bs@ satisfies @'DeletedFrom' as@, it means that there exists some
-- element @x@ such that @'IsDelete' as bs \@\@ x@: you can get @bs@ by
-- deleting @x@ from @as@ somewhere.
--
-- In other words, @'DeletedFrom' as@ is satisfied by @bs@ if you can turn
-- @as@ into @bs@ by deleting one individual item.
--
-- You can find this element (if it exists) using 'search', or the
-- 'Decidable' instance of @'Found' ('DeletedFrom' as)@.
--
-- @
-- 'searchTC' :: SingI as => Sing bs -> 'Decision' ('Σ' k ('IsDelete' as bs))
-- @
--
-- This will find you the single element you need to delete from @as@ to
-- get @bs@, if it exists.
--
-- @since 0.1.2.0
type DeletedFrom (as :: [k]) = (TyPP (Delete as) :: ParamPred [k] k)

instance (SDecide k, SingI (as :: [k])) => Decidable (Found (DeletedFrom as)) where
  decide (Sing :: Sing bs) =
    mapDecision
      (mapSigma (sing @IdSym0) insToDel)
      (mapSigma (sing @IdSym0) delToIns)
      $ decide @(Found (InsertedInto bs)) (sing @as)

-- | Automatically generate an 'Delete' if @as@, @bs@ and @x@ are known
-- statically.
--
-- Prefers the earlier deletion if there is an ambiguity.
--
-- @
-- DelS DelZ        :: Delete '[1,2,2] '[1,2] 2
-- DelS (DelS DelZ) :: Delete '[1,2,2] '[1,2] 2
-- autoDelete @'[1,2,2] @'[1,2] @2 == DelS DelZ
-- @
--
-- @since 0.1.2.0
autoDelete :: forall as bs x. Auto (IsDelete as bs) x => Delete as bs x
autoDelete = auto @_ @(IsDelete as bs) @x

-- | Kind-indexed singleton for 'Delete'.
data SDelete (as :: [k]) (bs :: [k]) (x :: k) :: Delete as bs x -> Type where
  SDelZ :: SDelete (x ': as) as x 'DelZ
  SDelS :: SDelete as bs x del -> SDelete (a ': as) (a ': bs) x ('DelS del)

deriving instance Show (SDelete as bs x del)

-- | Flip a deletion.
delToIns :: Delete as bs x -> Insert bs as x
delToIns = \case
  DelZ -> InsZ
  DelS d -> InsS (delToIns d)

-- | A @'Substitute' as bs x y@ is a witness that you can replace item @x@ in
-- @as@ with item @y@ to produce @bs@.
--
-- Some examples:
--
-- @
-- SubZ             :: Substitute '[1,2,3] '[4,2,3] 1 4
-- SubS SubZ        :: Substitute '[1,2,3] '[1,4,3] 2 4
-- SubS (SubS SubZ) :: Substitute '[1,2,3] '[1,2,4] 3 4
-- @
data Substitute :: [k] -> [k] -> k -> k -> Type where
  SubZ :: Substitute (x ': as) (y ': as) x y
  SubS :: Substitute as bs x y -> Substitute (c ': as) (c ': bs) x y

deriving instance Show (Substitute as bs x y)

-- | A type-level predicate that a given value can be used as
-- a substitution of @x@ to change @as@ to @bs@.
--
-- @since 0.1.2.0
type IsSubstitute as bs x = TyPred (Substitute as bs x)

instance Auto (IsSubstitute (x ': as) (y ': as) x) y where
  auto = SubZ

-- | Prefers the earlier subsitution if there is ambiguity.
instance Auto (IsSubstitute as bs x) y => Auto (IsSubstitute (c ': as) (c ': bs) x) y where
  auto = SubS (auto @_ @(IsSubstitute as bs x) @y)

instance {-# INCOHERENT #-} Auto (IsSubstitute (x ': as) (x ': as) x) x where
  auto = SubZ

-- | Automatically generate an 'Substitute' if @as@, @bs@, @x@, and @y@ are
-- known statically.
--
-- Prefers the "earlier" substitution if there is an ambiguity
--
-- @
-- SubZ      :: Substitute '[1,1] '[1,1] 1 1
-- SubS SubZ :: Substitute '[1,1] '[1,1] 1 1
-- autoSubstitute @'[1,1] @'[1,1] @1 @1 == SubZ
-- @
--
-- @since 0.1.2.0
autoSubstitute :: forall as bs x y. Auto (IsSubstitute as bs x) y => Substitute as bs x y
autoSubstitute = auto @_ @(IsSubstitute as bs x) @y

-- | Kind-indexed singleton for 'Substitute'.
data SSubstitute (as :: [k]) (bs :: [k]) (x :: k) (y :: k) :: Substitute as bs x y -> Type where
  SSubZ :: SSubstitute (x ': as) (y ': as) x y 'SubZ
  SSubS ::
    SSubstitute as bs x y sub ->
    SSubstitute (c ': as) (c ': bs) x y ('SubS sub)

-- | Flip a substitution
flipSub :: Substitute as bs x y -> Substitute bs as y x
flipSub = \case
  SubZ -> SubZ
  SubS s -> SubS (flipSub s)

-- | Decompose a 'Substitute' into a 'Delete' followed by an 'Insert'.
subToDelIns ::
  Substitute as bs x y ->
  (forall cs. Delete as cs x -> Insert cs bs y -> r) ->
  r
subToDelIns = \case
  SubZ -> \f -> f DelZ InsZ
  SubS s -> \f -> subToDelIns s $ \d i -> f (DelS d) (InsS i)

-- | An @'Edit' as bs@ is a reversible edit script transforming @as@ into
-- @bs@ through successive insertions, deletions, and substitutions.
--
-- TODO: implement Wagner-Fischer or something similar to minimize find
-- a minimal edit distance
data Edit :: [k] -> [k] -> Type where
  ENil :: Edit as as
  EIns :: Insert bs cs x -> Edit as bs -> Edit as cs
  EDel :: Delete bs cs x -> Edit as bs -> Edit as cs
  ESub :: Substitute bs cs x y -> Edit as bs -> Edit as cs

deriving instance Show (Edit as bs)

-- | Compose two 'Edit's
compEdit :: Edit as bs -> Edit bs cs -> Edit as cs
compEdit xs = \case
  ENil -> xs
  EIns i ys -> EIns i (compEdit xs ys)
  EDel d ys -> EDel d (compEdit xs ys)
  ESub s ys -> ESub s (compEdit xs ys)

-- | 'Edit' composition
instance C.Category Edit where
  id = ENil
  xs . ys = compEdit ys xs

-- | Reverse an 'Edit' script.  O(n^2).  Please do not use ever in any
-- circumstance.
--
-- TODO: Make O(n) using diff lists.
flipEdit :: Edit as bs -> Edit bs as
flipEdit = \case
  ENil -> ENil
  EIns i ys -> EDel (insToDel i) ENil `compEdit` flipEdit ys
  EDel d ys -> EIns (delToIns d) ENil `compEdit` flipEdit ys
  ESub s ys -> ESub (flipSub s) ENil `compEdit` flipEdit ys

-- | Insert a value into a 'Rec', at a position indicated by the 'Insert'.
insertRec :: Insert as bs x -> f x -> Rec f as -> Rec f bs
insertRec = \case
  InsZ -> (:&)
  InsS i -> \x -> \case
    y :& ys -> y :& insertRec i x ys

-- | Retrieve and delete a value in a 'Rec', at a position indicated by the 'Delete'.
deleteGetRec :: Delete as bs x -> Rec f as -> (f x, Rec f bs)
deleteGetRec = \case
  DelZ -> \case
    x :& xs -> (x, xs)
  DelS d -> \case
    x :& xs ->
      let (y, ys) = deleteGetRec d xs
       in (y, x :& ys)

-- | Delete a value in a 'Rec', at a position indicated by the 'Delete'.
deleteRec :: Delete as bs x -> Rec f as -> Rec f bs
deleteRec = \case
  DelZ -> \case
    _ :& xs -> xs
  DelS d -> \case
    x :& xs -> x :& deleteRec d xs

-- | A type-changing lens into a value in a 'Rec', given a 'Substitute'
-- indicating which value.
--
-- For example:
--
-- @
-- recLens (SubS SubZ)
--      :: Lens (Rec f '[a,b,c,d]) (Rec f '[a,e,c,d])
--              (f b)              (f e)
-- @
--
-- The number of 'SubS' in the index essentially indicates the index to
-- edit at.
--
-- This is similar to 'Data.Vinyl.Lens.rlensC' from /vinyl/, but is built
-- explicitly and inductively, instead of using typeclass magic.
recLens ::
  forall as bs x y f.
  () =>
  Substitute as bs x y ->
  Lens (Rec f as) (Rec f bs) (f x) (f y)
recLens s0 (f :: f x -> g (f y)) = go s0
  where
    go ::
      Substitute cs ds x y ->
      Rec f cs ->
      g (Rec f ds)
    go = \case
      SubZ -> \case
        x :& xs -> (:& xs) <$> f x
      SubS s -> \case
        x :& xs -> (x :&) <$> go s xs

-- | Substitute a value in a 'Rec' at a given position, indicated by the
-- 'Substitute'.  This is essentially a specialized version of 'recLens'.
substituteRec ::
  Substitute as bs x y ->
  (f x -> f y) ->
  Rec f as ->
  Rec f bs
substituteRec s = over (recLens s)

-- | If you add an item to @as@ to create @bs@, you also need to shift an
-- @'Index' as y@ to @Index bs y@.  This shifts the 'Index' in @as@ to
-- become an 'Index' in @bs@, but makes sure that the index points to the
-- same original value.
insertIndex :: Insert as bs x -> Index as y -> Index bs y
insertIndex = \case
  InsZ -> IS
  InsS ins -> \case
    IZ -> IZ
    IS i -> IS (insertIndex ins i)

-- | Used as the return type of 'deleteIndex'.  An @'DeletedIx' bs x y@ is
-- like a @'Maybe' ('Index' bs y)@, except the 'Nothing' case witnesses
-- that @x ~ y@.
data DeletedIx :: [k] -> k -> k -> Type where
  GotDeleted :: DeletedIx bs x x
  NotDeleted :: Index bs y -> DeletedIx bs x y

deriving instance Show (DeletedIx bs x y)

-- | If you delete an item in @as@ to create @bs@, you also need to move
-- @'Index' as y@ into @Index bs y@.  This transforms the 'Index' in @as@
-- to become an 'Index' in @bs@, making sure the index points to the same
-- original value.
--
-- However, there is a possibility that the deleted item is the item that
-- the index was originally pointing to.  If this is the case, this
-- function returns 'GotDeleted', a witness that @x ~ y@.  Otherwise, it
-- returns 'NotDeleted' with the unshifted index.
deleteIndex :: Delete as bs x -> Index as y -> DeletedIx bs x y
deleteIndex = \case
  DelZ -> \case
    IZ -> GotDeleted
    IS i -> NotDeleted i
  DelS del -> \case
    IZ -> NotDeleted IZ
    IS i -> case deleteIndex del i of
      GotDeleted -> GotDeleted
      NotDeleted j -> NotDeleted (IS j)

-- | A version of 'deleteIndex' returning a simple 'Maybe'.  This can be
-- used if you don't care about witnessing that @x ~ y@ in the case that
-- the index is the item that is deleted.
deleteIndex_ :: Delete as bs x -> Index as y -> Maybe (Index bs y)
deleteIndex_ del i = case deleteIndex del i of
  GotDeleted -> Nothing
  NotDeleted j -> Just j

-- | Used as the return type of 'substituteIndex'.  An @'SubstitutedIx' bs x y z@ is
-- like an @'Either' ('Index' bs y) ('Index' bs z)@, except the 'Left' case
-- witnesses that @x ~ z@.
data SubstitutedIx :: [k] -> k -> k -> k -> Type where
  GotSubbed :: Index bs y -> SubstitutedIx bs z y z
  NotSubbed :: Index bs z -> SubstitutedIx bs x y z

-- | If you substitute an item in @as@ to create @bs@, you also need to
-- reshift @'Index' as z@ into @'Index' bs z@.  This reshifts the 'Index'
-- in @as@ to become an 'Index' in @bs@, making sure the index points to
-- the same original value.
--
-- However, there is a possibility that the substituted item is the item
-- that the index was originally pointing to.  If this is the case, this
-- function returns 'GotSubbed', a witness that @x ~ z@.  Otherwise, it
-- returns 'NotSubbed'.  Both contain the updated index.
substituteIndex ::
  Substitute as bs x y ->
  Index as z ->
  SubstitutedIx bs x y z
substituteIndex = \case
  SubZ -> \case
    IZ -> GotSubbed IZ
    IS i -> NotSubbed (IS i)
  SubS s -> \case
    IZ -> NotSubbed IZ
    IS i -> case substituteIndex s i of
      GotSubbed j -> GotSubbed (IS j)
      NotSubbed j -> NotSubbed (IS j)

-- | A version of 'substituteIndex' returning a simple 'Either'.  This can be
-- the case if you don't care about witnessing @x ~ z@ in the case that the
-- index is the item that was substituted.
substituteIndex_ ::
  Substitute as bs x y ->
  Index as z ->
  Either (Index bs y) (Index bs z)
substituteIndex_ sub i = case substituteIndex sub i of
  GotSubbed j -> Left j
  NotSubbed j -> Right j

-- | Given an 'Index' pointing to an element, create a 'Delete'
-- corresponding to the given item.  The type of the resulting list is
-- existentially quantified, is guaranteed to be just exactly the original
-- list minus the specified element.
withDelete ::
  Index as x ->
  (forall bs. Delete as bs x -> r) ->
  r
withDelete = \case
  IZ -> \f -> f DelZ
  IS i -> \f -> withDelete i (f . DelS)

-- | Given an 'Index' pointing to an element, create an 'Insert' placing an
-- item /directly before/ the given element.  The type is existentailly
-- quantified.
withInsert ::
  Index as x ->
  (forall bs. Insert as bs y -> r) ->
  r
withInsert = \case
  IZ -> \f -> f InsZ
  IS i -> \f -> withInsert i (f . InsS)

-- | Given an 'Index' pointing to an element, create an 'Insert' placing an
-- item /directly after/ the given element.  The type is existentailly
-- quantified.
withInsertAfter ::
  Index as x ->
  (forall bs. Insert as bs y -> r) ->
  r
withInsertAfter = \case
  IZ -> \f -> f (InsS InsZ)
  IS i -> \f -> withInsertAfter i (f . InsS)

-- | Type-level version of 'insertIndex'.  Because of how GADTs and type
-- families interact, the type-level lists and kinds of the insertion and
-- index must be provided.
type family
  InsertIndex (as :: [k]) (bs :: [k]) (x :: k) (y :: k) (ins :: Insert as bs x) (i :: Index as y) ::
    Index bs y
  where
  InsertIndex as (x ': as) x y 'InsZ i = 'IS i
  InsertIndex (y ': as) (y ': bs) x y ('InsS ins) 'IZ = 'IZ
  InsertIndex (a ': as) (a ': bs) x y ('InsS ins) ('IS i) = 'IS (InsertIndex as bs x y ins i)

-- | Defunctionalization symbol for 'InsertIndex', expecting only the kind
-- variables.
data InsertIndexSym0 as bs x y :: Insert as bs x ~> Index as y ~> Index bs y

-- | Defunctionalization symbol for 'InsertIndex', expecting the 'Insert'
-- along with the kind variables.
data InsertIndexSym as bs x y :: Insert as bs x -> Index as y ~> Index bs y

type instance Apply (InsertIndexSym0 as bs x y) ins = InsertIndexSym as bs x y ins
type instance Apply (InsertIndexSym as bs x y ins) i = InsertIndex as bs x y ins i

-- | Singleton witness for 'InsertIndex'.
sInsertIndex ::
  SInsert as bs x ins ->
  SIndex as y i ->
  SIndex bs y (InsertIndex as bs x y ins i)
sInsertIndex = \case
  SInsZ -> SIS
  SInsS ins -> \case
    SIZ -> SIZ
    SIS i -> SIS (sInsertIndex ins i)

-- | Helper type family for the implementation of 'DeleteIndex', to get
-- around the lack of case statements at the type level.
type family
  SuccDeletedIx (b :: k) (bs :: [k]) (x :: k) (y :: k) (del :: DeletedIx bs x y) ::
    DeletedIx (b ': bs) x y
  where
  SuccDeletedIx b bs x x 'GotDeleted = 'GotDeleted
  SuccDeletedIx b bs x y ('NotDeleted i) = 'NotDeleted ('IS i)

-- | Type-level version of 'deleteIndex'.  Because of how GADTs and type
-- families interact, the type-level lists and kinds of the insertion and
-- index must be provided.
type family
  DeleteIndex (as :: [k]) (bs :: [k]) (x :: k) (y :: k) (del :: Delete as bs x) (i :: Index as y) ::
    DeletedIx bs x y
  where
  DeleteIndex (x ': bs) bs x x 'DelZ 'IZ = 'GotDeleted
  DeleteIndex (x ': bs) bs x y 'DelZ ('IS i) = 'NotDeleted i
  DeleteIndex (y ': as) (y ': bs) x y ('DelS del) 'IZ = 'NotDeleted 'IZ
  DeleteIndex (b ': as) (b ': bs) x y ('DelS del) ('IS i) =
    SuccDeletedIx b bs x y (DeleteIndex as bs x y del i)

-- | Defunctionalization symbol for 'DeleteIndex', expecting only the kind
-- variables.
data DeleteIndexSym0 as bs x y :: Delete as bs x ~> Index as y ~> DeletedIx bs x y

-- | Defunctionalization symbol for 'DeleteIndex', expecting the 'Delete'
-- along with the kind variables.
data DeleteIndexSym as bs x y :: Delete as bs x -> Index as y ~> DeletedIx bs x y

type instance Apply (DeleteIndexSym0 as bs x y) del = DeleteIndexSym as bs x y del
type instance Apply (DeleteIndexSym as bs x y del) i = DeleteIndex as bs x y del i

-- | Kind-indexed singleton for 'DeletedIx'.
data SDeletedIx (bs :: [k]) (x :: k) (y :: k) :: DeletedIx bs x y -> Type where
  SGotDeleted :: SDeletedIx bs x x 'GotDeleted
  SNotDeleted :: SIndex bs y i -> SDeletedIx bs x y ('NotDeleted i)

-- | Singleton witness for 'DeleteIndex'.
sDeleteIndex ::
  SDelete as bs x del ->
  SIndex as y i ->
  SDeletedIx bs x y (DeleteIndex as bs x y del i)
sDeleteIndex = \case
  SDelZ -> \case
    SIZ -> SGotDeleted
    SIS i -> SNotDeleted i
  SDelS del -> \case
    SIZ -> SNotDeleted SIZ
    SIS i -> case sDeleteIndex del i of
      SGotDeleted -> SGotDeleted
      SNotDeleted j -> SNotDeleted (SIS j)

-- | Helper type family for the implementation of 'SubstituteIndex', to get
-- around the lack of case statements at the type level.
type family
  SuccSubstitutedIx (b :: k) (bs :: [k]) (x :: k) (y :: k) (z :: k) (s :: SubstitutedIx bs x y z) ::
    SubstitutedIx (b ': bs) x y z
  where
  SuccSubstitutedIx b bs x y x ('GotSubbed i) = 'GotSubbed ('IS i)
  SuccSubstitutedIx b bs x y z ('NotSubbed i) = 'NotSubbed ('IS i)

-- | Type-level version of 'substituteIndex'.  Because of how GADTs and
-- type families interact, the type-level lists and kinds of the insertion
-- and index must be provided.
type family
  SubstituteIndex
    (as :: [k])
    (bs :: [k])
    (x :: k)
    (y :: k)
    (z :: k)
    (s :: Substitute as bs x y)
    (i :: Index as z) ::
    SubstitutedIx bs x y z
  where
  SubstituteIndex (z ': as) (y ': as) z y z 'SubZ 'IZ = 'GotSubbed 'IZ
  SubstituteIndex (x ': as) (y ': as) x y z 'SubZ ('IS i) = 'NotSubbed ('IS i)
  SubstituteIndex (z ': as) (z ': bs) x y z ('SubS s) 'IZ = 'NotSubbed 'IZ
  SubstituteIndex (b ': as) (b ': bs) x y z ('SubS s) ('IS i) =
    SuccSubstitutedIx b bs x y z (SubstituteIndex as bs x y z s i)

-- | Defunctionalization symbol for 'SubstituteIndex', expecting only the kind
-- variables.
data SubstituteIndexSym0 as bs x y z :: Substitute as bs x y ~> Index as z ~> SubstitutedIx bs x y z

-- | Defunctionalization symbol for 'SubstituteIndex', expecting the 'Substitute'
-- along with the kind variables.
data SubstituteIndexSym as bs x y z :: Substitute as bs x y -> Index as z ~> SubstitutedIx bs x y z

type instance Apply (SubstituteIndexSym0 as bs x y z) s = SubstituteIndexSym as bs x y z s
type instance Apply (SubstituteIndexSym as bs x y z s) i = SubstituteIndex as bs x y z s i

-- | Kind-indexed singleton for 'SubstitutedIx'.
data SSubstitutedIx (bs :: [k]) (x :: k) (y :: k) (z :: k) :: SubstitutedIx bs x y z -> Type where
  SGotSubbed :: SIndex bs y i -> SSubstitutedIx bs z y z ('GotSubbed i)
  SNotSubbed :: SIndex bs z i -> SSubstitutedIx bs x y z ('NotSubbed i)

-- | Singleton witness for 'SubstituteIndex'.
sSubstituteIndex ::
  SSubstitute as bs x y s ->
  SIndex as z i ->
  SSubstitutedIx bs x y z (SubstituteIndex as bs x y z s i)
sSubstituteIndex = \case
  SSubZ -> \case
    SIZ -> SGotSubbed SIZ
    SIS i -> SNotSubbed (SIS i)
  SSubS s -> \case
    SIZ -> SNotSubbed SIZ
    SIS i -> case sSubstituteIndex s i of
      SGotSubbed j -> SGotSubbed (SIS j)
      SNotSubbed j -> SNotSubbed (SIS j)
