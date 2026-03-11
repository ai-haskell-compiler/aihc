module Singleraeh.Tuple where

import Singleraeh.Demote
import Data.Kind ( Type, Constraint )

data SUnit (unit :: ()) where SUnit :: SUnit '()

demoteSUnit :: SUnit unit -> ()
demoteSUnit SUnit = ()

instance Demotable SUnit where
    type Demote SUnit = ()
    demote = demoteSUnit

type STuple2 :: (a -> Type) -> (b -> Type) -> (a, b) -> Type
data STuple2 sa sb ab where
    STuple2 :: sa a -> sb b -> STuple2 sa sb '(a, b)

demoteSTuple2
    :: forall da db sa sb ab
    .  (forall a. sa a -> da)
    -> (forall b. sb b -> db)
    -> STuple2 sa sb ab
    -> (da, db)
demoteSTuple2 demoteSA demoteSB (STuple2 sa sb) = (demoteSA sa, demoteSB sb)

instance (Demotable sa, Demotable sb) => Demotable (STuple2 sa sb) where
    type Demote (STuple2 sa sb) = (Demote sa, Demote sb)
    demote = demoteSTuple2 demote demote

class SingTuple2 (cl :: lk -> Constraint) (cr :: rk -> Constraint) (sl :: lk -> Type) (sr :: rk -> Type) (lr :: (lk, rk)) where
    singTuple2'
        :: (forall l. cl l => sl l)
        -> (forall r. cr r => sr r)
        -> STuple2 sl sr lr

singTuple2
    :: forall cl cr sl sr lr. SingTuple2 cl cr sl sr lr
    => (forall l. cl l => sl l)
    -> (forall r. cr r => sr r)
    -> STuple2 sl sr lr
singTuple2 = singTuple2' @_ @_ @cl @cr

instance (cl l, cr r) => SingTuple2 cl cr sl sr '(l, r) where
    singTuple2' sl sr = STuple2 sl sr

type STuple3 :: (a -> Type) -> (b -> Type) -> (c -> Type) -> (a, b, c) -> Type
data STuple3 sa sb sc abc where
    STuple3 :: sa a -> sb b -> sc c -> STuple3 sa sb sc '(a, b, c)

demoteSTuple3
    :: forall da db dc sa sb sc abc
    .  (forall a. sa a -> da)
    -> (forall b. sb b -> db)
    -> (forall c. sc c -> dc)
    -> STuple3 sa sb sc abc
    -> (da, db, dc)
demoteSTuple3 demoteSA demoteSB demoteSC (STuple3 sa sb sc) =
    (demoteSA sa, demoteSB sb, demoteSC sc)

instance (Demotable sa, Demotable sb, Demotable sc)
  => Demotable (STuple3 sa sb sc) where
    type Demote (STuple3 sa sb sc) = (Demote sa, Demote sb, Demote sc)
    demote = demoteSTuple3 demote demote demote

class SingTuple3 (ca :: ak -> Constraint) (cb :: bk -> Constraint) (cc :: ck -> Constraint) (sa :: ak -> Type) (sb :: bk -> Type) (sc :: ck -> Type) (abc :: (ak, bk, ck)) where
    singTuple3'
        :: (forall a. ca a => sa a)
        -> (forall b. cb b => sb b)
        -> (forall c. cc c => sc c)
        -> STuple3 sa sb sc abc

singTuple3
    :: forall ca cb cc sa sb sc abc. SingTuple3 ca cb cc sa sb sc abc
    => (forall a. ca a => sa a)
    -> (forall b. cb b => sb b)
    -> (forall c. cc c => sc c)
    -> STuple3 sa sb sc abc
singTuple3 = singTuple3' @_ @_ @_ @ca @cb @cc

instance (ca a, cb b, cc c) => SingTuple3 ca cb cc sa sb sc '(a, b, c) where
    singTuple3' sa sb sc = STuple3 sa sb sc
