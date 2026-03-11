module Singleraeh.Either where

import Singleraeh.Demote
import Data.Kind ( Type, Constraint )

-- | Singleton 'Either'.
type SEither :: (l -> Type) -> (r -> Type) -> Either l r -> Type
data SEither sl sr elr where
    SLeft  :: sl l -> SEither sl sr (Left  l)
    SRight :: sr r -> SEither sl sr (Right r)

demoteSEither
    :: forall dl dr sl sr elr
    .  (forall l. sl l -> dl)
    -> (forall r. sr r -> dr)
    -> SEither sl sr elr
    -> Either dl dr
demoteSEither demoteSL demoteSR = \case
  SLeft  sl -> Left  $ demoteSL sl
  SRight sr -> Right $ demoteSR sr

class SingEither (cl :: lk -> Constraint) (cr :: rk -> Constraint) (sl :: lk -> Type) (sr :: rk -> Type) (elr :: Either lk rk) where
    singEither'
        :: (forall l. cl l => sl l)
        -> (forall r. cr r => sr r)
        -> SEither sl sr elr

-- reordered types (hidden lk, rk because implied)
singEither
    :: forall cl cr sl sr elr. SingEither cl cr sl sr elr
    => (forall l. cl l => sl l)
    -> (forall r. cr r => sr r)
    -> SEither sl sr elr
singEither = singEither' @_ @_ @cl @cr

instance cl l => SingEither cl cr sl sr (Left  l) where
    singEither'  singL _singR = SLeft  singL
instance cr r => SingEither cl cr sl sr (Right r) where
    singEither' _singL  singR = SRight singR

instance (Demotable sl, Demotable sr) => Demotable (SEither sl sr) where
    type Demote (SEither sl sr) = Either (Demote sl) (Demote sr)
    demote = demoteSEither demote demote
