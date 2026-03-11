module Data.Type.Witness.General.AllConstraint where

import Import

type AllConstraint :: forall kw kt. (kw -> Constraint) -> (kt -> kw) -> Constraint
class AllConstraint c w where
    allConstraint :: forall t. Dict (c (w t))

withAllConstraint ::
       forall k (c :: Type -> Constraint) (w :: k -> Type) (a :: k) (r :: Type). AllConstraint c w
    => w a
    -> (c (w a) => r)
    -> r
withAllConstraint _ call =
    case allConstraint @Type @k @c @w @a of
        Dict -> call

instance AllConstraint Show ((:~:) t) where
    allConstraint = Dict

allShow ::
       forall k (w :: k -> Type) (t :: k). AllConstraint Show w
    => w t
    -> String
allShow wt =
    case allConstraint @_ @_ @Show @w @t of
        Dict -> show wt
