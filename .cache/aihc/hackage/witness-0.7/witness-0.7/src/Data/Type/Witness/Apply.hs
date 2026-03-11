module Data.Type.Witness.Apply where

import Import

applyRefl ::
       forall k1 k2 (fa :: k1 -> k2) (fb :: k1 -> k2) (a :: k1) (b :: k1). fa :~: fb -> a :~: b -> (fa a) :~: (fb b)
applyRefl Refl Refl = Refl

withRefl :: forall k (a :: k) (b :: k) (r :: Type). a :~: b -> (a ~ b => r) -> r
withRefl Refl r = r

reflId ::
       forall k (cat :: k -> k -> Type) (a :: k) (b :: k). Category cat
    => a :~: b -> cat a b
reflId Refl = id

reflId1 ::
       forall kq (cat :: kq -> kq -> Type) kp (w :: kp -> kq) (a :: kp) (b :: kp). Category cat
    => a :~: b -> cat (w a) (w b)
reflId1 Refl = id
