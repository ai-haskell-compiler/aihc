{-# LANGUAGE UndecidableInstances #-}

module Singleraeh.List where

import Data.Kind ( Type )
import Singleraeh.Demote

-- | Singleton list.
type SList :: (a -> Type) -> [a] -> Type
data SList sa as where
    SCons :: sa a -> SList sa as -> SList sa (a : as)
    SNil  ::                        SList sa '[]

demoteSList
    :: forall da sa as
    .  (forall a. sa a -> da)
    -> SList sa as
    -> [da]
demoteSList demoteSA = \case
  SCons sa sas -> demoteSA sa : demoteSList demoteSA sas
  SNil         -> []

instance Demotable sa => Demotable (SList sa) where
    type Demote (SList sa) = [Demote sa]
    demote = demoteSList demote

-- | Reverse a type level list.
type Reverse as = Reverse' '[] as
type family Reverse' (acc :: [k]) (as :: [k]) :: [k] where
  Reverse' acc (a : as) = Reverse' (a : acc) as
  Reverse' acc '[]      = acc

sReverse :: SList sa as -> SList sa (Reverse as)
sReverse as = sReverse' SNil as

sReverse' :: SList sa acc -> SList sa as -> SList sa (Reverse' acc as)
sReverse' acc = \case
  SCons a as -> sReverse' (SCons a acc) as
  SNil       -> acc
