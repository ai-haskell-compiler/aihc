module Bytezap.Common.Generic where

import GHC.TypeNats
import DeFun.Core ( type (~>), type App )
import Generic.Type.Function.FoldMap ( type GTFoldMapC )

type PlusSym :: Natural ~> Natural ~> Natural
data PlusSym f
type instance App PlusSym f = PlusSym1 f

type PlusSym1 :: Natural -> Natural ~> Natural
data PlusSym1 l r
type instance App (PlusSym1 l) r = l + r

-- | Generic type 'foldMap' using the addition monoid.
type GTFoldMapCAddition f gf = GTFoldMapC PlusSym 0 f gf
