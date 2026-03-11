-- {-# LANGUAGE UndecidableInstances #-}

module Generic.Type.Example where

import DeFun.Core ( type (~>), type App )
import GHC.Generics ( Generic )
import GHC.TypeNats ( type (+), type Natural )

type FieldCountSym :: a ~> Natural
data FieldCountSym a
type instance App FieldCountSym a = 1

type PlusSym :: Natural ~> Natural ~> Natural
data PlusSym f
type instance App PlusSym f = PlusSym1 f

type PlusSym1 :: Natural -> Natural ~> Natural
data PlusSym1 l r
type instance App (PlusSym1 l) r = l + r

data X3Fields = X3Fields { x3Fields1 :: (), x3Fields2 :: (), x3Fields3 :: () }
    deriving stock Generic

-- > k! GTFoldMapC PlusSym 5 FieldCountSym (Rep X3Fields) = 3
