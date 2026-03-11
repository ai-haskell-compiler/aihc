module Singleraeh.Demote where

import Data.Kind ( Type, Constraint )
import GHC.TypeLits
import GHC.TypeNats qualified as TN

-- | Singleton types which may be demoted.
type Demotable :: (k -> Type) -> Constraint
class Demotable sk where
    -- | Demoted type.
    type Demote sk :: Type

    -- | Demote a term of the singleton @sk@.
    demote :: forall k. sk k -> Demote sk

instance Demotable SNat where
    type Demote SNat = Natural
    demote = TN.fromSNat

instance Demotable SSymbol where
    type Demote SSymbol = String
    demote = fromSSymbol

instance Demotable SChar where
    type Demote SChar = Char
    demote = fromSChar
