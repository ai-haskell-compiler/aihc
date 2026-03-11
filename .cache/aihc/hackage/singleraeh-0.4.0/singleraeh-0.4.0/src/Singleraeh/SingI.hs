{-# LANGUAGE UndecidableInstances #-} -- for recursive SingI constraints

-- Orphan instances when we define them elsewhere, so here they shall stay.

module Singleraeh.SingI where

import Data.Kind ( Type )
import GHC.TypeLits

import Singleraeh.Bool
import Singleraeh.Tuple
import Singleraeh.Either
import Singleraeh.Maybe

class SingI (a :: k) where
    type Sing :: k -> Type
    sing' :: Sing a

sing :: forall {k} (a :: k). SingI a => Sing a
sing = sing' @_ @a

instance KnownNat n => SingI n where
    type Sing = SNat
    sing' = SNat

instance KnownSymbol str => SingI str where
    type Sing = SSymbol
    sing' = SSymbol

instance KnownChar ch => SingI ch where
    type Sing = SChar
    sing' = SChar

instance SingBool b => SingI b where
    type Sing = SBool
    sing' = singBool

instance SingTuple2 SingI SingI Sing Sing lr => SingI lr where
    type Sing = STuple2 Sing Sing
    sing' = singTuple2 @SingI @SingI sing sing

instance SingEither SingI SingI Sing Sing elr => SingI elr where
    type Sing = SEither Sing Sing
    sing' = singEither @SingI @SingI sing sing

instance SingMaybe SingI Sing ma => SingI ma where
    type Sing = SMaybe Sing
    sing' = singMaybe @SingI sing
