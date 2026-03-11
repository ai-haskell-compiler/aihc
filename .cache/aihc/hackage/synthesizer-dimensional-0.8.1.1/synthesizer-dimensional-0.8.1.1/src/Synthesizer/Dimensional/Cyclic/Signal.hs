{- |
Copyright   :  (c) Henning Thielemann 2008-2011
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Treat a signal as period of a cyclic signal.

ToDo:
In principle this module does no longer belong to dimensional package
but could be moved to synthesizer-core.
-}
module Synthesizer.Dimensional.Cyclic.Signal where

-- import qualified Synthesizer.Format as Format

import qualified Synthesizer.Generic.Cyclic as Cyclic
import qualified Synthesizer.Generic.Signal as SigG
-- import qualified Synthesizer.State.Signal as Sig

{-
import qualified Algebra.Module         as Module
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
-}
import qualified Algebra.Additive       as Additive

import Data.Monoid (Monoid, )


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


newtype T period =
   Cons {
      toPeriod :: period   {-^ the sampled values -}
   }
   deriving (Eq, Show)

{-
instance Functor seq => Functor (T seq) where
   fmap f = Cons . fmap f . samples

instance Format.C seq => Format.C (T seq) where
   format p = Format.format p . samples

instance (Format.C seq, Show y) => Show (T seq y) where
   showsPrec = Format.format


type R s yv = RP.T s (T Sig.T) yv
-}


{-
replacePeriod :: Sig.T yv1 -> R s yv0 -> R s yv1
replacePeriod ss _  =  fromPeriod ss
-}

processPeriod ::
   (body0 -> body1) -> T body0 -> T body1
processPeriod f =
   fromPeriod . f . toPeriod


{-# INLINE fromPeriod #-}
fromPeriod :: body -> T body
fromPeriod  =  Cons


{- |
Periodization of a straight signal.
-}
{-# INLINE fromSignal #-}
fromSignal ::
   (Additive.C yv, SigG.Write sig yv) =>
   Int -> sig yv -> T (sig yv)
fromSignal n  =
   fromPeriod . Cyclic.fromSignal SigG.defaultLazySize n

{- |
Convert a cyclic signal to a straight signal containing a loop.
-}
{-# INLINE toSignal #-}
toSignal :: (Monoid sig) => T sig -> sig
toSignal  =
   SigG.cycle . toPeriod
