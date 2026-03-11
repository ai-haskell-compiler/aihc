module Synthesizer.Plain.Builder (
   T, Put, put, run,
   signalToBinary, signalToBinaryMono, signalToBinaryStereo,
   ) where

import qualified Synthesizer.Basic.Binary as BinSmp

import Data.Monoid (Monoid, mempty, mappend, mconcat, Endo(Endo), appEndo, )
import Data.Semigroup (Semigroup, (<>), )

import qualified Algebra.FloatingPoint as Float
import qualified Algebra.ToInteger as ToInteger

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



newtype T a = Cons {decons :: Endo [a]}

type Put a = a -> T a


instance Semigroup (T a) where
   x <> y = Cons $ decons x <> decons y

instance Monoid (T a) where
   mempty = Cons mempty
   mappend = (<>)

put :: Put a
put = Cons . Endo . (:)

run :: T a -> [a]
run = flip appEndo [] . decons


{-# INLINE signalToBinary #-}
signalToBinary ::
   (BinSmp.C v, ToInteger.C int, Bounded int) =>
   [v] -> [int]
signalToBinary =
   run . mconcat . map (BinSmp.outputFromCanonical put)

{-# INLINE signalToBinaryMono #-}
signalToBinaryMono ::
   (Float.C a, ToInteger.C int, Bounded int) =>
   [a] -> [int]
signalToBinaryMono =
   map (BinSmp.fromCanonicalWith round)

{-# INLINE signalToBinaryStereo #-}
signalToBinaryStereo ::
   (Float.C a, ToInteger.C int, Bounded int) =>
   [(a,a)] -> [int]
signalToBinaryStereo =
   concatMap (\(l,r) ->
      [BinSmp.fromCanonicalWith round l,
       BinSmp.fromCanonicalWith round r])
