{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Plain.Filter.Recursive where

import qualified Algebra.Module                as Module
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Various Filters -}


{- ** Recursive filters with resonance -}

{-| Description of a filter pole. -}
data Pole a =
    Pole {poleResonance :: !a  {- ^ Resonance, that is the amplification of the band center frequency. -}
        , poleFrequency :: !a  {- ^ Band center frequency. -} }
    deriving (Eq, Show, Read)

instance Additive.C v => Additive.C (Pole v) where
   zero = Pole zero zero
   (+) (Pole yr yf) (Pole xr xf) = Pole (yr + xr) (yf + xf)
   (-) (Pole yr yf) (Pole xr xf) = Pole (yr - xr) (yf - xf)
   negate           (Pole xr xf) = Pole (negate xr) (negate xf)

{-
An instance for Module.C of the Pole datatype
makes no sense in most cases,
but when it comes to interpolation
this is very handy.
-}
instance Module.C a v => Module.C a (Pole v) where
   s *> (Pole xr xf) = Pole (s *> xr) (s *> xf)


data Passband = Lowpass | Highpass
       deriving (Show, Eq, Enum)
