{-# LANGUAGE RebindableSyntax #-}
module Algebra.RealTranscendental where

import qualified Algebra.Transcendental      as Trans
import qualified Algebra.RealField           as RealField

import Algebra.Transcendental (atan, pi)
import Algebra.Field          ((/))
import Algebra.Ring           (fromInteger)
import Algebra.Additive       ((+), negate)

import Data.Bool.HT (select, )

import qualified Prelude as P
import NumericPrelude.Base



{-|
This class collects all functions for _scalar_ floating point numbers.
E.g. computing 'atan2' for complex floating numbers makes certainly no sense.
-}
class (RealField.C a, Trans.C a) => C a where
    atan2 :: a -> a -> a

    atan2 y x = select 0   -- must be after the other double zero tests
      [(x>0,          atan (y/x)),
       (x==0 && y>0,  pi/2),
       (x<0  && y>0,  pi + atan (y/x)),
       (x<=0 && y<0, -atan2 (-y) x),
       (y==0 && x<0,  pi)] -- must be after the previous test on zero y

instance C P.Float where
    atan2 = P.atan2

instance C P.Double where
    atan2 = P.atan2
