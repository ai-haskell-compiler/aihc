{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Causal.Cut where

import qualified Synthesizer.Causal.Process as Causal

import Control.Monad.Trans.State (StateT(StateT), )

import Data.Maybe.HT (toMaybe, )

import NumericPrelude.Numeric
import NumericPrelude.Base


{-# INLINE take #-}
take :: Int -> Causal.T a a
take =
   Causal.fromStateMaybe (\x ->
      StateT (\i -> toMaybe (i>0) (x, pred i)))
