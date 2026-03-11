{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Packed.Repa
-- Copyright   :  (c) Alexander Vivian Hugh McPhail 2011, 2015
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  portable
--
-- Repa / hmatrix conversion functions
--
-----------------------------------------------------------------------------

module Data.Packed.Repa (
                         vectorToRepa
                         , repaToVector
                         , matrixToRepa
                         , repaToMatrix
                         ) where

import qualified Numeric.LinearAlgebra      as H
import qualified Numeric.LinearAlgebra.Data as HV
import qualified Numeric.LinearAlgebra.Data as HM

import Foreign.Storable

import Data.Vector.Storable
import qualified Data.Vector.Generic as GV

import qualified Data.Array.Repa as RA
import qualified Data.Array.Repa.Repr.Vector as RV

-- | convert a Storable vector to a DIM1 repa array
vectorToRepa :: (Storable e, GV.Vector HV.Vector e, H.Container Vector e)
             => HV.Vector e
             -> RV.Array RV.V RA.DIM1 e
vectorToRepa v = let ln = HV.size v
                 in RV.fromVector (RA.Z RA.:. ln) $ convert v

-- XXX: note that this type could be made shape polymorhphic.
-- | convert a 1d repa array to a Storable vector
repaToVector :: (GV.Vector HV.Vector e)
             => RV.Array RV.V RA.DIM1 e -> HV.Vector e
repaToVector = convert . RV.toVector

-- | convert a Storable matrix to a DIM2 repa array
matrixToRepa :: (H.Element e, GV.Vector HV.Vector e)
             => HM.Matrix e
             -> RV.Array RV.V RA.DIM2 e
matrixToRepa m = let (r,c) = (HM.rows m,HM.cols m) 
                 in RV.fromVector (RA.Z RA.:. r RA.:. c) $ convert $ HM.flatten m

-- XXX: note that this could be made shape polymorphic
-- | convert a 2d repa array to a Storable matrix
repaToMatrix :: (Storable e, GV.Vector HV.Vector e)
             => RV.Array RV.V RA.DIM2 e
             -> HM.Matrix e
repaToMatrix a = let (RA.Z RA.:. _ RA.:. c) = RA.extent a
                 in HM.reshape c $ convert $ RV.toVector a
