{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.Generic.CutChunky where

import qualified Synthesizer.Generic.Cut as Cut

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import qualified Number.NonNegativeChunky as Chunky
import qualified Algebra.NonNegative as NonNeg
import qualified Algebra.ToInteger as ToInteger



class (Cut.Transform chunky, Cut.Transform (Chunk chunky)) => C chunky where
   type Chunk chunky :: *
   fromChunks :: [Chunk chunky] -> chunky
   toChunks :: chunky -> [Chunk chunky]

instance Storable a => C (SVL.Vector a) where
   type Chunk (SVL.Vector a) = SV.Vector a
   fromChunks = SVL.fromChunks
   toChunks = SVL.chunks

instance (ToInteger.C a, NonNeg.C a, Cut.Transform a) => C (Chunky.T a) where
   type Chunk (Chunky.T a) = a
   fromChunks = Chunky.fromChunks
   toChunks = Chunky.toChunks
