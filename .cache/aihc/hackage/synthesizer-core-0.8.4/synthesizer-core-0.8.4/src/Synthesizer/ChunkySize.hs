module Synthesizer.ChunkySize where

import qualified Synthesizer.Generic.Signal as SigG
import qualified Number.NonNegativeChunky as Chunky
import qualified Numeric.NonNegative.Chunky as Chunky98

import qualified Data.StorableVector.Lazy as SigSt
import qualified Data.StorableVector.Lazy.Pattern as SigStV

import qualified Data.List as List


type T = Chunky.T SigG.LazySize


fromStorableVectorSize ::
   SigStV.LazySize -> T
fromStorableVectorSize =
   Chunky.fromChunks .
   List.map (\(SigSt.ChunkSize size) -> (SigG.LazySize size)) .
   Chunky98.toChunks

toStorableVectorSize ::
   T -> SigStV.LazySize
toStorableVectorSize =
   Chunky98.fromChunks .
   List.map (\(SigG.LazySize size) -> (SigSt.ChunkSize size)) .
   Chunky.toChunks

toNullList :: T -> [()]
toNullList =
   List.concatMap (\(SigG.LazySize n) -> List.replicate n ()) .
   Chunky.toChunks
