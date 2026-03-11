{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Synthesizer.ChunkySize.Signal where

import qualified Synthesizer.ChunkySize.Cut as Cut
import qualified Synthesizer.ChunkySize as ChunkySize

import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.State.Signal as SigS

import qualified Data.StorableVector.Lazy.Pattern as SigStV
import qualified Data.StorableVector.Lazy as Vector
import Foreign.Storable (Storable)

import Control.Monad.Trans.State (runStateT, )

import qualified Data.List.Match as Match
import qualified Data.List as List

import Prelude (Maybe(Just), fst, (.), id, )


class (SigG.Write sig y, Cut.Transform (sig y)) => Write sig y where
   unfoldRN :: ChunkySize.T -> (s -> Maybe (y,s)) -> s -> sig y


instance Storable y => Write Vector.Vector y where
   {-# INLINE unfoldRN #-}
   unfoldRN size f =
      fst .
      SigStV.unfoldrN
         (ChunkySize.toStorableVectorSize size) f

instance Write [] y where
   {-# INLINE unfoldRN #-}
   unfoldRN size f =
      Match.take (ChunkySize.toNullList size) .
      List.unfoldr f

instance Write SigS.T y where
   {-# INLINE unfoldRN #-}
   unfoldRN size f =
      Cut.take size . SigS.unfoldR f


{-# INLINE replicate #-}
replicate :: (Write sig y) =>
   ChunkySize.T -> y -> sig y
replicate = iterateN id

{-# INLINE iterateN #-}
iterateN :: (Write sig y) =>
   (y -> y) -> ChunkySize.T -> y -> sig y
iterateN f size =
   unfoldRN size (\y -> Just (y, f y))

{-# INLINE fromState #-}
fromState :: (Write sig y) =>
   ChunkySize.T -> SigS.T y -> sig y
fromState size (SigS.Cons f x) =
   unfoldRN size (runStateT f) x
