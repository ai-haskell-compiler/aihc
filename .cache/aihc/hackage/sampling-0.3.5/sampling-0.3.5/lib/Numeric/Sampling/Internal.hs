{-# LANGUAGE FlexibleContexts #-}

-- Much of the code in this module is a modification of that found in the
-- 'foldl' library by Gabriel Gonzalez.  Its license is reproduced below.

-- Copyright (c) 2013 Gabriel Gonzalez
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without modification,
-- are permitted provided that the following conditions are met:
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright notice,
--       this list of conditions and the following disclaimer in the documentation
--       and/or other materials provided with the distribution.
--     * Neither the name of Gabriel Gonzalez nor the names of other contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
-- (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
-- LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
-- ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Numeric.Sampling.Internal (
    randomN
  ) where

import           Control.Foldl               (FoldM (..))
import           Control.Monad               (when)
import           Control.Monad.Primitive
import           Data.Vector.Generic         (Mutable, Vector)
import qualified Data.Vector.Generic         as V
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as M
import           System.Random.MWC

data VectorState = Incomplete {-# UNPACK #-} !Int | Complete

data RandomNState m v a = RandomNState
    { _size      ::                !VectorState
    , _reservoir ::                !(Mutable v (PrimState m) a)
    , _position  :: {-# UNPACK #-} !Int
    , _gen       ::                !(Gen (PrimState m))
    }

randomN
  :: (PrimMonad m, Vector v a)
  => Int -> Gen (PrimState m) -> FoldM m a (Maybe (v a))
randomN n gen = FoldM step begin done where
  step
      :: (PrimMonad m, MVector (Mutable v) a)
      => RandomNState m v a -> a -> m (RandomNState m v a)
  step (RandomNState (Incomplete m) mv i g) a = do
      M.write mv m a
      let m' = m + 1
      let s  = if n <= m' then Complete else Incomplete m'
      return $! RandomNState s mv (i + 1) g

  step (RandomNState  Complete      mv i g) a = do
      r <- uniformR (0, i - 1) g
      when (r < n) (M.unsafeWrite mv r a)
      return (RandomNState Complete mv (i + 1) g)

  begin = do
      mv  <- M.new n
      let s = if n <= 0 then Complete else Incomplete 0
      return (RandomNState s mv 1 gen)

  done :: (PrimMonad m, Vector v a) => RandomNState m v a -> m (Maybe (v a))
  done (RandomNState (Incomplete _) _  _ _) = return Nothing
  done (RandomNState  Complete      mv _ _) = do
      v <- V.freeze mv
      return (Just v)
{-# INLINABLE randomN #-}

