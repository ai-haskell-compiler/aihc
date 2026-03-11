{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
module Data.Matrix.Unboxed.Mutable
    ( -- * Mutable Matrix
      MMatrix
    , dim
    , takeRow
    , write
    , unsafeWrite
    , read
    , unsafeRead
    , new
    , replicate
    ) where

import GHC.Exts (Constraint)
import Prelude hiding (read, replicate)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import Data.Vector.Unboxed.Mutable (MVector, Unbox)

import qualified Data.Matrix.Generic.Mutable as MG

type MMatrix a = MG.MMatrix MVector a
type Context x = (Unbox x :: Constraint)

dim :: Context a => MMatrix s a -> (Int, Int)
dim = MG.dim

takeRow :: Context a => MMatrix s a -> Int -> MVector s a
takeRow = MG.takeRow

write :: Context a => PrimMonad s => MMatrix (PrimState s) a -> (Int, Int) -> a -> s ()
write = MG.write

unsafeWrite :: (Context a, PrimMonad s) => MMatrix (PrimState s) a -> (Int, Int) -> a -> s ()
unsafeWrite = MG.unsafeWrite

read :: (Context a, PrimMonad s) => MMatrix (PrimState s) a -> (Int, Int) -> s a
read = MG.read

unsafeRead :: (Context a, PrimMonad s) => MMatrix (PrimState s) a -> (Int, Int) -> s a
unsafeRead = MG.unsafeRead

-- | Create a mutable matrix without initialization
new :: (Context a, PrimMonad s) => (Int, Int) -> s (MMatrix (PrimState s) a)
new = MG.new

replicate :: (Context a, PrimMonad s) => (Int, Int) -> a -> s (MMatrix (PrimState s) a)
replicate = MG.replicate