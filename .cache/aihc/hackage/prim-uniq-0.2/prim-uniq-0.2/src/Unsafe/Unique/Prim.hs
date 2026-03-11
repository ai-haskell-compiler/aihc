{-# LANGUAGE BangPatterns, FlexibleInstances #-}
module Unsafe.Unique.Prim
    ( Uniq, getUniq
    , unsafeMkUniq, unsafeShowsPrecUniq, unsafeShowUniq
    ) where

import Control.Monad.Primitive
import Data.IORef
import System.IO.Unsafe

-- A smaller numeric type could be used, such as Word or Word64, but I
-- want to be able to guarantee uniqueness even over very long execution 
-- times.  Smaller types would require either checking for overflow,
-- accepting the possibility of aliasing, or tracking allocation and 
-- deallocation, which would be a lot of extra work.  Word64 is almost
-- certainly big enough for practical purposes, though.  Allocating one 
-- 'Uniq' every nanosecond, it would take 584 years to start aliasing....
-- So, in the future I may choose to switch to Word64.

-- |A 'Uniq' is a value that can only be constructed under controlled 
-- conditions (in IO or ST, basically), and once constructed can only be
-- compared to 'Uniq' values created under the same conditions (in the same
-- monad).  Upon comparison, a 'Uniq' is ONLY ever equal to itself.  Beyond
-- that, no promises regarding ordering are made except that once constructed
-- the order is deterministic and a proper ordering relation (eg, > is 
-- transitive and irreflexive, etc.)
newtype Uniq s = Uniq Integer deriving (Eq, Ord)

-- |There is only one 'RealWorld', so this instance is sound (unlike the 
-- general 'unsafeShowsPrecUniq').  Note that there is no particular
-- relationship between 'Uniq' values (or the strings 'show' turns them into)
-- created in different executions of a program.  The value they render to
-- should be considered completely arbitrary, and the Show instance only even
-- exists for convenience when testing code that uses 'Uniq's.
instance Show (Uniq RealWorld) where
    showsPrec = unsafeShowsPrecUniq

{-# NOINLINE nextUniq #-}
-- | [internal] Assuming the compiler behaves "as expected", this is a single
-- statically-created IORef holding the counter which will be used as the 
-- source of new 'Prim' keys (in 'ST' and 'IO').
nextUniq :: IORef Integer
nextUniq = unsafePerformIO (newIORef 0)

-- |Construct a new 'Uniq' that is equal to itself, unequal to every other
-- 'Uniq' constructed in the same monad, and incomparable to every 'Uniq' 
-- constructed in any other monad.
getUniq :: PrimMonad m => m (Uniq (PrimState m))
getUniq = unsafePrimToPrim (atomicModifyIORef' nextUniq (\u -> (u+1, Uniq u)))

-- |For the implementation of 'Uniq' construction in new monads, this operation
-- is exposed.  Users must accept responsibility for ensuring true uniqueness 
-- across the lifetime of the resulting 'Uniq' value.  Failure to do so could
-- lead to type unsoundness in code depending on uniqueness as a type witness
-- (eg, "Data.Unique.Tag").
unsafeMkUniq :: Integer -> Uniq s
unsafeMkUniq n = Uniq n

-- |A `Show` instance for @`Uniq` s@ would not be sound, but for debugging
-- purposes we occasionally will want to do it anyway.  Its unsoundness is 
-- nicely demonstrated by:
-- 
-- > runST (fmap show getUniq) :: String
--
-- Which, despite having type 'String', is not referentially transparent.
unsafeShowsPrecUniq :: Int -> Uniq s -> ShowS
unsafeShowsPrecUniq p (Uniq u) = showsPrec p u

-- |See 'unsafeShowsPrecUniq'.
unsafeShowUniq :: Uniq s -> String
unsafeShowUniq (Uniq u) = show u
