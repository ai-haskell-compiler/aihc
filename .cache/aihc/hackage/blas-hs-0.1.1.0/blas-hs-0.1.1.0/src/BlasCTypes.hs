{-# OPTIONS_HADDOCK hide #-}
module BlasCTypes (BlasInt, BlasIndex) where
import Foreign.C.Types

-- Here, we define some aliases for the C types in case the Blas
-- implementation decides to use some non-standard types.

-- This is the main integral type used by the CBlas interface.  Usually, this
-- corresponds the `int` type in C.  However, some implementations may offer
-- an alternative interface that uses a different type (e.g. `int64`).
type BlasInt = CInt

-- This is the integral type used for indices returned by `iamax` functions.
-- Usually, this corresponds to the `size_t` type in C.  In CBlas, this is
-- referred to as `CBLAS_INDEX`.
type BlasIndex = CSize
