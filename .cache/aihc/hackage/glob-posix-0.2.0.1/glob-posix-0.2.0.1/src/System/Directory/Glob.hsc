{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : System.Directory.Glob
Copyright   : (c) Reuben D'Netto 2016
License     : Apache 2.0
Maintainer  : rdnetto@gmail.com
Portability : POSIX

This module provides a wrapper around the <https://linux.die.net/man/3/glob glob(3)> C function, which finds file paths matching a given pattern.
All of the standard flags are supported, though GNU extensions are contained in the "System.Directory.Glob.GNU" module to encourage portability.
-}
module System.Directory.Glob (
    (<>),
    glob,
    globDefaults,
    globMany,
    GlobFlag,
    -- Only export standard flags by default
    globMark,
    globNoCheck,
    globNoEscape,
    globNoSort
    ) where

import Control.Monad (unless, forM_)
import Data.Monoid ((<>))
import Foreign (alloca, peekArray)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))

import System.Directory.Glob.Internal
#include <glob.h>


-- Each call to glob() involves String marshalling and return code checking
c_glob' :: Ptr CGlob -> GlobFlag -> String -> IO ()
c_glob' globPtr (GlobFlag f) p = withCString p $ \p' -> do
    errCode <- c_glob p' f nullPtr globPtr

    unless (errCode == 0 || errCode == (#const GLOB_NOMATCH))
        (error $ "glob failed with exit code: " ++ show errCode)

-- | Finds pathnames matching a pattern. e.g @foo*@, @prog_v?@, @ba[zr]@, etc.
glob :: GlobFlag        -- ^ The control flags to apply.
     -> String          -- ^ The pattern.
     -> IO [FilePath]   -- ^ The paths matching the pattern.
glob flags pat = alloca $ \globPtr -> do
    -- Call glob
    c_glob' globPtr flags pat

    -- Unpack results
    CGlob strs <- peek globPtr
    res <- mapM peekCString strs

    -- Cleanup
    c_globfree globPtr
    return res

-- | Like glob, but matches against multiple patterns.
--   This function only allocates and marshals data once, making it more efficient than multiple glob calls.
globMany :: GlobFlag        -- ^ The control flags to apply.
         -> [String]        -- ^ A list of patterns to apply.
         -> IO [FilePath]   -- ^ The paths matching the patterns.
globMany _ [] = return []       -- Need to handle this explicitly, or we'll free an uninitiallized glob_t.
globMany flags (p0:ps) = alloca $ \globPtr -> do
    -- First call to glob must be *without* GLOB_APPEND
    c_glob' globPtr flags p0

    -- Call glob for the remaining patterns with GLOB_APPEND
    forM_ ps . c_glob' globPtr $ flags <> globAppend

    -- Unpack paths
    CGlob strs <- peek globPtr
    res <- mapM peekCString strs

    -- Cleanup
    c_globfree globPtr
    return res

{-
    typedef struct {
        size_t   gl_pathc;    /* Count of paths matched so far  */
        char   **gl_pathv;    /* List of matched pathnames.     */
        size_t   gl_offs;     /* Slots to reserve in gl_pathv.  */
    } glob_t;
-}
newtype CGlob = CGlob [CString]

instance Storable CGlob where
    sizeOf _    = #size glob_t
    alignment _ = #alignment glob_t
    peek ptr = do
        pathC <- (#peek glob_t, gl_pathc) ptr
        pathV <- peekArray pathC  =<< (#peek glob_t, gl_pathv) ptr
        return $ CGlob pathV

    poke _ _ = error "Poke unsupported for CGlob"

-- We don't use this, so its type doesn't matter
type C_ErrorFunc = Ptr ()


-- int glob(const char *pattern, int flags, int (*errfunc) (const char *epath, int eerrno), glob_t *pglob);
foreign import ccall "glob.h glob"
     c_glob :: CString -> CInt -> C_ErrorFunc -> Ptr CGlob -> IO CInt

-- void globfree(glob_t *pglob);
foreign import ccall "glob.h globfree"
     c_globfree :: Ptr CGlob -> IO ()

