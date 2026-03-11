{-# LANGUAGE EmptyDataDecls #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Permutation
-- Copyright   :  (c) A. V. H. McPhail 2010, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL permutation functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Permutation (
                                Permutation, CanPerm
                                , random_permute
                               , get, swap, swapList
                               , size
                               , valid
                               , reverse 
                               , inverse
                               , next, prev
                               , permute, inverse_permute, mul
                               , fwritePermutation, freadPermutation, fprintfPermutation, fscanfPermutation
                               , canonical, linear
                               , inversions
                               , cyclesLinear, cyclesCanonical
                             ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data hiding(find,size)
import Numeric.LinearAlgebra.Devel

--import Data.Vector.Storable hiding(Vector,mapM_,reverse)
--import Data.Vector.Storable hiding(Vector,sum,find,mapM_)

--import Numeric.LinearAlgebra.Linear

--import Control.Monad(when)

import Foreign hiding(shift)
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
import Foreign.C.Types(CInt(..),CChar(..))
--import Foreign.C.String(newCString,peekCString)
import Foreign.C.String(newCString)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

import Prelude hiding(reverse)

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

infixr 1 #
a # b = apply a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

data Perm
type PermHandle = Ptr Perm
-- | A permutation structure
data Permutation = P { pdim :: {-# UNPACK #-} !Int -- ^ number of bins
                   , perm :: {-# UNPACK #-} !(ForeignPtr Perm) }
-- | A canonical permutation structure
data CanPerm =  CP { cdim :: {-# UNPACK #-} !Int -- ^ number of bins
                   , canperm :: {-# UNPACK #-} !(ForeignPtr Perm) }

-----------------------------------------------------------------------------
{-
instance Eq Permutation where
    (==) = equalBins

instance Num Permutation where
    (+) = add
    (-) = subtract
    negate = flip scale (-1.0)
    (*) = multiply
    signum = error "can't signumm Permutation"
    abs = error "can't abs Permutation"
    fromInteger x = fromLimits (fromInteger x) (0,1)

instance Fractional Permutation where
    fromRational x = fromLimits (round x) (0,fromRational x)
    (/) = divide
-}
-----------------------------------------------------------------------------

foreign import ccall "gsl-permutation.h gsl_permutation_alloc" permutation_new :: CInt -> IO PermHandle
foreign import ccall "gsl-permutation.h gsl_permutation_calloc" permutation_init :: CInt -> IO PermHandle
foreign import ccall "gsl-permutation.h &gsl_permutation_free" permutation_free :: FunPtr (PermHandle -> IO ())

-----------------------------------------------------------------------------

nullPermutation :: Int -> IO Permutation
nullPermutation n = do
                    p <- permutation_init (fromIntegral n)
                    p' <- newForeignPtr permutation_free p
                    return (P n p')

clonePermutation :: Permutation -> IO Permutation
clonePermutation (P n s) = do
                           d <- permutation_new (fromIntegral n)
                           d' <- newForeignPtr permutation_free d
                           check "clonePermutation" $
                              withForeignPtr s $ \s' ->
                                  withForeignPtr d' $ \d'' ->
                                      permutation_clone d'' s'
                           return (P n d')

foreign import ccall "gsl-permutation.h gsl_permutation_memcpy" permutation_clone :: PermHandle -> PermHandle -> IO CInt

-----------------------------------------------------------------------------

-- | generate a random permutation
random_permute :: Int -- ^ seed
               -> Int -- ^ size
               -> Permutation
random_permute s n = unsafePerformIO $ do
                     (P _ p) <- nullPermutation n
                     check "random_permute" $
                        withForeignPtr p $ \p' -> permutation_random_permute (fromIntegral s) p'
                     return (P n p)

foreign import ccall "permutation-aux.h random_permute" permutation_random_permute :: CInt -> PermHandle -> IO CInt

-----------------------------------------------------------------------------

-- | returns the value of the i-th element of the permutation
get :: Permutation -> Int -> Int
get (P _ p) i = unsafePerformIO $ do
                j <- withForeignPtr p $ \p' -> permutation_get p' (fromIntegral i)
                return $ fromIntegral j

-- | swaps the i-th and j-th elements
swapIO :: Permutation -> Int -> Int -> IO ()
swapIO (P _ p) i j = do
             check "swap" $
                withForeignPtr p $ \p' -> permutation_swap p' (fromIntegral i) (fromIntegral j)

-- | swaps the i-th and j-th elements
swap :: Permutation -> Int -> Int -> Permutation
swap p i j = unsafePerformIO $ do
             p' <- clonePermutation p
             swapIO p' i j
             return p'

-- | swaps pairs of elements 
swapList :: Permutation -> [(Int,Int)] -> Permutation
swapList p xs = unsafePerformIO $ do
                (P n p') <- clonePermutation p
                withForeignPtr p' $ \p'' -> mapM_ (\(i,j) -> permutation_swap p'' (fromIntegral i) (fromIntegral j)) xs
                return (P n p')

foreign import ccall "gsl-permutation.h gsl_permutation_get" permutation_get :: PermHandle -> CInt -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_swap" permutation_swap :: PermHandle -> CInt -> CInt -> IO CInt

-----------------------------------------------------------------------------

-- | get the length of the permutation
size :: Permutation -> Int
size (P s _) = s

-- | checks that the permutation is valid
valid :: Permutation -> Bool
valid (P _ p) = unsafePerformIO $ do
                v <- withForeignPtr p $ \p' -> permutation_valid p'
                if v == 0 
                   then return False
                   else return True

foreign import ccall "gsl-permutation.h gsl_permutation_valid" permutation_valid :: PermHandle -> IO CInt

-----------------------------------------------------------------------------

-- | reverse the elements of the permutation
reverseIO :: Permutation -> IO ()
reverseIO (P _ p) = do
                    check "reverseIO" $
                       withForeignPtr p $ \p' -> permutation_reverse p' 

-- | reverse the elements of the permutation
reverse :: Permutation -> Permutation
reverse p = unsafePerformIO $ do
            p' <- clonePermutation p
            reverseIO p'
            return p'

-- | computes the inverse of the permutation
inverse :: Permutation -> Permutation
inverse (P n p) = unsafePerformIO $ do
                    d <- permutation_new (fromIntegral n)
                    d' <- newForeignPtr permutation_free d
                    check "inverse" $
                       withForeignPtr d' $ \d'' ->
                        withForeignPtr p $ \p' -> permutation_inverse d'' p'
                    return (P n d')

-- | advances the permutation to the next in lexicographic order, if there is one
next :: Permutation -> IO Bool
next (P _ p) = do
               err <- withForeignPtr p $ \p' -> permutation_next p'
               if err == 0
                  then return True
                  else return False

-- | steps the permutation back to the previous in lexicographic order, if there is one
prev :: Permutation -> IO Bool
prev (P _ p) = do
               err <- withForeignPtr p $ \p' -> permutation_prev p'
               if err == 0
                  then return True
                  else return False


foreign import ccall "gsl-permutation.h gsl_permutation_reverse" permutation_reverse :: PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_inverse" permutation_inverse :: PermHandle -> PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_next" permutation_next :: PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_prev" permutation_prev :: PermHandle -> IO CInt

-----------------------------------------------------------------------------

-- | apply the permutation to a vector
permute :: Permutation -> Vector Double -> Vector Double
permute (P n p) v = unsafePerformIO $ do
                    r <- createVector n
                    (v # r # id) (\vs vp rs rp -> withForeignPtr p $ \p' -> permutation_permute p' vs vp rs rp) #| "permute"
                    return r

-- | apply the inverse permutation to a vector
inverse_permute :: Permutation -> Vector Double -> Vector Double
inverse_permute (P n p) v = unsafePerformIO $ do
                    r <- createVector n
                    (v # r #id) (\vs vp rs rp -> withForeignPtr p $ \p' -> permutation_permute_inverse p' vs vp rs rp) #| "permute"
                    return r

-- | multiply two permutations, P = PA * PB
mul :: Permutation -> Permutation -> Permutation
mul (P n p1) (P _ p2) = unsafePerformIO $ do
                        p <- permutation_new (fromIntegral n)
                        p' <- newForeignPtr permutation_free p
                        check "mul" $
                           withForeignPtr p' $ \p'' ->
                               withForeignPtr p1 $ \p1' -> 
                                   withForeignPtr p2 $ \p2' -> permutation_mul p'' p1' p2'
                        return (P n p')

foreign import ccall "permutation-aux.h permute" permutation_permute :: PermHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "permutation-aux.h permute_inverse" permutation_permute_inverse :: PermHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_mul" permutation_mul :: PermHandle -> PermHandle -> PermHandle -> IO CInt

-----------------------------------------------------------------------------

-- | write a permutation in the native binary format (may not be portable)
fwritePermutation :: FilePath -> Permutation -> IO ()
fwritePermutation fn (P _ p) = do
                       cn <- newCString fn
                       check "fwritePermutation" $
                          withForeignPtr p $ permutation_fwrite cn
                       free cn

-- | read a permutation in the native binary format, length must be known
freadPermutation :: FilePath -> Int -> IO Permutation
freadPermutation fn b = do
                      h <- permutation_new (fromIntegral b)
                      h' <- newForeignPtr permutation_free h
                      cn <- newCString fn
                      check "freadPermutation" $
                         withForeignPtr h' $ permutation_fread cn
                      return $ P b h'
                      
-- | saves the permutation with the given format
fprintfPermutation :: FilePath -> String -> Permutation -> IO ()
fprintfPermutation fn fr (P _ p) = do
                                   cn <- newCString fn
                                   cr <- newCString fr
                                   check "fprintfPermutation" $
                                      withForeignPtr p $ permutation_fprintf cn cr
                                   free cn
                                   free cr
                                   return ()

-- | reads formatted data as written by fprintf, the number of bins must be known in advance
fscanfPermutation :: FilePath -> Int -> IO Permutation
fscanfPermutation fn b = do
                         h <- permutation_new (fromIntegral b)
                         h' <- newForeignPtr permutation_free h
                         cn <- newCString fn
                         check "fscanfPermutation" $
                            withForeignPtr h' $ permutation_fscanf cn
                         return $ P b h'

foreign import ccall "permutation-aux.h perm_fwrite" permutation_fwrite :: Ptr CChar -> PermHandle -> IO CInt
foreign import ccall "permutation-aux.h perm_fread" permutation_fread :: Ptr CChar -> PermHandle -> IO CInt
foreign import ccall "permutation-aux.h perm_fprintf" permutation_fprintf :: Ptr CChar -> Ptr CChar -> PermHandle -> IO CInt
foreign import ccall "permutation-aux.h perm_fscanf" permutation_fscanf :: Ptr CChar -> PermHandle -> IO CInt

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | compute the canonical form
canonical :: Permutation -> CanPerm
canonical (P n p) = unsafePerformIO $ do
                    q <- permutation_new (fromIntegral n)
                    q' <- newForeignPtr permutation_free q
                    check "canonical" $
                       withForeignPtr p $ \p' ->
                           withForeignPtr q' $ \q'' ->
                               permutation_linear_to_canonical q'' p'
                    return (CP n q')

-- | convert from canonical to linear
linear :: CanPerm -> Permutation
linear (CP n p) = unsafePerformIO $ do
                    q <- permutation_new (fromIntegral n)
                    q' <- newForeignPtr permutation_free q
                    check "linear" $
                       withForeignPtr p $ \p' ->
                           withForeignPtr q' $ \q'' ->
                               permutation_canonical_to_linear q'' p'
                    return (P n q')

-- | a count of the inversions
inversions :: Permutation -> Int
inversions (P _ p) = unsafePerformIO $ do
                     i <- withForeignPtr p $ \p' -> permutation_inversions p'
                     return $ fromIntegral i

-- | a count of the cycles of a permutation in linear form
cyclesLinear :: Permutation -> Int
cyclesLinear (P _ p) = unsafePerformIO $ do
                     i <- withForeignPtr p $ \p' -> permutation_linear_cycles p'
                     return $ fromIntegral i

-- | a count of the cycles of a permutation in canonical form
cyclesCanonical :: CanPerm -> Int
cyclesCanonical (CP _ p) = unsafePerformIO $ do
                     i <- withForeignPtr p $ \p' -> permutation_canonical_cycles p'
                     return $ fromIntegral i


foreign import ccall "gsl-permutation.h gsl_permutation_linear_to_canonical" permutation_linear_to_canonical :: PermHandle -> PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_canonical_to_linear" permutation_canonical_to_linear :: PermHandle -> PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_inversions" permutation_inversions :: PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_linear_cycles" permutation_linear_cycles :: PermHandle -> IO CInt
foreign import ccall "gsl-permutation.h gsl_permutation_canonical_cycles" permutation_canonical_cycles :: PermHandle -> IO CInt

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
