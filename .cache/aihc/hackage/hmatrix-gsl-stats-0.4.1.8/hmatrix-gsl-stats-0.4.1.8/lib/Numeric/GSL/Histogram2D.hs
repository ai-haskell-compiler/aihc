{-# LANGUAGE EmptyDataDecls,
             FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Histogram2D
-- Copyright   :  (c) A. V. H. McPhail 2010, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL 2D histogram functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Histogram2D (
                               -- * Creation
                                Histogram2D
                               , emptyRanges, emptyLimits
                               , fromRanges, fromLimits
                               -- * Loading
                               , addList, addVector, addListWeighted, addVectorWeighted
                               -- * Marshalling
                               , toMatrix, fromMatrix
                               -- * Information
                               , getBin, getXRange, getYRange
                               , getXMax, getYMax, getXMin, getYMin, getXBins, getYBins
                               , reset
                               -- * Querying
                               , find
                               , count, prob, probPaired, countPaired, countInstance, probability
                               , maxVal, maxBin, minVal, minBin
                               -- * Statistics
                               , xmean, ymean, xstddev, ystddev, covariance, sum
                               , equalBins
                               -- * Mathematics
                               , add, subtract, multiply, divide, shift, scale
                               -- * Files     
                               , fwriteHistogram2D, freadHistogram2D, fprintfHistogram2D, fscanfHistogram2D
                               -- * PDF
                               , Histogram2DPDF
                               , fromHistogram2D
                               , sample
                               ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data hiding(find)
import Numeric.LinearAlgebra.Devel

import Data.Vector.Storable hiding(Vector,sum,find,mapM_)

--import Numeric.LinearAlgebra.Algorithms hiding (multiply)
--import Numeric.LinearAlgebra.Linear hiding (multiply,add,divide,scale)
--import Numeric.LinearAlgebra hiding (multiply,add,divide,scale,find)
--import Numeric.Container 

--import Control.Monad
--import Control.Monad(when)

import Data.Binary

import Foreign hiding(shift)
--import Foreign.Storable
--import Foreign.Ptr
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
import Foreign.C.Types(CInt(..),CChar(..))
--import Foreign.C.String(newCString,peekCString)
import Foreign.C.String(newCString)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

import Prelude hiding(subtract,sum,length)

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

infixr 1 #
a # b = applyRaw a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

instance (Storable a) => Storable (a,a) where
    sizeOf z        = 2 * sizeOf (fst z)
    alignment z     = alignment (fst z)
    peek p          = do let q = castPtr p
                         a <- peek q
                         b <- peekElemOff q 1
                         return (a,b)
    poke p (a,b) = do let q = (castPtr p)
                      poke q a
                      pokeElemOff q 1 b

-----------------------------------------------------------------------------

data Hist2D
type Hist2DHandle = Ptr Hist2D
-- | A histogram structure
data Histogram2D = H { hxdim :: {-# UNPACK #-} !Int -- ^ number of bins
                     , hydim :: {-# UNPACK #-} !Int -- ^ number of bins
                     , hist :: {-# UNPACK #-} !(ForeignPtr Hist2D) }

data PDF
type PDFHandle = Ptr PDF
-- | A histogram-derived cumulative distribution function (CDF)
data Histogram2DPDF = P { pdf :: {-# UNPACK #-} !(ForeignPtr PDF)}

-----------------------------------------------------------------------------

instance Eq Histogram2D where
    (==) = equalBins
{-
instance Num Histogram2D where
    (+) = add
    (-) = subtract
    negate = flip scale (-1.0)
    (*) = multiply
    signum = error "can't signumm Histogram2D"
    abs = error "can't abs Histogram2D"
    fromInteger x = fromLimits (fromInteger x) (0,1)

instance Fractional Histogram2D where
    fromRational x = fromLimits (round x) (0,fromRational x)
    (/) = divide
-}
-----------------------------------------------------------------------------

instance Binary Histogram2D where
    put h = do
            let (rx,ry,w) = toMatrix h
            put rx
            put ry
            put w
    get = do
          rx <- get
          ry <- get
          w <- get
          return $! fromMatrix rx ry w

-----------------------------------------------------------------------------

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_alloc" histogram2d_new :: CInt -> CInt -> IO Hist2DHandle
foreign import ccall "gsl-histogram2d.h &gsl_histogram2d_free" histogram2d_free :: FunPtr (Hist2DHandle -> IO ())

-----------------------------------------------------------------------------

-- | create a histogram with n bins from ranges (x0->x1),(x1->x2),..,(xn->xn+1) and y0,..,yn+1
fromRangesIO :: Vector Double -> Vector Double -> IO Histogram2D
fromRangesIO v w = do
                   let sx = fromIntegral $ size v - 1
                   let sy = fromIntegral $ size w - 1
                   h <- histogram2d_new sx sy
                   h' <- newForeignPtr histogram2d_free h
                   (v # w # id) (\xs xp ys yp -> withForeignPtr h' (\f -> histogram2d_set_ranges f xp xs yp ys)) #| "fromRanges"
                   return $ H (fromIntegral sx) (fromIntegral sy) h'

-- | create a histogram with n bins and lower and upper limits
fromLimitsIO :: Int -> Int      -- ^ number of bins
           -> (Double,Double) -- ^ xmin, xmax
           -> (Double,Double) -- ^ ymin, ymax
           -> IO Histogram2D
fromLimitsIO nx ny (lx,ux) (ly,uy) = do
                                   h <- histogram2d_new (fromIntegral nx) (fromIntegral ny)
                                   h' <- newForeignPtr histogram2d_free h
                                   check "set_ranges_uniform" $ withForeignPtr h' (\f -> histogram2d_set_ranges_uniform f lx ux ly uy)
                                   return $ H nx ny h'

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_set_ranges" histogram2d_set_ranges :: Hist2DHandle -> Ptr Double -> CInt -> Ptr Double -> CInt -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_set_ranges_uniform" histogram2d_set_ranges_uniform :: Hist2DHandle -> Double -> Double -> Double -> Double -> IO CInt

-----------------------------------------------------------------------------

-- | create a histogram with n bins from ranges (x0->x1),(x1->x2)..(xn->xn+1) 
emptyRanges :: Vector Double  -- ^ the x ranges
            -> Vector Double  -- ^ the y ranges
            -> Histogram2D      -- ^ result
emptyRanges x y = unsafePerformIO $ fromRangesIO x y

-- | create a histogram with n bins and lower and upper limits
emptyLimits :: Int -> Int      -- ^ bins
            -> (Double,Double) -- ^ lower and upper limits x
            -> (Double,Double) -- ^ lower and upper limits y
            -> Histogram2D       -- ^ result
emptyLimits nx ny x y = unsafePerformIO $ fromLimitsIO nx ny x y

-- | create a histogram with n bins from ranges (x0->x1),(x1->x2)..(xn->xn+1) and increment from a vector
fromRanges :: Vector Double     -- ^ the x ranges
           -> Vector Double     -- ^ the y ranges
           -> [(Double,Double)] -- ^ the data
           -> Histogram2D       -- ^ result
fromRanges rx ry d = unsafePerformIO $ do
                     h <- fromRangesIO rx ry
                     incrementListIO h d
                     return h

-- | create a histogram with n bins and lower and upper limits and increment from a vector
fromLimits :: Int -> Int        -- ^ bins
           -> (Double,Double)   -- ^ x lower and upper limits
           -> (Double,Double)   -- ^ y lower and upper limits
           -> [(Double,Double)] -- ^ the data
           -> Histogram2D       -- ^ result
fromLimits nx ny rx ry d = unsafePerformIO $ do
                           h <- fromLimitsIO nx ny rx ry
                           incrementListIO h d
                           return h

-----------------------------------------------------------------------------

-- | extract the ranges and bins
toMatrix :: Histogram2D -> (Vector Double,Vector Double,Matrix Double) -- ^ (ranges,bins)
toMatrix (H bx by h) = unsafePerformIO $ do
                    rx <- createVector (bx+1)
                    ry <- createVector (by+1)
                    bs <- createMatrix RowMajor bx by
                    (rx # ry # bs # id) (\s1 p1 s2 p2 sx sy p -> withForeignPtr h $ \f -> histogram_to_matrix f s1 p1 s2 p2 sx sy p) #| "toMatrix"
                    return (rx,ry,bs)

foreign import ccall "histogram-aux.h to_matrix" histogram_to_matrix :: Hist2DHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------
{-
vectorToTuples = toTuples . toList
    where toTuples []         = error "need a minimum of two elements"
          toTuples [_]        = error "need a minimum of two elements"
          toTuples [x1,x2]    = [(x1,x2)]
          toTuples (x1:x2:xs) = (x1,x2) : (toTuples (x2:xs))
-}
-----------------------------------------------------------------------------

-- | create from ranges and bins
fromMatrix :: Vector Double            -- ^ x ranges
           -> Vector Double            -- ^ y ranges
           -> Matrix Double            -- ^ bins (row major)
           -> Histogram2D              -- ^result
fromMatrix x y w = unsafePerformIO $ do
                   h@(H _ _ h') <- fromRangesIO x y
                   (x # y # (cmat w) # id) (\xs x' ys y' rs cs b -> withForeignPtr h' $ \h'' -> histogram_from_matrix h'' xs x' ys y' rs cs b) #| "fromMatrix"
                   return h

foreign import ccall "histogram-aux.h from_matrix" histogram_from_matrix :: Hist2DHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | create a copy of a histogram
cloneHistogram2D :: Histogram2D -> IO Histogram2D
cloneHistogram2D (H nx ny h) = do
                               h' <- withForeignPtr h histogram2d_clone
                               h'' <- newForeignPtr histogram2d_free h'
                               return $ H nx ny h''

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_clone" histogram2d_clone :: Hist2DHandle -> IO Hist2DHandle

-----------------------------------------------------------------------------

-- | add 1.0 to the correct bin for each element of the list, fails silently if the value is outside the range
addList :: Histogram2D -> [(Double,Double)] -> Histogram2D
addList h xs = unsafePerformIO $ do
            h' <- cloneHistogram2D h
            incrementListIO h' xs
            return h'

-- | add 1.0 to the correct bin for each element of the vector pair, fails silently if the value is outside the range
addVector :: Histogram2D -> Vector Double -> Vector Double -> Histogram2D
addVector h x y = unsafePerformIO $ do
                  h' <- cloneHistogram2D h
                  incrementVectorIO h' x y
                  return h'

-- add the appropriate weight for each element of the list, fails silently if the value is outside the range
addListWeighted :: Histogram2D -> [(Double,Double,Double)] -> Histogram2D
addListWeighted h xs = unsafePerformIO $ do
                        h' <- cloneHistogram2D h
                        accumulateListIO h' xs
                        return h'

-- add the appropriate weight for each element of the vector pair, fails silently if the value is outside the range
addVectorWeighted :: Histogram2D -> Vector Double -> Vector Double -> Vector Double -> Histogram2D
addVectorWeighted h x y w = unsafePerformIO $ do
                        h' <- cloneHistogram2D h
                        accumulateVectorIO h' x y w
                        return h'


-----------------------------------------------------------------------------

-- | add 1.0 to the correct bin, fails silently if the value is outside the range
incrementIO :: Histogram2D -> Double -> Double -> IO ()
incrementIO (H _ _ h) x y = do
                            check "increment" $ withForeignPtr h (\f -> histogram2d_increment f x y)
                            return ()

-- | add 1.0 to the correct bin for each element of the vector pair, fails silently if the value is outside the range
incrementVectorIO :: Histogram2D -> Vector Double -> Vector Double -> IO ()
incrementVectorIO (H _ _ h) x y = do
                                (x # y # id) (\xs xp ys yp -> withForeignPtr h (\f -> histogram2d_increment_matrix f xs xp ys yp)) #| "incrementVector"
                                return ()

-- | add 1.0  to the correct bin for each element of the list, fails silently if the value is outside the range
incrementListIO :: Histogram2D -> [(Double,Double)] -> IO ()
incrementListIO (H _ _ h) zs = withForeignPtr h (\f -> mapM_ (\(x,y) -> histogram2d_increment f x y) zs)
              
-- | Adds the weight (third Double) to the bin appropriate for the value (first two Doubles)
accumulateIO :: Histogram2D -> Double -> Double -> Double -> IO ()
accumulateIO (H _ _ h) x y w = do
                               check "accumulate" $ withForeignPtr h (\f -> histogram2d_accumulate f x y w)
                               return ()

-- | add the weight (third) to the correct bin for each vector pair element, fails silently if the value is outside the range
accumulateVectorIO :: Histogram2D -> Vector Double -> Vector Double -> Vector Double -> IO ()
accumulateVectorIO (H _ _ h) x y w = do
                                (x # y # w # id) (\xs xp ys yp ws wp -> withForeignPtr h (\f -> histogram2d_accumulate_matrix f xs xp ys yp ws wp)) #| "accumulateVector"
                                return ()

-- | add the weight (snd) to the correct bin for each (fst) element of the list, fails silently if the value is outside the range
accumulateListIO :: Histogram2D -> [(Double,Double,Double)] -> IO ()
accumulateListIO (H _ _ h) zs = withForeignPtr h (\f -> mapM_ (\(x,y,w) -> histogram2d_accumulate f x y w) zs)

-- | returns the contents of the i-th bin
getBin :: Histogram2D -> (Int,Int) -> Double
getBin (H _ _ h) (bx,by) = unsafePerformIO $ do
                           withForeignPtr h (\f -> histogram2d_get f (fromIntegral bx) (fromIntegral by))

-- | returns the upper and lower limits in the first dimension of the i-th bin
getXRange :: Histogram2D -> Int -> (Double,Double)
getXRange (H _ _ h) b = unsafePerformIO $ do
                        alloca $ \l ->
                            alloca $ \u -> do
                            check "get_xrange" $ withForeignPtr h (\f -> histogram2d_get_xrange f (fromIntegral b) l u)
                            l' <- peek l
                            u' <- peek u
                            return (l',u')

-- | returns the upper and lower limits in the second dimension of the i-th bin
getYRange :: Histogram2D -> Int -> (Double,Double)
getYRange (H _ _ h) b = unsafePerformIO $ do
                        alloca $ \l ->
                            alloca $ \u -> do
                            check "get_yrange" $ withForeignPtr h (\f -> histogram2d_get_yrange f (fromIntegral b) l u)
                            l' <- peek l
                            u' <- peek u
                            return (l',u')

-- | the maximum upper range limit in the first dimension
getXMax :: Histogram2D -> Double
getXMax (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_xmax

-- | the minimum lower range limit in the first dimension
getXMin :: Histogram2D -> Double
getXMin (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_xmin

-- | the number of binsin the first dimension
getXBins :: Histogram2D -> Int
getXBins (H bx _ _) = bx

-- | the maximum upper range limit in the first dimension
getYMax :: Histogram2D -> Double
getYMax (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_ymax

-- | the minimum lower range limit in the first dimension
getYMin :: Histogram2D -> Double
getYMin (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_ymin

-- | the number of binsin the first dimension
getYBins :: Histogram2D -> Int
getYBins (H _ by _) = by

-- | reset all the bins to zero
reset :: Histogram2D -> IO ()
reset (H _ _ h) = withForeignPtr h histogram2d_reset

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_increment" histogram2d_increment :: Hist2DHandle -> Double -> Double -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_accumulate" histogram2d_accumulate :: Hist2DHandle -> Double -> Double -> Double -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_get" histogram2d_get :: Hist2DHandle -> CInt -> CInt -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_get_xrange" histogram2d_get_xrange :: Hist2DHandle -> CInt -> Ptr Double -> Ptr Double -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_get_yrange" histogram2d_get_yrange :: Hist2DHandle -> CInt -> Ptr Double -> Ptr Double -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_xmax" histogram2d_xmax :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_xmin" histogram2d_xmin :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_nx" histogram2d_xn :: Hist2DHandle -> IO Int
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_ymax" histogram2d_ymax :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_ymin" histogram2d_ymin :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_ny" histogram2d_yn :: Hist2DHandle -> IO Int
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_reset" histogram2d_reset :: Hist2DHandle -> IO ()

foreign import ccall "histogram-aux.h increment_matrix" histogram2d_increment_matrix :: Hist2DHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "histogram-aux.h accumulate_matrix" histogram2d_accumulate_matrix :: Hist2DHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | find the bin corresponding to the value
find :: Histogram2D -> (Double,Double) -> Maybe (Int,Int)
find (H _ _ h) (x,y) = unsafePerformIO $ do
                       alloca $ \bx -> 
                           alloca $ \by -> do
                           err <- withForeignPtr h (\f -> histogram2d_find f x y bx by)
                           if err == 0
                              then do
                                   bx' <- peek bx
                                   by' <- peek by
                                   return $ Just $ (fromIntegral bx',fromIntegral by')
                              else return Nothing

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_find" histogram2d_find :: Hist2DHandle -> Double -> Double -> Ptr CInt -> Ptr CInt -> IO CInt

-- | find the number of occurences for the input
countInstance :: Histogram2D -> (Double,Double) -> Double
countInstance h x = let Just x' = find h x in getBin h x'

-- | find the probability of the input
probability :: Histogram2D -> (Double,Double) -> Double
probability h x = let Just x' = find h x in (getBin h x') / (sum h)

-- | find the number of occurences for each element of the input vector
count :: Histogram2D -> (Vector Double, Vector Double) -> Vector Double
count (H _ _ h) (x,y) = unsafePerformIO $ do
               r <- createVector $ size x
               (x # y # r # id) (\xs' x' ys' y' rs' r' -> withForeignPtr h $ \h' -> histogram2d_count h' xs' x' ys' y' rs' r') #| "histogram2d_count"
               return r

foreign import ccall "histogram-aux.h hist2d_count" histogram2d_count :: Hist2DHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-- | find the joint probability of occuring for each element of the input vector pair
prob :: Histogram2D -> (Vector Double,Vector Double) -> Vector Double
prob h z = (count h z) / (scalar $ sum h)

-- | find the number of occurences for each element of the input vector
countPaired :: Histogram2D -> Vector (Double,Double) -> Vector Double
countPaired (H _ _ h) x = unsafePerformIO $ do
               r <- createVector $ length x
               (x # r # id) (\xs' x' rs' r' -> withForeignPtr h $ \h' -> histogram2d_count_pair h' xs' x' rs' r') #| "histogram2d_count_pair"
               return r

foreign import ccall "histogram-aux.h hist2d_count_pair" histogram2d_count_pair :: Hist2DHandle -> CInt -> Ptr (Double,Double) -> CInt -> Ptr Double -> IO CInt

-- | find the joint probability of occuring for each element of the input vector pair
probPaired :: Histogram2D -> (Vector (Double,Double)) -> Vector Double
probPaired h z = (countPaired h z) / (scalar $ sum h)

-----------------------------------------------------------------------------

-- | the maximum value contained in the bins
maxVal :: Histogram2D -> Double
maxVal (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_max_val

-- | the index of the bin containing the maximum value
maxBin :: Histogram2D -> (Int,Int)
maxBin (H _ _ h) = unsafePerformIO $ do
                   alloca $ \bx -> 
                       alloca $ \by -> do
                           withForeignPtr h (\f -> histogram2d_max_bin f bx by)
                           bx' <- peek bx
                           by' <- peek by
                           return $ (fromIntegral bx',fromIntegral by')

-- | the minimum value contained in the bins
minVal :: Histogram2D -> Double
minVal (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_min_val

-- | the index of the bin containing the minimum value
minBin :: Histogram2D -> (Int,Int)
minBin (H _ _ h) = unsafePerformIO $ do
                   alloca $ \bx -> 
                       alloca $ \by -> do
                           withForeignPtr h (\f -> histogram2d_min_bin f bx by)
                           bx' <- peek bx
                           by' <- peek by
                           return $ (fromIntegral bx',fromIntegral by')

-- | the mean of the values in the first dimension, accuracy limited by bin width
xmean :: Histogram2D -> Double
xmean (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_xmean

-- | the mean of the values in the second dimension, accuracy limited by bin width
ymean :: Histogram2D -> Double
ymean (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_ymean

-- | the standard deviation of the values in thee first dimension, accuracy limited by bin width
xstddev :: Histogram2D -> Double
xstddev (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_xsigma

-- | the standard deviation of the values in thee first dimension, accuracy limited by bin width
ystddev :: Histogram2D -> Double
ystddev (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_ysigma

-- | the covariance of the first and second dimensions
covariance :: Histogram2D -> Double
covariance (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_cov

-- | the sum of the values, accuracy limited by bin width
sum :: Histogram2D -> Double
sum (H _ _ h) = unsafePerformIO $ withForeignPtr h histogram2d_sum

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_max_val" histogram2d_max_val :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_max_bin" histogram2d_max_bin :: Hist2DHandle -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_min_val" histogram2d_min_val :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_min_bin" histogram2d_min_bin :: Hist2DHandle -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_xmean" histogram2d_xmean :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_ymean" histogram2d_ymean :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_xsigma" histogram2d_xsigma :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_ysigma" histogram2d_ysigma :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_cov" histogram2d_cov :: Hist2DHandle -> IO Double
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_sum" histogram2d_sum :: Hist2DHandle -> IO Double

-----------------------------------------------------------------------------

-- | returns True of all the individual bin ranges of the two histograms are identical
equalBins :: Histogram2D -> Histogram2D -> Bool
equalBins (H _ _ h1) (H _ _ h2) = unsafePerformIO $ do
                          j <- withForeignPtr h1 $ \p1 -> do
                               withForeignPtr h2 $ \p2 -> histogram2d_equal_bins p1 p2
                          if (fromIntegral j) == (1 :: Int)
                             then return True
                             else return False

-- | adds the contents of the bins of the second histogram to the first
add :: Histogram2D -> Histogram2D -> Histogram2D
add d (H _ _ s) = unsafePerformIO $ do
          h@(H _ _ d') <- cloneHistogram2D d                  
          check "add" $
             withForeignPtr d' $ \dp ->
                 withForeignPtr s $ \sp -> histogram2d_add dp sp
          return h

-- | subtracts the contents of the bins of the second histogram from the first
subtract :: Histogram2D -> Histogram2D -> Histogram2D
subtract d (H _ _ s) = unsafePerformIO $ do
               h@(H _ _ d') <- cloneHistogram2D d                  
               check "subtract" $
                  withForeignPtr d' $ \dp ->
                      withForeignPtr s $ \sp -> histogram2d_sub dp sp
               return h

-- | multiplies the contents of the bins of the second histogram by the first
multiply :: Histogram2D -> Histogram2D -> Histogram2D
multiply d (H _ _ s) = unsafePerformIO $ do
               h@(H _ _ d') <- cloneHistogram2D d                  
               check "multiply" $
                  withForeignPtr d' $ \dp ->
                      withForeignPtr s $ \sp -> histogram2d_mul dp sp
               return h

-- | divides the contents of the bins of the first histogram by the second
divide :: Histogram2D -> Histogram2D -> Histogram2D
divide d (H _ _ s) = unsafePerformIO $ do
             h@(H _ _ d') <- cloneHistogram2D d                  
             check "divide" $
                withForeignPtr d' $ \dp ->
                    withForeignPtr s $ \sp -> histogram2d_div dp sp
             return h

-- | multiplies the contents of the bins by a constant
scale :: Histogram2D -> Double -> Histogram2D
scale d s = unsafePerformIO $ do
            h@(H _ _ d') <- cloneHistogram2D d                  
            check "scale" $
               withForeignPtr d' $ (\f -> histogram2d_scale f s)
            return h

-- | adds a constant to the contents of the bins
shift :: Histogram2D -> Double -> Histogram2D
shift d s = unsafePerformIO $ do
            h@(H _ _ d') <- cloneHistogram2D d                  
            check "shift" $
               withForeignPtr d' $ (\f -> histogram2d_shift f s)
            return h

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_equal_bins_p" histogram2d_equal_bins :: Hist2DHandle -> Hist2DHandle -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_add" histogram2d_add :: Hist2DHandle -> Hist2DHandle -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_sub" histogram2d_sub :: Hist2DHandle -> Hist2DHandle -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_mul" histogram2d_mul :: Hist2DHandle -> Hist2DHandle -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_div" histogram2d_div :: Hist2DHandle -> Hist2DHandle -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_scale" histogram2d_scale :: Hist2DHandle -> Double -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_shift" histogram2d_shift :: Hist2DHandle -> Double -> IO CInt

-----------------------------------------------------------------------------

-- | write a histogram in the native binary format (may not be portable)
fwriteHistogram2D :: FilePath -> Histogram2D -> IO ()
fwriteHistogram2D fn (H _ _ h) = do
                       cn <- newCString fn
                       check "fwriteHistogram2d2D" $
                          withForeignPtr h $ histogram2d_fwrite cn
                       free cn

-- | read a histogram in the native binary format, number of bins must be known
freadHistogram2D :: FilePath -> Int -> Int -> IO Histogram2D
freadHistogram2D fn bx by = do
                            h <- histogram2d_new (fromIntegral bx) (fromIntegral by)
                            h' <- newForeignPtr histogram2d_free h
                            cn <- newCString fn
                            check "freadHistogram2d2D" $
                               withForeignPtr h' $ histogram2d_fread cn
                            return $ H bx by h'
                      
-- | saves the histogram with the given formats (%f,%e,%g) for ranges and bins
--   each line comprises: xrange[i] xrange[i+1] xrange[j] xrange[j+1]  bin(i,j)
fprintfHistogram2D :: FilePath -> String -> String -> Histogram2D -> IO ()
fprintfHistogram2D fn fr fb (H _ _ h) = do
                                  cn <- newCString fn
                                  cr <- newCString fr
                                  cb <- newCString fb
                                  check "fprintfHistogram2d2D" $
                                     withForeignPtr h $ histogram2d_fprintf cn cr cb
                                  free cn
                                  free cr
                                  free cb
                                  return ()

-- | reads formatted data as written by fprintf, the number of bins must be known in advance
fscanfHistogram2D :: FilePath -> Int -> Int -> IO Histogram2D
fscanfHistogram2D fn bx by = do
                             h <- histogram2d_new (fromIntegral bx) (fromIntegral by)
                             h' <- newForeignPtr histogram2d_free h
                             cn <- newCString fn
                             check "fscanfHistogram2d2D" $
                                withForeignPtr h' $ histogram2d_fscanf cn
                             return $ H bx by h'

foreign import ccall "histogram-aux.h hist2d_fwrite" histogram2d_fwrite :: Ptr CChar -> Hist2DHandle -> IO CInt
foreign import ccall "histogram-aux.h hist2d_fread" histogram2d_fread :: Ptr CChar -> Hist2DHandle -> IO CInt
foreign import ccall "histogram-aux.h hist2d_fprintf" histogram2d_fprintf :: Ptr CChar -> Ptr CChar -> Ptr CChar -> Hist2DHandle -> IO CInt
foreign import ccall "histogram-aux.h hist2d_fscanf" histogram2d_fscanf :: Ptr CChar -> Hist2DHandle -> IO CInt
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_pdf_alloc" histogram2d_pdf_new :: CInt -> CInt -> IO PDFHandle
foreign import ccall "gsl-histogram2d.h &gsl_histogram2d_pdf_free" histogram2d_pdf_free :: FunPtr (PDFHandle -> IO ())

-----------------------------------------------------------------------------

-- | create a histogram PDF from a histogram
fromHistogram2D :: Histogram2D -> Histogram2DPDF
fromHistogram2D (H bx by h) = unsafePerformIO $ do
                              p <- histogram2d_pdf_new (fromIntegral bx) (fromIntegral by) 
                              p' <- newForeignPtr histogram2d_pdf_free p
                              withForeignPtr p' $ \p'' -> 
                                  withForeignPtr h $ \h' -> do
                                                check "pdf_init" $ histogram2d_pdf_init p'' h'
                                                return $ P p'

-- | given a randomm from the uniform distribution [0,1], draw a random sample from the PDF
sample :: Histogram2DPDF -> Double -> Double
sample (P p) r = unsafePerformIO $ withForeignPtr p $ \p' -> histogram2d_pdf_sample p' r

foreign import ccall "gsl-histogram2d.h gsl_histogram2d_pdf_init" histogram2d_pdf_init :: PDFHandle -> Hist2DHandle -> IO CInt
foreign import ccall "gsl-histogram2d.h gsl_histogram2d_pdf_sample" histogram2d_pdf_sample :: PDFHandle -> Double -> IO Double

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
{-
unzipPair :: Vector (Double,Double) -> (Vector Double,Vector Double)
unzipPair v = unsafePerformIO $ do
              a <- createVector $ size v
              b <- createVector $ size v
              histogram2d_unzip_double_pair # v # a # b #| "unzipPair"
              return (a,b)

foreign import ccall "histogram-aux.h unzip_double_pair" histogram2d_unzip_double_pair :: CInt -> Ptr (Double,Double) -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
-}
-----------------------------------------------------------------------------
