{-# LANGUAGE EmptyDataDecls,
             FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Histogram
-- Copyright   :  (c) A. V. H. McPhail 2010, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL histogram functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Histogram (
                             -- * Creation
                              Histogram
                             , emptyRanges, emptyLimits
                             , fromRanges, fromLimits
                             -- * Loading
                             , addList, addVector, addListWeighted, addVectorWeighted
                             -- * Marshalling
                             , toVectors, fromVectors
                             -- * Information
                             , getBin, getRange
                             , getMax, getMin, getBins
                             -- * Querying
                             , find 
                             , count, prob, countInstance, probability
                             , maxVal, maxBin, minVal, minBin
                             -- * Statistics
                             , mean, stddev, sum
                             , equalBins
                             -- * Mathematics
                             , add, subtract, multiply, divide, shift, scale
                             -- * Files     
                             , fwriteHistogram, freadHistogram, fprintfHistogram, fscanfHistogram
                             -- * PDF
                             , HistogramPDF
                             , fromHistogram
                             , sample
                             ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data hiding(find)
import Numeric.LinearAlgebra.Devel

--import Numeric.LinearAlgebra.Algorithms hiding (multiply)
--import Numeric.LinearAlgebra hiding (multiply,add,divide,scale,find)
--import Numeric.Container 

--import Control.Monad
import Data.Binary

import Foreign hiding(shift)
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
import Foreign.C.Types(CInt(..),CChar(..))
--import Foreign.C.String(newCString,peekCString)
import Foreign.C.String(newCString)
--import Control.Monad(when)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

import Prelude hiding(subtract,sum)

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

data Hist
infixr 1 #
a # b = apply a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

type HistHandle = Ptr Hist
-- | A histogram structure
data Histogram = H { hdim :: {-# UNPACK #-} !Int -- ^ number of bins
                   , hist :: {-# UNPACK #-} !(ForeignPtr Hist) }

data PDF
type PDFHandle = Ptr PDF
-- | A histogram-derived cumulative distribution function (CDF)
data HistogramPDF = P { pdf :: {-# UNPACK #-} !(ForeignPtr PDF)}

-----------------------------------------------------------------------------

instance Eq Histogram where
    (==) = equalBins
{-
instance Num Histogram where
    (+) = add
    (-) = subtract
    negate = flip scale (-1.0)
    (*) = multiply
    signum = error "can't signumm Histogram"
    abs = error "can't abs Histogram"
    fromInteger x = fromLimits (fromInteger x) (0,1)

instance Fractional Histogram where
    fromRational x = fromLimits (round x) (0,fromRational x)
    (/) = divide
-}
-----------------------------------------------------------------------------

instance Binary Histogram where
    put h = do
            let (b,c) = toVectors h
            put b
            put c
    get = do
          b <- get
          c <- get
          return $! fromVectors b c

-----------------------------------------------------------------------------

foreign import ccall "gsl-histogram.h gsl_histogram_alloc" histogram_new :: CInt -> IO HistHandle
foreign import ccall "gsl-histogram.h &gsl_histogram_free" histogram_free :: FunPtr (HistHandle -> IO ())

-----------------------------------------------------------------------------

-- | create a histogram with n bins from ranges (x0->x1),(x1->x2)..(xn->xn+1)
fromRangesIO :: Vector Double -> IO Histogram
fromRangesIO v = do
               let sz = fromIntegral $ size v - 1
               h <- histogram_new sz
               h' <- newForeignPtr histogram_free h
               (v # id) (\d p -> withForeignPtr h' $ \f -> histogram_set_ranges f (fromIntegral d) p) #| "fromRanges"
               return $ H (fromIntegral sz) h'

-- | create a histogram with n bins and lower and upper limits
fromLimitsIO :: Int -> (Double,Double) -> IO Histogram
fromLimitsIO n (l,u) = do
                         h <- histogram_new (fromIntegral n)
                         h' <- newForeignPtr histogram_free h
                         check "set_ranges_uniform" $ withForeignPtr h' (\f -> histogram_set_ranges_uniform f l u)
                         return $ H n h'

foreign import ccall "gsl-histogram.h gsl_histogram_set_ranges" histogram_set_ranges :: HistHandle -> CInt -> Ptr Double -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_set_ranges_uniform" histogram_set_ranges_uniform :: HistHandle -> Double -> Double -> IO CInt

-----------------------------------------------------------------------------

-- | create a histogram with n bins from ranges (x0->x1),(x1->x2)..(xn->xn+1) 
emptyRanges :: Vector Double  -- ^ the ranges
            -> Histogram      -- ^ result
emptyRanges x = unsafePerformIO $ fromRangesIO x

-- | create a histogram with n bins and lower and upper limits
emptyLimits :: Int             -- ^ bins
            -> (Double,Double) -- ^ lower and upper limits
            -> Histogram       -- ^ result
emptyLimits n x = unsafePerformIO $ fromLimitsIO n x

-- | create a histogram with n bins from ranges (x0->x1),(x1->x2)..(xn->xn+1) and increment from a vector
fromRanges :: Vector Double   -- ^ the ranges
           -> Vector Double   -- ^ the data
           -> Histogram       -- ^ result
fromRanges r d = unsafePerformIO $ do
                 h <- fromRangesIO r
                 incrementVectorIO h d
                 return h

-- | create a histogram with n bins and lower and upper limits and increment from a vector
fromLimits :: Int              -- ^ bins
           -> (Double,Double)  -- ^ lower and upper limits
           -> Vector Double    -- ^ the data
           -> Histogram        -- ^ result
fromLimits n r d = unsafePerformIO $ do
                   h <- fromLimitsIO n r
                   incrementVectorIO h d
                   return h

-----------------------------------------------------------------------------
{-
vectorToTuples = toTuples . toList
    where toTuples []         = error "need a minimum of two elements"
          toTuples [_]        = error "need a minimum of two elements"
          toTuples [x1,x2]    = [(x1,x2)]
          toTuples (x1:x2:xs) = (x1,x2) : (toTuples (x2:xs))

middle = fromList . map (\(x1,x2) -> (x1 + x2)/2) . vectorToTuples
-}
-----------------------------------------------------------------------------

-- | create a histogram from the ranges and bin weights
fromVectors :: Vector Double -> Vector Double -> Histogram
fromVectors r b = unsafePerformIO $ do
                  h@(H _ h') <- fromRangesIO r
                  (r # b # id) (\rs r' bs b' -> withForeignPtr h' $ \h'' -> histogram_from_vectors h'' rs r' bs b') #| "fromVectors"
                  return h
                  
foreign import ccall "gsl-histogram.h from_vectors" histogram_from_vectors :: HistHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-- | extract the ranges and bin weights
toVectors :: Histogram -> (Vector Double,Vector Double) -- ^ (ranges,bins)
toVectors (H b h) = unsafePerformIO $ do
                    rs <- createVector (b+1)
                    bs <- createVector b
                    (rs # bs # id) (\s1 p1 s2 p2 -> withForeignPtr h $ \f -> histogram_to_vectors f s1 p1 s2 p2) #| "toVectors"
                    return (rs,bs)

foreign import ccall "gsl-histogram.h to_vectors" histogram_to_vectors :: HistHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | create a copy of a histogram
cloneHistogram :: Histogram -> IO Histogram
cloneHistogram (H b h) = do
                         h' <- withForeignPtr h histogram_clone
                         h'' <- newForeignPtr histogram_free h'
                         return $ H b h''

foreign import ccall "gsl-histogram.h gsl_histogram_clone" histogram_clone :: HistHandle -> IO HistHandle

-----------------------------------------------------------------------------

-- | adds 1.0 to the correct bin for each element of the list
addList :: Histogram -> [Double] -> Histogram
addList h xs = unsafePerformIO $ do
               h' <- cloneHistogram h
               incrementListIO h' xs
               return h'

-- | adds 1.0 to the correct bin for each element of the vector
addVector :: Histogram -> Vector Double -> Histogram
addVector h v = unsafePerformIO $ do
                h' <- cloneHistogram h
                incrementVectorIO h' v
                return h'

-- | adds the appropriate weight for each element of the list
addListWeighted :: Histogram -> [(Double,Double)] -> Histogram
addListWeighted h xs = unsafePerformIO $ do
                     h' <- cloneHistogram h
                     accumulateListIO h' xs
                     return h'

-- | adds the appropriate weight for each element of the list
addVectorWeighted :: Histogram -> Vector Double -> Vector Double -> Histogram
addVectorWeighted h v w = unsafePerformIO $ do
                        h' <- cloneHistogram h
                        accumulateVectorIO h' v w
                        return h'

-- | add 1.0 to the correct bin, fails silently if the value is outside the range
incrementIO :: Histogram -> Double -> IO ()
incrementIO (H _ h) x = withForeignPtr h (\f -> check "increment" $ histogram_increment f x)

-- | add 1.0 to the correct bin for each element of the vector, fails silently if the value is outside the range
incrementListIO :: Histogram -> [Double] -> IO ()
incrementListIO (H _ h) xs = withForeignPtr h (\f -> mapM_ (histogram_increment f) xs)

-- | add 1.0 to the correct bin for each element of the vector, fails silently if the value is outside the range
incrementVectorIO :: Histogram -> Vector Double -> IO ()
incrementVectorIO (H _ h) v = do
                             (v # id) (\s p -> withForeignPtr h (\f -> histogram_increment_vector f s p)) #| "incrementVector"
                             return ()

-- | adds the weight (second Double) to the bin appropriate for the value (first Double)
accumulateIO :: Histogram -> Double -> Double -> IO ()
accumulateIO (H _ h) x w = withForeignPtr h (\f -> check "accumulate" $ histogram_accumulate f x w)

-- | add the weight (snd) to the correct bin for each (fst) element of the list, fails silently if the value is outside the range
accumulateListIO :: Histogram -> [(Double,Double)] -> IO ()
accumulateListIO (H _ h) xs = do
                             withForeignPtr h (\f -> mapM_ (\(x,w) -> histogram_accumulate f x w) xs)
                             return ()

-- | add the weight (second vector) to the correct bin for each element of the first vector, fails silently if the value is outside the range
accumulateVectorIO :: Histogram -> Vector Double -> Vector Double -> IO ()
accumulateVectorIO (H _ h) v w = do
                                (v # w # id) (\s1 p1 s2 p2 -> withForeignPtr h (\f -> histogram_accumulate_vector f s1 p1 s2 p2)) #| "accumulateVector"
                                return ()

-- | returns the contents of the i-th bin
getBin :: Histogram -> Int -> Double
getBin (H _ h) b = unsafePerformIO $ do
                 withForeignPtr h (\f -> histogram_get f (fromIntegral b))

-- | returns the upper and lower limits of the i-th bin
getRange :: Histogram -> Int -> (Double,Double)
getRange (H _ h) b = unsafePerformIO $ do
                   alloca $ \l ->
                       alloca $ \u -> do
                       check "get_range" $ withForeignPtr h (\f -> histogram_get_range f (fromIntegral b) l u)
                       l' <- peek l
                       u' <- peek u
                       return (l',u')

-- | the maximum upper range limit
getMax :: Histogram -> Double
getMax (H _ h) = unsafePerformIO $ withForeignPtr h histogram_max

-- | the minimum lower range limit
getMin :: Histogram -> Double
getMin (H _ h) = unsafePerformIO $ withForeignPtr h histogram_min

-- | the number of bins
getBins :: Histogram -> Int
getBins (H b _) = b

-- | reset all the bins to zero
reset :: Histogram -> IO ()
reset (H _ h) = withForeignPtr h histogram_reset

foreign import ccall "gsl-histogram.h gsl_histogram_increment" histogram_increment :: HistHandle -> Double -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_accumulate" histogram_accumulate :: HistHandle -> Double -> Double -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_get" histogram_get :: HistHandle -> CInt -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_get_range" histogram_get_range :: HistHandle -> CInt -> Ptr Double -> Ptr Double -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_max" histogram_max :: HistHandle -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_min" histogram_min :: HistHandle -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_bins" histogram_bins :: HistHandle -> IO Int
foreign import ccall "gsl-histogram.h gsl_histogram_reset" histogram_reset :: HistHandle -> IO ()

foreign import ccall "histogram-aux.h increment_vector" histogram_increment_vector :: HistHandle -> CInt -> Ptr Double -> IO CInt
foreign import ccall "histogram-aux.h accumulate_vector" histogram_accumulate_vector :: HistHandle -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | find the bin corresponding to the value
find :: Histogram -> Double -> Maybe Int
find (H _ h) x = unsafePerformIO $ do
               alloca $ \b -> do
                  err <- withForeignPtr h (\f -> histogram_find f x b)
                  if err == 0
                     then do
                          b' <- peek b
                          return $ Just $ fromIntegral b'
                     else return Nothing

foreign import ccall "gsl-histogram.h gsl_histogram_find" histogram_find :: HistHandle -> Double -> Ptr CInt -> IO CInt

-- | find the number of occurences for the input
countInstance :: Histogram -> Double -> Double
countInstance h x = let Just x' = find h x in getBin h x'

-- | find the probability of the input
probability :: Histogram -> Double -> Double
probability h x = let Just x' = find h x in (getBin h x') / (sum h)

-- | find the number of occurences for each element of the input vector
count :: Histogram -> Vector Double -> Vector Double
count h = cmap (\x -> let Just x' = find h x
                      in getBin h x')

-- | find the probability of occurring for each element of the input vector
prob :: Histogram -> Vector Double -> Vector Double
prob h x = (count h x) / (scalar $ sum h)

-----------------------------------------------------------------------------

-- | the maximum value contained in the bins
maxVal :: Histogram -> Double
maxVal (H _ h) = unsafePerformIO $ withForeignPtr h histogram_max_val

-- | the index of the bin containing the maximum value
maxBin :: Histogram -> Int
maxBin (H _ h) = unsafePerformIO $ do
           j <- withForeignPtr h histogram_max_bin
           return $ fromIntegral j

-- | the minimum value contained in the bins
minVal :: Histogram -> Double
minVal (H _ h) = unsafePerformIO $ withForeignPtr h histogram_min_val

-- | the index of the bin containing the minimum value
minBin :: Histogram -> Int
minBin (H _ h) = unsafePerformIO $ do
           j <- withForeignPtr h histogram_min_bin
           return $ fromIntegral j

-- | the mean of the values, accuracy limited by bin width
mean :: Histogram -> Double
mean (H _ h) = unsafePerformIO $ withForeignPtr h histogram_mean

-- | the standard deviation of the values, accuracy limited by bin width
stddev :: Histogram -> Double
stddev (H _ h) = unsafePerformIO $ withForeignPtr h histogram_sigma

-- | the sum of the values, accuracy limited by bin width
sum :: Histogram -> Double
sum (H _ h) = unsafePerformIO $ withForeignPtr h histogram_sum

foreign import ccall "gsl-histogram.h gsl_histogram_max_val" histogram_max_val :: HistHandle -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_max_bin" histogram_max_bin :: HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_min_val" histogram_min_val :: HistHandle -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_min_bin" histogram_min_bin :: HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_mean" histogram_mean :: HistHandle -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_sigma" histogram_sigma :: HistHandle -> IO Double
foreign import ccall "gsl-histogram.h gsl_histogram_sum" histogram_sum :: HistHandle -> IO Double

-----------------------------------------------------------------------------

-- | returns True of all the individual bin ranges of the two histograms are identical
equalBins :: Histogram -> Histogram -> Bool
equalBins (H _ h1) (H _ h2) = unsafePerformIO $ do
                          j <- withForeignPtr h1 $ \p1 -> do
                               withForeignPtr h2 $ \p2 -> histogram_equal_bins p1 p2
                          if (fromIntegral j) == (1 :: Int)
                             then return True
                             else return False

-- | adds the contents of the bins of the second histogram to the first
add :: Histogram -> Histogram -> Histogram
add d (H _ s) = unsafePerformIO $ do
          h@(H _ d') <- cloneHistogram d
          check "add" $
             withForeignPtr d' $ \dp ->
                 withForeignPtr s $ \sp -> histogram_add dp sp
          return h

-- | subtracts the contents of the bins of the second histogram from the first
subtract :: Histogram -> Histogram -> Histogram
subtract d (H _ s) = unsafePerformIO $ do
               h@(H _ d') <- cloneHistogram d
               check "subtract" $
                  withForeignPtr d' $ \dp ->
                      withForeignPtr s $ \sp -> histogram_sub dp sp
               return h

-- | multiplies the contents of the bins of the second histogram by the first
multiply :: Histogram -> Histogram -> Histogram
multiply d (H _ s) = unsafePerformIO $ do
               h@(H _ d') <- cloneHistogram d
               check "multiply" $
                  withForeignPtr d' $ \dp ->
                      withForeignPtr s $ \sp -> histogram_mul dp sp
               return h

-- | divides the contents of the bins of the first histogram by the second
divide :: Histogram -> Histogram -> Histogram
divide d (H _ s) = unsafePerformIO $ do
             h@(H _ d') <- cloneHistogram d
             check "divide" $
                withForeignPtr d' $ \dp ->
                    withForeignPtr s $ \sp -> histogram_div dp sp
             return h

-- | multiplies the contents of the bins by a constant
scale :: Histogram -> Double -> Histogram
scale d s = unsafePerformIO $ do
            h@(H _ d') <- cloneHistogram d
            check "scale" $
               withForeignPtr d' $ (\f -> histogram_scale f s)
            return h

-- | adds a constant to the contents of the bins
shift :: Histogram -> Double -> Histogram
shift d s = unsafePerformIO $ do
            h@(H _ d') <- cloneHistogram d
            check "shift" $
               withForeignPtr d' $ (\f -> histogram_shift f s)
            return h

foreign import ccall "gsl-histogram.h gsl_histogram_equal_bins_p" histogram_equal_bins :: HistHandle -> HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_add" histogram_add :: HistHandle -> HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_sub" histogram_sub :: HistHandle -> HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_mul" histogram_mul :: HistHandle -> HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_div" histogram_div :: HistHandle -> HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_scale" histogram_scale :: HistHandle -> Double -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_shift" histogram_shift :: HistHandle -> Double -> IO CInt

-----------------------------------------------------------------------------

-- | write a histogram in the native binary format (may not be portable)
fwriteHistogram :: FilePath -> Histogram -> IO ()
fwriteHistogram fn (H _ h) = do
                       cn <- newCString fn
                       check "fwriteHistogram" $
                          withForeignPtr h $ histogram_fwrite cn
                       free cn

-- | read a histogram in the native binary format, number of bins must be known
freadHistogram :: FilePath -> Int -> IO Histogram
freadHistogram fn b = do
                      h <- histogram_new (fromIntegral b)
                      h' <- newForeignPtr histogram_free h
                      cn <- newCString fn
                      check "freadHistogram" $
                         withForeignPtr h' $ histogram_fread cn
                      return $ H b h'
                      
-- | saves the histogram with the given formats (%f,%e,%g) for ranges and bins
--   each line comprises: range[i] range[i+1] bin[i]
fprintfHistogram :: FilePath -> String -> String -> Histogram -> IO ()
fprintfHistogram fn fr fb (H _ h) = do
                                  cn <- newCString fn
                                  cr <- newCString fr
                                  cb <- newCString fb
                                  check "fprintfHistogram" $
                                     withForeignPtr h $ histogram_fprintf cn cr cb
                                  free cn
                                  free cr
                                  free cb
                                  return ()

-- | reads formatted data as written by fprintf, the number of bins must be known in advance
fscanfHistogram :: FilePath -> Int -> IO Histogram
fscanfHistogram fn b = do
                       h <- histogram_new (fromIntegral b)
                       h' <- newForeignPtr histogram_free h
                       cn <- newCString fn
                       check "fscanfHistogram" $
                          withForeignPtr h' $ histogram_fscanf cn
                       return $ H b h'

foreign import ccall "histogram-aux.h hist_fwrite" histogram_fwrite :: Ptr CChar -> HistHandle -> IO CInt
foreign import ccall "histogram-aux.h hist_fread" histogram_fread :: Ptr CChar -> HistHandle -> IO CInt
foreign import ccall "histogram-aux.h hist_fprintf" histogram_fprintf :: Ptr CChar -> Ptr CChar -> Ptr CChar -> HistHandle -> IO CInt
foreign import ccall "histogram-aux.h hist_fscanf" histogram_fscanf :: Ptr CChar -> HistHandle -> IO CInt
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

foreign import ccall "gsl-histogram.h gsl_histogram_pdf_alloc" histogram_pdf_new :: CInt -> IO PDFHandle
foreign import ccall "gsl-histogram.h &gsl_histogram_pdf_free" histogram_pdf_free :: FunPtr (PDFHandle -> IO ())

-----------------------------------------------------------------------------

-- | create a histogram PDF from a histogram
fromHistogram :: Histogram -> HistogramPDF
fromHistogram (H b h) = unsafePerformIO $ do
                        p <- histogram_pdf_new $ fromIntegral b 
                        p' <- newForeignPtr histogram_pdf_free p
                        withForeignPtr p' $ \p'' -> 
                            withForeignPtr h $ \h' -> check "pdf_init" $ histogram_pdf_init p'' h'
                        return $ P p'

-- | given a random number from the uniform distribution [0,1], draw a random sample from the PDF
sample :: HistogramPDF -> Double -> Double
sample (P p) r = unsafePerformIO $ withForeignPtr p $ \p' -> histogram_pdf_sample p' r

foreign import ccall "gsl-histogram.h gsl_histogram_pdf_init" histogram_pdf_init :: PDFHandle -> HistHandle -> IO CInt
foreign import ccall "gsl-histogram.h gsl_histogram_pdf_sample" histogram_pdf_sample :: PDFHandle -> Double -> IO Double

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
