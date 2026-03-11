-- |
-- Module      :  Numeric.Stats
-- Copyright   :  (c) OleksandrZhabenko 2020-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A very basic descriptive statistics. Functions use a tail recursion approach to compute the values and are strict by an accumulator.

{-# LANGUAGE BangPatterns, MagicHash, NoImplicitPrelude #-}

module Numeric.Stats where

import GHC.Exts
import GHC.Prim
import GHC.Base
import GHC.Real
import GHC.Float
import GHC.Num

-- | Uses GHC unlifted types from @ghc-prim@ package.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
--
mean2F :: [Float] -> Float# -> Int# -> Float
mean2F ((F# !x):xs) !s1 !l1 = mean2F xs (plusFloat# s1 x) (l1 +# 1#)
mean2F _ !s1 !l1 =
 case I# l1 of
  0 -> error "Numeric.Stats.mean2F: Not defined for the third zero argument. "
  _  -> F# m
   where !m = divideFloat# s1 (int2Float# l1)

-- | Uses GHC unlifted types from @ghc-prim@ package.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
--
mean2D :: [Double] -> Double# -> Int# -> Double
mean2D ((D# !x):xs) !s1 !l1 = mean2D xs (s1 +## x) (l1 +# 1#)
mean2D _ !s1 !l1 =
 case I# l1 of
  0 -> error "Numeric.Stats.mean2D: Not defined for the third zero argument. "
  _  -> D# m
   where !m = s1 /## int2Double# l1

-- | One-pass and tail-recursive realization for the pair of the mean and dispersion. Is vulnerable to the floating-point cancellation errors.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
-- When using the needed, please, refer better to their variants.
--
-- Among the 'meanWithDispersion', 'meanWithDisprsionF' and 'meanWithDispersionD' better to use the last one.
meanWithDispersionP :: (RealFrac a, Floating a) => [a] -> a -> a -> a -> a -> a -> (a,a)
meanWithDispersionP (!x:xs) !s1 !s2 !l1 m1 d = meanWithDispersionP xs (s1 + x) (s2 + x*x) (l1 + 1) (m0 s1 l1 x) (m0 s2 l1 (x*x) - (m0 s1 l1 x)**2)
  where m0 !s3 !l2 !x = (s3 + x) / (l2 + 1)
meanWithDispersionP _ _ _ _ !m !d = (m,d)

-- | Among the 'meanWithDispersion', 'meanWithDisprsionF' and 'meanWithDispersionD' better to use the last one.
meanWithDispersionFP :: [Float] -> Float# -> Float# -> Int# -> (Float,Float)
meanWithDispersionFP ((F# !x):xs) !s1 !s2 !l1 = meanWithDispersionFP xs (plusFloat# s1 x) (plusFloat# s2 (timesFloat# x x)) (l1 +# 1#)
meanWithDispersionFP [] !s1 !s2 !l1 = (F# m, F# (minusFloat# (divideFloat# s2 (int2Float# l1)) (timesFloat# m m)))
  where !m = divideFloat# s1 (int2Float# l1)

-- | Among the 'meanWithDispersion', 'meanWithDisprsionF' and 'meanWithDispersionD' better to use the last one.
meanWithDispersionDP :: [Double] -> Double# -> Double# -> Int# -> (Double,Double)
meanWithDispersionDP ((D# !x):xs) !s1 !s2 !l1 = meanWithDispersionDP xs (s1 +## x) (s2 +## (x *## x)) (l1 +# 1#)
meanWithDispersionDP [] !s1 !s2 !l1 = (D# m, D# ((s2 /## int2Double# l1) -## (m *## m)))
  where !m = s1 /## int2Double# l1

-- | Uses 'mean2F' inside.
meanF :: [Float] -> Float
meanF xs = mean2F xs 0.0# 0#
{-# INLINE meanF #-}

meanD :: [Double] -> Double
meanD xs = mean2D xs 0.0## 0#

-- | Among the 'meanWithDispP', 'meanWithDispF2P' and 'meanWithDispD2P' better to use the last one.
meanWithDispP :: (RealFrac a, Floating a) => [a] -> (a,a)
meanWithDispP xs@(_:_) = meanWithDispersionP xs 0.0 0.0 0.0 0.0 0.0
meanWithDispP _ = error "Numeric.Stats.meanWithDispP: Not defined for the empty list. "
{-# RULES "realfrac/float" meanWithDispP = meanWithDispF2P #-}
{-# RULES "realfrac/double" meanWithDispP = meanWithDispD2P #-}
{-# INLINE[2] meanWithDispP #-}

-- | Among the 'meanWithDispP', 'meanWithDispF2P' and 'meanWithDispD2P' better to use the last one.
meanWithDispF2P :: [Float] -> (Float,Float)
meanWithDispF2P xs@(_:_) = meanWithDispersionFP xs 0.0# 0.0# 0#
meanWithDispF2P _ = error "Numeric.Stats.meanWithDispF2P: Not defined for the empty list. "
{-# INLINE meanWithDispF2P #-}

-- | Among the 'meanWithDispP', 'meanWithDispF2P' and 'meanWithDispD2P' better to use the last one.
meanWithDispD2P :: [Double] -> (Double,Double)
meanWithDispD2P xs@(x:_) = meanWithDispersionDP xs 0.0## 0.0## 0#
meanWithDispD2P _ = error "Numeric.Stats.meanWithDispD2P: Not defined for the empty list. "
{-# INLINE meanWithDispD2P #-}

--------------------------------------------------

-- Inspired by: https://www.khanacademy.org/math/ap-statistics/summarizing-quantitative-data-ap/more-standard-deviation/v/simulation-showing-bias-in-sample-variance
-- and:
-- https://www.khanacademy.org/math/ap-statistics/summarizing-quantitative-data-ap/more-standard-deviation/v/simulation-providing-evidence-that-n-1-gives-us-unbiased-estimate



-- | One-pass and tail-recursive realization for the pair of the mean and dispersion. Is vulnerable to the floating-point cancellation errors.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
-- When using the needed, please, refer better to their variants.
-- Among the 'meanWithDisp', 'meanWithDispF2' and 'meanWithDispD2' better to use the last one.
meanWithDisp :: (RealFrac a, Floating a) => [a] -> (a,a)
meanWithDisp xs@(_:_:_) = mdl xs 0.0 0.0 0.0
  where mdl (!x:ys) !s1 !s2 !l1 = mdl ys (s1 + x) (s2 + x*x) (l1 + 1)
        mdl _ !s1 !s2 !l = (mm, (s2 - mm**2 * l) / (l - 1))
          where !mm = s1 / l
meanWithDisp _ = error "Numeric.Stats.meanWithDisp: Not defined for the list with less than two elements. "
{-# RULES "realfrac/float" meanWithDisp = meanWithDispF2 #-}
{-# RULES "realfrac/double" meanWithDisp = meanWithDispD2 #-}
{-# INLINE[2] meanWithDisp #-}

-- | One-pass and tail-recursive realization for the pair of the mean and dispersion. Is vulnerable to the floating-point cancellation errors.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
-- When using the needed, please, refer better to their variants.
-- Among the 'meanWithDisp', 'meanWithDispF2' and 'meanWithDispD2' better to use the last one.
meanWithDispF2 :: [Float] -> (Float,Float)
meanWithDispF2 xs@(_:_:_) = mdlF xs 0.0# 0.0# 0#
  where mdlF (F# !x:ys) !s1 !s2 !l1 = mdlF ys (plusFloat# s1 x) (plusFloat# s2 (timesFloat# x x)) (l1 +# 1#)
        mdlF [] !s1 !s2 !l1 = (F# m, F# (divideFloat# (minusFloat# s2 (timesFloat# (timesFloat# m m) (int2Float# l1))) (int2Float# (l1 -# 1#))))
          where !m = divideFloat# s1 (int2Float# l1)
meanWithDispF2 _ = error "Numeric.Stats.meanWithDispF2: Not defined for the list with less than two elements. "
{-# INLINE meanWithDispF2 #-}

-- | One-pass and tail-recursive realization for the pair of the mean and dispersion. Is vulnerable to the floating-point cancellation errors.
-- Similar code is here: Don Stewart.  https://donsbot.wordpress.com/2008/05/06/write-haskell-as-fast-as-c-exploiting-strictness-laziness-and-recursion/
-- And here: http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-- And here: Michael Snoyman. https://www.fpcomplete.com/blog/2017/09/all-about-strictness/
-- When using the needed, please, refer better to their variants.
-- Among the 'meanWithDisp', 'meanWithDispF2' and 'meanWithDispD2' better to use the last one.
meanWithDispD2 :: [Double] -> (Double,Double)
meanWithDispD2 xs@(_:_:_) = mdlD xs 0.0## 0.0## 0#
  where mdlD ((D# !x):xs) !s1 !s2 !l1 = mdlD xs (s1 +## x) (s2 +## (x *## x)) (l1 +# 1#)
        mdlD [] !s1 !s2 !l1 = (D# m, D# ((s2 -## m *## m *## int2Double# l1) /## (int2Double# (l1 -# 1#))))
          where !m = s1 /## int2Double# l1
meanWithDispD2 _ = error "Numeric.Stats.meanWithDispD2: Not defined for the list with less than two elements. "
{-# INLINE meanWithDispD2 #-}
