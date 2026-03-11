{-|
Module : Math.List.Wavelet
Copyright : (C) Dominick Samperi 2023
License : BSD3
Maintainer : djsamperi@gmail.com
Stability : experimental

Wavelets can be viewed as an alternative to the usual Fourier
basis (used in the Fourier transform) that supports analyzing
functions at different scales. Frequency information is also
captured, though not as readily as this is done with the
Fourier transform. A powerful feature of the wavelet transform is
that in many problems it leads to a sparse representation. This
can be exploited to reduce computational complexity (when solving
differential equations, say) or to implement improved data
compression (in image analysis, for example).

Only the 1D case is implemented here. Extension to higher dimensions
is straightforward, but the list implementation used here may not
be appropriate for this purpose. This implementation is suitable
for small or moderate sized 1D problems, and for understanding the
underlying theory which does not change as the dimension increases.

References on wavelets with abbreviations:

[@Compact1988@]: /Orthogonal Bases of Compactly Supported Wavelets/,
by Ingrid Daubechies, Communications on Pure and Applied Mathematics,
Vol XLI, 909-996 (1988).
[@TenLectures@]: /Ten Lectures on Wavelets/ by Ingrid Daubechies, SIAM (1992).
[@TourBook@]: /A Wavelet Tour of Signal Processing: The Sparse Way/, Third Edition, by Stephane Mallat, with Gabriel Peyre' (2009).
[@Data2021@]: /Mathematical Foundations of Data Sciences/ by Gabriel Peyre' (2021).
[@NumericalTours@]: Websites <https://mathematical-tours.github.io> and
<https://www.numerical-tours.com>.

-}
module Math.List.Wavelet (
  wt1d,
  iwt1d,
  cconv1d, -- circular convolution with centering
  cconv1dnc,-- no centering (standard)
  conv1d,  -- standard convolution with zero padding
  deltaFunc) where

import Data.Complex
import Data.List

-- |'wt1d' is the 1-dimensional discrete wavelet transform.
--
-- > Usage: wt1d x nd jmin jmax
--
-- * `x` - input vector of size \( M = 2^{jmax} \).
-- * `nd` - Daubechies wavelet identifier \( N \), where \( 2 \leq N \leq 10 \).
-- * `jmin` - Last coarse vector has size \( 2^{jmin} \).
-- * `jmax` - input vector size is \( 2^{jmax} \).
--
-- The Daubechies wavelets are defined in __TenLectures__.
-- The input vector is the initial 'detail' vector.
-- The first level of the transform splits this vector into two
-- parts, 'coarse' (coarse coefficients---see below) 
-- and 'detail' (detail coefficients). Then the 'coarse'
-- vector is split into two parts, one 'coarse' and the other 'detail'
-- (the previous 'detail' vector is unchanged). This continues until
-- the 'coarse' vector is reduced to size \( 2^{jmin} \), where
-- \( jmax > jmin \ge 0 \). The output vector returned by this function has
-- the same size as the input vector, and it
-- has the form \( (coarse, detail, d, d', d'',...) \), where 'coarse' is the
-- last 'coarse' vector, 'detail' is its companion, and d, d', d'', etc.
-- are detail vectors from previous steps, increasing in size by factors
-- of 2. The inverse wavelet transform 'iwt1d' starts with this vector
-- and works backwards to recover the original input vector. Of course,
-- it needs jmin to know where to start. See Examples below.
--
-- Here are the fundamental relations derived in __Compact1988__ that 
-- define the discrete compactly supported wavelet transform and its inverse 
-- \[
-- a_k^{j+1} = \sum_n h(n-2k) a_n^j,\qquad d_k^{j+1} = \sum_n g(n-2k) a_n^j, 
-- \]
-- where \( a_k^j \) and \( d_k^j \) are the approximation and detail 
-- coefficients, respectively. See __Compact1988__, pp.935-938. The 
-- reverse or inverse DWT is defined by 
-- \[ 
-- a_n^{j-1} = \sum_k h(n-2k) a_k^j + \sum_k g(n-2k) d_k^j. 
-- \]
-- It is clear from this that decimation by two and convolution is involved 
-- at each step. More precisely, these equations and the corresponding 
-- Haskell implementation makes it clear that
-- in the forward transform the filters 
-- are reversed, convolved with the input (prior level coefficients), 
-- and subsampled, while in the reverse transform, the prior level 
-- coefficients are upsampled, convolved with the filters (not reversed), 
-- and summed. Implementations in Python, R, Julia and Matlab can
-- be found in __Data2021__ and __NumericalTours__.
--
-- Note that much of the analysis in __Compact1988__ is 
-- done where n varies over all integers. In the implementation 
-- below we work modulo the size of the input vector, which turns 
-- ordinary convolutions into circular convolutions in the usual way.
--
-- The \( N \)-th Daubechies wavelet filter h has support \( [0,2N-1] \)
-- (even number of points). See __Compact1988__, Table 1, p.980, 
-- and __TenLectures__, Table 6.1, p.195 (higher precision). The 
-- novelty of this work was the discovery of invertible filters with 
-- compact support (the Shannon wavelet does not have 
-- compact support). This requires detailed Fourier analysis, from 
-- which it is deduced that the conjugate filter g can be chosen 
-- to satisfy \( g(k) = (-1)^k h(2p+1-k) \) for a convenient choice 
-- for p (see __TenLectures__, p.136).
-- To define g with the same support as h, for the N-th wavelet, 
-- use p = N-1, so \( g(k) = (-1)^k h(2N-1-k), k=0,1,...,2N-1 \).
--
-- Following __NumericalTours__, we define the circular convolution
-- 'cconv1d' in such a way that the filter h is centered, and for
-- this purpose a zero is prepended to both h and g (so these vectors
-- have an odd number of points, with the understanding that their
-- support does not change).
--
-- === __Examples:__
--
-- >>> dist :: [Double] -> [Double] -> Double
-- >>> dist x y = sqrt (sum (zipWith (\x y -> (x-y)^2) x y))
-- >>> sig = deltaFunc 5 1024
-- >>> forward = wt1d sig 10 5 10 -- wavelet 10, jmin=5, jmax=10
-- >>> backward = iwt1d forward 10 5 10
-- >>> dist sig backward
--
-- > 1.2345e-12
--
-- Let's take a look at the simplest Daubechies wavelet (N=2) by taking the
-- inverse transform of a delta function at position 5...
--
-- >>> sig = deltaFunc 5 1024
-- >>> wavelet2 = iwt1d sig 2 0 10
-- >>> [rgraph| plot(wavelet2_hs,type='l') |]
--
-- ![Wavelet2](https://humangarden.net/images/Wavelet2.png)
--
-- Let's place masses at positions 100 and 400, with the
-- first twice as large as the second.
--
-- >>> x = deltaFunc 100 1024
-- >>> y = deltaFunc 400 1024
-- >>> z = zipWith (\x y -> 2*x + y) x y
-- >>> wt = wt1d z 2 0 10
-- >>> [rgraph| plot(wt_hs,type='l') |]
--
-- ![TwoSpikes](https://humangarden.net/images/TwoSpikes.png)
--
-- The spikes show up in the scalogram at about 1/2 the distance
-- in scale units, and there are "harmonics" at the coarser scales
-- (on the left).
--
wt1d :: [Double] -> Int -> Int -> Int -> [Double]
wt1d x nd jmin jmax = if jmin==jmax then x
                     else wt1d coarse nd jmin (jmax-1) ++ detail
  where h = 0.0 : hcoef!!(nd-2)
        n = length h
        g = 0.0 : [z | k <- [0..(n-2)], let z = (-1)^k * h!!((n-1-k) `mod` n)]
        coarse = subsample $ cconv1d (reverse h) x
        detail = subsample $ cconv1d (reverse g) x
        
-- |'iwt1d' is the 1-dimensional inverse discrete wavelet transform.
--
-- > Usage: iwt1d x nd jmin jmax
--
-- * `x` - The output from 'wt1d` when the last coarse vector has size \( 2^{jmin} \), where \( 0 \leq jmin \lt jmax \).
-- * `nd` - Daubechies wavelet identifier \( N \), where \( 2 \leq N \leq 10 \).
-- * `jmin` - Last coarse vector has size \( 2^{jmin} \).
-- * `jmax` - Output vector has size is \( 2^{jmax} \).
--
-- Reverses the steps taken in 'wt1d' above.
--
iwt1d :: [Double] -> Int -> Int -> Int -> [Double]
iwt1d x nd jmin jmax = if jmin==jmax then x
                      else iwt1d x' nd (jmin+1) jmax
  where h = 0.0 : hcoef!!(nd-2)
        n = length h
        g = 0.0 : [z | k <- [0..(n-2)], let z = (-1)^k * h!!((n-1-k) `mod` n)]
        coarse = take (2^jmin) x
        detail = drop (2^jmin) (take (2*2^jmin) x)
        coarse' = cconv1d h $ upsample coarse
        detail' = cconv1d g $ upsample detail
        x' = zipWith (+) coarse' detail' ++ drop (2*2^jmin) x

-- | 'deltaFunc' creates a list of zero's except for one element that equals 1
--        
-- > Usage: deltaFunc k n
--        
-- * `n` - length of the list
-- * `k` - position that equals 1 (zero-based, k < n).
--
deltaFunc :: Num a => Int -> Int -> [a]
deltaFunc k n = take k l ++ [1] ++ drop (k+1) l
  where l = replicate n 0
        
subsample :: (Num a) => [a] -> [a]
subsample [x,y] = [x]
subsample (x:(y:xs)) = x:subsample xs

upsample :: (Num a) => [a] -> [a]
upsample [] = [0]
upsample (x:[]) = [x,0]
upsample (x:xs) = (x:(0:upsample xs))

-- |'cconv1d' is the 1-dimensional circular convolution (with centering).
--
-- > Usage: cconv1d h x        
--        
-- * `x` - input signal        
-- * `h` - filter to convolve with.
--        
-- Let 'N' = length x, and 'M' = length h, and assume \( M \leq N \). The
-- convolution with centering offset 'p' is defined by
-- \[
-- y_i = \sum_{j=0}^{M-1} h_j x_{i-j+p},\qquad i=0,1,2,...,N-1,
-- \]
-- where \( p = (M-1)/2 \) when M is odd. The idea is to have
-- each \( y_i \) equal a weighted average of values of \( x_j \)
-- for \( j \) near \( i \), because \( i \) is in the middle of
-- the support of \( h \), approximately.
--
-- For this to be well-defined for the specified range of \( i \),
-- \( x_i \) must be extended periodically outside of the
-- range \( 0 \leq i \leq N-1 \), like this...
-- \[
-- x_{J} \cdots x_{N-1} x_0 x_1 \cdots x_{N-1} x_0 x_1 \cdots x_{p-1},
-- \]
-- where \( J \) is determined by the condition that we need
-- \( M-1-p \) extra elements on the left, so we have
-- \( (N-1) - J + 1 = (M-1) - p \), and \( J = N-M+1+p \). Since subscripts
-- begin with 0, we must drop the first \( J \) elements of \( x \) to get the
-- padding on the left.
--
-- Consequently, padding on the left is determined by dropping \( N-M+1+p \) elements of \( x \), and
-- padding on the right is determined by taking the first p elements of \( x \). The
-- circular convolution is then found by breaking the extended list into sublists of
-- length \( M \), and taking the inner produce of each of these sublists with
-- \( h \) reversed (see source code).
--
-- === __Examples:__
--
-- >>> cconv1d [1..4] [1..4]
--
-- > [28,26,20,26]
--
-- Agrees with implementation in __NumericalTours__, where circular
-- convolutions are centered.
--
cconv1d :: (Num a) => [a] -> [a] -> [a]
cconv1d hs xs =
  let m = length hs
      n = length xs
      p = (m-1) `div` 2
      padLeft  = drop (n-m+1+p) xs
      padRight = take p xs
      ts  = padLeft ++ xs ++ padRight
  in map (sum . zipWith (*) (reverse hs)) (sublists m ts)

-- |'cconv1dnc' standard circular convolution without centering.
-- Same as 'cconv1d' with \( p = 0 \). No padding on the right
-- is needed in this case.
--        
-- === __Examples:__
--
-- >>> cconv1dnc [1..4] [1..4]
--
-- > [26,28,26,20]
--
-- > R equivalent: convolve(1:4,1:4,type='circular',conj=FALSE)
-- Circular convolutions are not centered in R.
--
cconv1dnc :: (Num a) => [a] -> [a] -> [a]
cconv1dnc hs xs =
  let m = length hs
      n = length xs
      padLeft  = drop (n-m+1) xs
      ts  = padLeft ++ xs
  in map (sum . zipWith (*) (reverse hs)) (sublists m ts)

-- |'conv1d' is the 1-dimensional conventional convolution (no FFT).
--        
-- > Usage: conv1d h x        
--        
-- * `x` - input signal        
-- * `h` - filter to convolve with.        
-- 
-- === __Examples:__
--
-- >>> conv1d [1..4] [1..4]
--
-- > [1,4,10,20,25,24,16]
--
-- Standard (non-circular) convolution with zero-padding...
--
-- > R equivalent: convolve(1:4,rev(1:4), type='open')
-- > Python: np.convolve([1,2,3,4], [1,2,3,4]
-- > Octave/Matlab: conv(1:4,1:4)
--
conv1d :: (Num a) => [a] -> [a] -> [a]
conv1d hs xs =
  let pad = replicate ((length hs) - 1) 0
      ts  = pad ++ xs
  in map (sum . zipWith (*) (reverse hs)) (init $ tails ts)                    

-- Get list of sublists of a fixed length, with no short ones.
-- Second pattern for go kicks in when the second argument is
-- a list with a single element. This avoids filtering out
-- short lists using length conditional, and works for infinite
-- lists.
sublists :: (Num a) => Int -> [a] -> [[a]]
sublists k lst = go lst (drop (k-1) lst)
  where go l@(_:lt) (_:xs) = take k l : go lt xs
        go _ _ = []
        
-- Shorter and less transparent version of sublists
-- From stackoverflow (Willem Van Onsem)
-- Here we are applying zipWith (const . take k) to (tails lst)        
-- and (drop (k-1) lst). The latter list has n-k elements, and
-- we don't care what they are, thanks to the const. These elements        
-- determine how many items from (tails lst) are actually taken: only
-- items of length k are taken!
sublists' :: Int -> [a] -> [[a]]
sublists' k lst = zipWith (const . take k) (tails lst) (drop (k-1) lst)

-- From Daubechies Ten Lectures (p.195)
-- For each N, g(k) = (-1)^k * h(2N + 1 - k)
hcoef = [
-- N=2
  [ 0.4829629131445341
  , 0.8365163037378077
  , 0.2241438680420134
  ,-0.1294095225512603
  ],
-- N=3 (6 coefs)
  [ 0.3326705529500825
  , 0.8068915093110924
  , 0.4598775021184914
  ,-0.1350110200102546
  ,-0.0854412738820267
  , 0.0352262918857095
  ],
-- N=4 (8 coefs)
  [ 0.2303778133068964
  , 0.7148465705529154
  , 0.6308807679398587
  ,-0.0279837694168599
  ,-0.1870348117190931
  , 0.0308413818355607
  , 0.0328830116668852
  ,-0.0105974017850690
  ],
-- N=5 (10 coefs)
  [ 0.1601023979741929
  , 0.6038292697971895
  , 0.7243085284377726
  , 0.1384281459013203
  ,-0.2422948870663823
  ,-0.0322448695846381
  , 0.0775714938400459
  ,-0.0062414902127983
  ,-0.0125807519990820
  , 0.0033357252854738
  ],
-- N=6 (12 coefs)
  [ 0.1115407433501095
  , 0.4946238903984533
  , 0.7511339080210959
  , 0.3152503517091982
  ,-0.2262646939654400
  ,-0.1297668675672625
  , 0.0975016055873225
  , 0.0275228655303053
  ,-0.0315820393174862
  , 0.0005538422011614
  , 0.0047772575109455
  ,-0.0010773010853085
  ],
-- N=7
  [ 0.0778520540850037
  , 0.3965393194818912
  , 0.7291320908461957
  , 0.4697822874051889
  ,-0.1439060039285212
  ,-0.2240361849938412
  , 0.0713092192668272
  , 0.0806126091510774
  ,-0.0380299369350104
  ,-0.0165745416306655
  , 0.0125509985560986
  , 0.0004295779729214
  ,-0.0018016407040473
  , 0.0003537137999745
  ],
-- N=8 (16 coefs)
  [ 0.0544158422431072
  , 0.3128715909143166
  , 0.6756307362973195
  , 0.5853546836542159
  ,-0.0158291052563823
  ,-0.2840155429615824
  , 0.0004724845739124
  , 0.1287474266204893
  ,-0.0173693010018090
  ,-0.0440882539307971
  , 0.0139810279174001
  , 0.0087460940474065
  ,-0.0048703529934520
  ,-0.0003917403733770
  , 0.0006754494064506
  ,-0.0001174767841248
  ],
-- N=9 (18 coefs)
  [ 0.0380779473638778
  , 0.2438346746125858
  , 0.6048231236900955
  , 0.6572880780512736
  , 0.1331973858249883
  ,-0.2932737832791663
  ,-0.0968407832229492
  , 0.1485407493381256
  , 0.0307256814793385
  ,-0.0676328290613279
  , 0.0002509471148340
  , 0.0223616621236798
  ,-0.0047232047577518
  ,-0.0042815036824635
  , 0.0018476468830563
  , 0.0002303857635232
  ,-0.0002519631889427
  , 0.0000393473203163
  ],
-- N=10 (20 coefs)
  [ 0.0266700579005473
  , 0.1881768000776347
  , 0.5272011889315757
  , 0.6884590394534363
  , 0.2811723436605715
  ,-0.2498464243271598
  ,-0.1959462743772862
  , 0.1273693403357541
  , 0.0930573646035547
  ,-0.0713941471663501
  ,-0.0294575368218399
  , 0.0332126740593612
  , 0.0036065535669870
  ,-0.0107331754833007
  , 0.0013953517470688
  , 0.0019924052951925
  ,-0.0006858566949564
  ,-0.0001164668551285
  , 0.0000935886703202
  ,-0.0000132642028945
  ]
  ]
