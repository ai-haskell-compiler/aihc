{-|
Module : Math.List.FFT
Copyright : (C) Dominick Samperi 2023
License : BSD3
Maintainer : djsamperi@gmail.com
Stability : experimental

Includes an implementation of the fast Fourier transform and its
inverse using lists. Support for shifting and scaling for easier
interpretation are included, as is computation of the analytic
signal (where spectral power is shifted from negative frequencies
to positive frequencies). The imaginary part of the latter is the
Hilbert transform.

The FFT might be called the "Feasible Fourier Transform" because in
many problems using an input vector size that is not a power of 2
can result in a huge performance penalty (hours instead of seconds),
so it is important to keep this in mind when working with large
input vectors.

The discrete Fourier transform can be viewed as an approximation to
the Fourier integral of a non-periodic function that is very small
outside a finite interval. Alternatively, and more commonly, it
can be viewed as a partial scan of a function that may behave
arbitrarily outside of the scanned (or sampled) points (like an
MRI scan of part of the human body). When we observe below that
the transform is periodic, we are referring to properties of
the mathematical model, not of the signal under study (a periodic
MRI scan does not mean the human body is periodic!).

A periodic function is not integrable over all real numbers, so
it doesn't have a Fourier integral (ignoring weak forms of
convergence), and the Fourier series applies only to periodic
functions, so Fourier series and the Fourier integral are
distinct probes that depend on a particular choice of basis.
The wavelet transform
uses a different choice of basis.

Shannon's sampling theorem tells us that if we sample a
band-limited function fast enough, the function is fully
determined by the sampled values. In other words, the
discrete Shannon wavelet basis is sufficient to represent the
function exactly in this case.

-}
module Math.List.FFT (
  fft,
  ifft,
  ft1d,
  ift1d,
  fftshift,
  fftscale,
  analytic,
  analytic') where

import Data.Complex
import Data.List

-- |Pure and simple fast Fourier transform following the recursive
-- algorithm that appears in /Mathematical Foundations of Data Sciences/ by
-- Gabriel Peyre' (2021). The Haskell implementation is a direct translation
-- of the mathematical specifications. The input vector 
-- size \( N \) must be a power of 2 (the functions 'ft1d' and 'ift1d' work
-- with vectors of any size, but they may be extremely slow for large input vectors).
--
-- Recall that the discrete Fourier transform applied to a 
-- vector \( x \in R^N \) is defined by
-- \[ 
-- \mathbb{F}_N(x)(k) = \sum_{j=0}^{N-1} x_n e^{-2\pi i j k/N},
-- \qquad k=0,1,2,...N-1.
-- \]
--
-- As written this has time complexity \( O(N^2) \), but the fast Fourier
-- transform algorithm reduces this to \( O(N \log N) \). The algorithm
-- can be written as follows
-- \[
-- \mathbb{F}_N(x) = \mathbb{I}_N(\mathbb{F}_{N/2}(x_e),\mathbb{F}_{N/2}(x_o \odot \alpha_N)).
-- \]
-- Here \( \mathbb{F}_{N/2} \) is the Fourier transform defined on 
-- vectors of size  \( N/2 \),
-- \( x_e(n) = x_n + x_{n+N/2} \), and \( x_o(n) = x_n - x_{n+N/2} \), for 
-- \( n=0,1,...,N/2-1 \). Here
-- \( \alpha_N = \exp(-2\pi i/N) \), and \( \mathbb{I}_N \) is the interleaving operator defined by
-- \( \mathbb{I}_N(a,b) = (a_1,b_1,a_2,b_2,...,a_{N/2},b_{N/2}) \). The
-- operator \( \odot \) is defined as follows
-- \[
-- x \odot \alpha_N(k) = x(k) (\alpha_N)^k,\qquad k=0,1,...,N/2-1,
-- \qquad x \in R^{N/2}
-- \]
--
-- The algorithm follows easily by considering the even and odd terms in
-- the discrete Fourier transform. Let \( N' = N/2 \), and let's consider
-- the even terms first:
--
-- \[
-- \begin{eqnarray}
-- X_{2k} &=& \sum_{n=0}^{N-1} x_n e^{-2\pi i n k/N'} \\
-- &=& \sum_{n=0}^{N'-1} x_n e^{-2\pi i n k/N'}
-- + \sum_{n=0}^{N'-1} x_{n+N'} e^{-2\pi i (n+N')k/N'} \\
-- &=& \sum_{n=0}^{N'-1} (x_n + x_{n+N'}) e^{-2\pi i n k/N'}
-- \end{eqnarray}
-- \]
-- The last term is just the Fourier transform of the vector of length
-- \( N/2 \) given by \( x_e(n) = x_n + x_{n+N/2}, n=0,1,...,N/2-1. \)
--
-- For the odd terms we have
--
-- \[
-- \begin{eqnarray*}
-- X_{2k+1} &=& \sum_{n=0}^{N'-1} x_n e^{-2\pi i n (2k+1)/N}
-- + \sum_{n=0}^{N'-1} x_{n+N'} e^{-2\pi i (2k+1) (n+N/2)/N} \\
-- &=& \sum_{n=0}^{N'-1} x_n e^{-2\pi i n k/N'} e^{-2\pi i n/N}
-- + \sum_{n=0}^{N'-1} x_{n+N'} e^{-2\pi i (2kn + n + kN + N/2)N} \\
-- &=&
-- \sum_{n=0}^{N'-1} (x_n - x_{n+N'})e^{-2\pi i n/N} e^{-2\pi i n k/N'}
-- \end{eqnarray*}
-- \]
-- The last term is just the Fourier transform of the vector of
-- length \( N/2 \) given by 
-- \( \tilde{x}_n = (x_n - x_{n+N/2})e^{-2\pi i n/N}, n=0,1,...,N/2-1. \)
--
-- The recursive algorithm now follows by interleaving the even
-- and the odd terms.
--
-- === __Examples:__
-- >>> fft [1,2,3,4]
-- 
-- > [10.0 :+ 0.0, (-2.0) :+ 2.0, (-2.0) :+ 0.0, (-1.99999) :+ (-2.0)]
--
-- Check reproduction property with N=4
--
-- >>> z = fft [1,2,3,4]
-- >>> map (realPart . (/4)) $ ifft z
--
-- > [1.0,2.0,3.0,4.0]
--
-- Test on a mixture of two sine waves...
-- Evalutate a mixture of two sine waves on n equally-spaced points
--
-- >>> n = 1024 -- sample points (a power of 2)
-- >>> dt = 10.24/n -- time interval for Fourier integral approximation.
-- >>> df = 1/dt/n -- frequency increment (df x dt = 1/n)
-- >>> fs = 1/dt -- sampling rate (sampling theorem requires fs >= 2*BW)
-- >>> f1 = 20 -- signal is a mixture of frequencies 20 and 30
-- >>> f2 = 30
-- >>> signal t = sin (2*pi*f1*t) + 0.5* sin(2*pi*f2*t)
-- >>> t = take n $ iterate (+ dt) 0
-- >>> f = take n $ iterate (+ df) 0
-- >>> y = map ((:+ 0.0) . signal) t -- apply signal and complexify
-- >>> z = fft y -- Fourier transform
-- >>> mags = map magnitude z -- modulus of the complex numbers
-- >>> [rgraph| plot(f_hs, mags_hs,type='l') |] -- show plot using HaskellR
--
-- ![TwoSines](https://humangarden.net/images/TwoSines.png)
--
-- Notice that by default the zero frequency point is on the left edge
-- of the chart. To bring the zero frequency point to the center of
-- the chart see 'fftshift' and its examples.
fft :: [Complex Double] -> [Complex Double]
fft [z] = [z]
fft f = interleave [fft fe,fft fo']
  where n = length f
        n' = if odd n then 
               error "fft: Input vector length must be a power of 2" 
             else n `div` 2
        fe = zipWith (+) (take n' f) (rotate n' f)
        fo = zipWith (-) (take n' f) (rotate n' f)
        e = iterate (*alpha) 1.0
        fo' = zipWith (*) fo e
        alpha = exp(-2*pi*i/fromIntegral n)
        interleave = concat . transpose
        i = 0 :+ 1
        
-- |Pure and simple inverse fast Fourier Transform.        
-- This is the same as 'fft' with one change: replace
-- \( \alpha_N \) with \( \alpha_N^* \), its complex conjugate.       
-- As is well-known, 
--
-- > ifft (fft x) = x*N,
--       
-- so we must divide by \( N \) to recover the original input vector        
-- from its discrete Fourier transform.
--        
-- === __Examples:__
--        
-- Check ability to recover original input vector.      
--        
-- >>> z = fft [1,2,3,4]        
-- >>> map (realPart . (/4)) $ ifft z        
--        
-- > [1.0,2.0,3.0,4.0]
--        
ifft :: [Complex Double] -> [Complex Double]
ifft [z] = [z]
ifft f = interleave [ifft fe,ifft fo']
  where n = length f
        n' = if odd n then
               error "ifft: Input vector length must be a power of 2"
             else n `div` 2
        fe = zipWith (+) (take n' f) (rotate n' f)
        fo = zipWith (-) (take n' f) (rotate n' f)
        e = iterate (*alpha) 1.0
        fo' = zipWith (*) fo e
        alpha = exp(2*pi*i/fromIntegral n) -- only change for inverse
        interleave = concat . transpose
        i = 0 :+ 1
        
-- |Slow 1D Fourier transform.
-- Accepts input vectors of any size.
ft1d :: [Complex Double] -> [Complex Double]
ft1d f = [z | k <- [0..(n-1)], let z = sum $ zipWith (*) (iterate (* alpha^k) 1.0) f ]
  where n = length f
        alpha = exp(-2*pi*i/fromIntegral n)
        i = 0 :+ 1
        
-- |Slow 1D inverse Fourier transform.
-- Accepts input vectors of any size.
ift1d :: [Complex Double] -> [Complex Double]
ift1d f = [z | k <- [0..(n-1)], let z = sum $ zipWith (*) (iterate (* alpha^k) 1.0) f ]
  where n = length f
        alpha = exp(2*pi*i/fromIntegral n)
        i = 0 :+ 1
        
-- |'fftshift` rotates the result of `fft` so that the zero
-- frequency point is in the center of the range. To understand
-- why this is needed, it is helpful to recall the following        
-- approximation for the continuous Fourier transform, for a        
-- function that is very small outside the interval \( [a,b] \). As noted
-- in the introductory comments above, this is one of two possible        
-- interpretations of the discrete Fourier transform. We have        
--        
-- \[ 
-- X(f) \approx \int_a^b x(t) e^{-2\pi i f t} dt \approx 
-- \sum_{j=0}^{N-1} x(a + j \Delta t) e^{-2\pi i f (a + j \Delta t)} \Delta t,
-- \]       
-- where \( \Delta t = \frac{b-a}{N} \). Discretizing in the frequency        
-- domain and setting \( f = k\Delta f \) yields        
-- \[        
--  X(k\Delta f) = \sum_{j=0}^{N-1} x(a + j\Delta t) 
-- e^{-2\pi i (a + j\Delta t)k \Delta f} = e^{-2\pi i a k \Delta f} 
-- \sum_{j=0}^{N-1} x_j e^{-2\pi i j k \Delta f\Delta t}\Delta t,       
-- \]        
-- where \( x_j = x(a + j\Delta t) \). Now set \( \Delta f\Delta t = 1/N \),
-- and the standard discrete Fourier transform appears:        
-- \[        
-- X(k\Delta f) = e^{-2\pi i a k\Delta f} \sum_{j=0}^{N-1} x_j e^{-2\pi i j k/N} \Delta t
-- = e^{-2\pi i a k \Delta f} \Delta t X_d(x)(k),
-- \]        
-- where \( X_d(x)(k) = \sum_{j=0}^{N-1} x_j e^{-2\pi i j k/N} \).        
--        
-- Let's gather together the parameters involved in this approximation:        
-- \[        
--  \Delta f\Delta t = \frac{1}{N},\qquad \Delta t = \frac{b-a}{N},\qquad \Delta f = \frac{f_s}{N},
-- \]        
-- where \( f_s = 1/\Delta t \) is the sampling rate. Corresponding to the \( N \) samples in the
-- time domain, for convenience we normally consider exactly \( N \) samples in the frequency domain, but
-- what samples do we choose? It is easy to check that the discrete Fourier transform \( X_d(x)(k) \)        
-- is periodic with period \( N \) in \( k \), and the same is true of our approximation        
-- \( X(k\Delta f) \) provided we choose \( a = -p\Delta t \) for 
-- some integer       
-- \( p \) (which we can do without loss of generality), so the exponential factor in the equation for \( X(k\Delta f) \) is periodic with     
-- period \( N \).        
--        
-- In the standard discrete Fourier transform we define the time and frequency grid as follows:
-- \( t = j\Delta t \),       
-- for \( j = 0,1,2,...,N-1 \), and 
-- \( f = k\Delta f \), for \( k = 0,1,2,...,N-1 \). In terms of our approximation above, this implicitly
-- assumes that \( a = 0 \), so the exponential term in the expression for \( X(k\Delta f) \) does not        
-- appear. It also assumes that the zero frequency point is on the left edge where \( k = 0 \). It
-- follows from periodicity that        
-- \( X(-k\Delta f) = X((N-k)\Delta f) \), for        
-- \( k = N/2+1,N/2+2,...,N-1 \), so negative frequencies wrap around in a circular        
-- fashion and appear to the right of the mid point. In many applications it is more natural to
-- work with \( \tilde{X}(k) = X((k+N/2) \mod N) \), the circular rotation of $X$ to the left        
-- by \( N/2 \). This brings the zero frequency to the center of the range, and places the negative        
-- frequencies where we expect them to be. This is what 'fftshift' does.        
--        
-- After applying this transformation the frequency grid becomes \( f = -f_s/2 + k\Delta f \),        
-- for \( k=0,1,...,N-1 \), and we have \( -f_s/2 \leq f \lt f_s/2 \), which happens to be
-- the same as the restriction imposed by Shannon's sampling theorem.        
--         
-- === __Examples:__
--
-- It is well-known that the Fourier transform of a Gaussian function
-- \( \exp(-a t^2) \) is another Gaussian. Let's check this by first        
-- generating a time/frequency grid.        
--        
-- >>> n = 1024 -- sample points.
-- >>> dt = 10.24/n -- time increment in Fourier integral approximation.
-- >>> df = 1/dt/n  -- frequency increment (df x dt = 1/n).
-- >>> fs = 1/dt    -- sample rate
-- >>> t = take n $ iterate (+ dt) 0
-- >>> f = take n $ iterate (+ df) 0        
-- >>> signal t = exp(-64.0*t^2) --- Gaussian
-- >>> gauss = map ((:+ 0.0) . signal) t -- apply signal and complexify        
-- >>> ft = fft gauss        
-- >>> mags = map magnitude ft        
-- >>> [rgraph| plot(f_hs, mags_hs,type='l',main='Uncentered Fourier Transform') |]        
--        
-- ![GaussUncentered](https://humangarden.net/images/GaussUncentered.png)
--        
-- Notice that the resulting Gaussian is not properly centered, because      
-- zero frequency is on the left, and negative frequencies wrap around        
-- and appear on the right. Let's use 'fftshift' to fix this.        
--        
-- >>> ftshift = fftshift ft        
-- >>> magshift = map magnitude ftshift        
-- >>> fshift = take n $ iterate (+ df) (-fs/2)        
-- >>> [rgraph| plot(fshift_hs,magshift_hs,type='l',main='Centered Fourier Transform') |]        
--        
-- ![GaussCentered](https://humangarden.net/images/GaussCentered.png)        
--        
fftshift :: [Complex Double] -> [Complex Double]
fftshift v = do
  let n = length v
      p = ceiling(fromIntegral n/2)
      shiftedIndices = [p..(n-1)] ++ [0..(p-1)]
  perm shiftedIndices v        

-- |'fftscale' shows the result of `fft` on a logarithmic (dB) scale
--  
-- > Usage: fftscale x nfirst nsamples fs fc  
--  
-- * `x` - complex-valued input signal  
-- * `nfirst` - first index value (zero-based)  
-- * `nsamples` - numer of samples (must be a power of 2) 
-- * `fs` - original sample rate  
-- * `fc` - original central frequency  
--  
-- The subsample starts at nfirst and consists of 
-- 'nsamples' points. 'nsamples' must be a power of 2.
-- The corresponding
-- frequencies and normalized power are returned (measured in
-- dB down from the max).
-- 
-- === __Examples:__
-- Here we use HaskellR tools to import a data file created by R that
-- contains I/Q data corresponding to an FM radio broadcast station
-- (the inexpensive RTL-SDR tuner and ADC is used).
-- The file contains the modulus of I + jQ for 1.2M samples (file
-- size about 10 MB since each double occupies 8 bytes). This
-- is the result of 12M values downsampled by a factor of 10. 
--
-- We show here how to use 'fftscale' to create a spectral  
-- chart that shows power concentrated at the pilot tone (19k),  
-- mono L+R audio (low freqs up to 15k), stereo L-R channel (38k), and 
-- the station ID RDS channel (57k). These spectral lines are
-- seen clearly in the waterfall diagram that appears in the documentation
-- for 'analytic'. (The station is decoded as Mono even thought the
-- content is Stereo, probably because the signal was weak.)  
--
-- Note that the sample size  
-- (a power of 2) is less than the size of the input vector, and  
-- this introduces some noise. See Wikipedia entry on FM broadcasting
-- for more information. (Note that "RDS" in the R context means
-- R Data Serialization, unrelated to RDS used in FM broadcasting!)
--  
-- @  
--  getData :: R s [Double]
--  getData = do  
--    R.dynSEXP \<$\> [r| setwd("path to HaskellR")  
--                        readRDS("FMIQ.rds") |]
--  
-- fmiq <- R.runRegion getData -- 1.2M samples (modulus I + jQ)  
--  
-- toDoubleComplex :: [Double] -> [Complex Double]  
-- toDoubleComplex = map (:+ 0.0)  
--  
-- fs = 1200000  
-- fftdata = fftscale (toDoubleComplex fmiq) 0 (2^15) (fs/10) 0  
-- freq = fst fftdata  
-- mag  = snd fftdata  
-- [rgraph| plot(freq_hs, mag_hs, type='l',xlab="Freq",  
--          ylab="Rel Amp [dB down from max]",main="FM Spectrum")  
--          abline(v=19000,col='red')  -- Pilot frequency
--          abline(v=38000,col='pink') -- L-R channel
--          abline(v=57000,col='green') |]  -- RDS station signal
-- @ 
--  
-- ![FMIQ](https://humangarden.net/images/FMIQ.png)
--  
fftscale :: [Complex Double] -> Int -> Int -> Double -> Double -> ([Double],[Double])
fftscale x nfirst nsamples fs fc = (freqs,fftscaled)
    where
        xSegment = sublist [nfirst..(nfirst+nsamples-1)] x
        ftseg = fft xSegment
        p = fftshift ftseg
        mags = map magnitude p
        mx = maximum mags
        fftscaled = map (((* 20) . log10) . (/ mx)) mags
        lowFreq = fc - fs/2
        highFreq = fc + fs/2
        n = length fftscaled
        df = fs/fromIntegral n
        freqs = take n $ iterate (+ df) lowFreq

-- |'analytic' uses 'fft' and 'ifft' to compute the analytic signal.
-- Thus the input vector must have size a power of 2. Use the        
-- slower primed variant to support input vectors of any size.        
-- Imaginary part is the Hilbert transform of the input signal.
-- Normally the input should have zero imaginary part.        
--        
-- Today digital signals (as well as analog signals like FM radio)
-- are transmitted in the form a one-dimensional
-- functions of the form
-- \[        
-- x(t) = I(t) \cos(2\pi f_c t) + Q(t) \sin(2\pi f_c t),
-- \]
-- where \( f_c \) is a carrier frequency (modern technologies like
-- WiFi/OFDM employ more than one carrier), and        
-- the functions \( I(t) \) and \( Q(t) \) encode information about        
-- amplitude and phase. In this way two-dimensional information is
-- encoded in a one-dimensional signal (the extra dimension comes from phase
-- differences or timing). Digital information is sent by subdividing the
-- complex plane of \( I + j Q \) into regions corresponding to
-- different symbols of the alphabet to be used (such regions are        
-- shown in a constellation diagram, like the one shown below).
--
-- The image below shows part of the GUI for
-- a GNU Radio FM receiver and decoder (available at
-- [GR-RDS](https://github.com/bastibl/gr-rds.git)) where the complex
-- \( I + j Q \) plane is divided into two halves corresponding to two        
-- values of a binary signal that carries information like station name,        
-- content type, etc. The digital information has central frequency
-- 57 kHz, and its power profile is clearly seen in the spectral        
-- waterfall. A detailed discussion of the decoding process can be
-- found in the online book [PySDR](https://pysdr.org/content/rds.html). See
-- the examples for 'fftscale' for more information.        
--        
-- ![RDSConstellation](https://humangarden.net/images/RDSConstellation.png)        
--        
-- The Hilbert transform is related to this representation for        
-- the signal thanks to Bedrosian's Theorem (see Wikipedia on
-- Hilbert transform). It implies that in many cases we can write        
-- the Hilbert transform in terms of the same \( I(t) \) and
-- \( Q(t) \) as follows:        
-- \[        
-- \mathbb{H}(x)(t) = I(t) \cos(2\pi f_c t - \pi/2) + Q(t) \sin(2\pi f_c t - \pi/2).        
-- \]       
-- In other words, the Hilbert transform introduces a phase-shift in    
-- the carrier basis functions by -90 degrees (this is synthesized in        
-- quadrature demodulation).        
--        
-- The Hilbert transform for continuous-time functions is defined
-- below, and most of its properties carry over to the discrete-time        
-- case. It is used to define the analytic signal for a real-valued
-- signal \( x(t) \) as follows:        
-- \( \mathbb{A}(t) = x(t) + j\mathbb{H}(x)(t), \) where
-- \( \mathbb{H}(x)(t) \) is the Hilbert transform of \( x(t). \)   
-- The most important property of the analytic signal is that its        
-- Fourier transform is zero for negative frequencies (see below).    
-- Let's begin by defining the analytic signal using this property.        
--
-- Constructing the analytic signal for a given signal \( x(t) \) is
-- straightforward: compute its Fourier transform \( X(f) \), and
-- replace it with \( Z(f), \) where \( Z(f) = 2 X(f), \) for \( f > 0 \),
-- \( Z(f) = 0, \) for \( f < 0 \), and \( Z(0) = X(0) \). Then
-- apply the inverse Fourier transform to obtain the analytic        
-- signal \( z(t) = x(t) + j\mathbb{H}{x}(t) \), where \( \mathbb{H}{x}(t) \)        
-- is the Hilbert transform of \( x(t) \). What we have done here is   
-- shift power from negative frequencies to positive frequencies in        
-- such a way that the total power is preserved. It is well-known that        
-- this definition of the Hilbert transform agrees with the one to be
-- introduced below.        
--   
-- Some care is required when this is implemented in the 
-- discrete sampling domain, and this is the focus of
-- /Computing the Discrete-Time \"Analytic\" Signal via FFT/ by 
-- S. Lawrence Marple, Jr, IEEE Trans. on Signal Processing,
-- 47(9), 1999. 
-- The goal of this paper is to define the discrete analytic signal in        
-- such a way that properties of the continuous case are preserved, in
-- particular, the real part of the analytic signal should be the 
-- original signal, and the real and imaginary parts should be
-- orthogonal. It is shown that this will be the case if we modify
-- the discrete Fourier transform as follows. \( Z[m] = 0 \) for
-- \( N/2+1 \leq m \leq N-1 \) (as discussed in 'fftshift', these are
-- negative frequencies), \( Z[m] = X[m] \) for        
-- \( m = 0 \) and \( m = N/2 \), and \(Z[m] = 2 X[m] \) for        
-- \( 1 \leq m \leq N/2-1 \) (these are positive frequencies, so we
-- double the transform).        
--         
-- The Hilbert transform was originally studied in the continuous-time        
-- domain, where it is defined as        
-- \( \mathbb{H}(x)(t) = \frac{1}{\pi} \int_{-\infty}^{\infty} \frac{x(s)}{t-s} ds, \)
-- a convolution with a singular kernel.        
-- The Hilbert transform is closely related to the Cauchy integral
-- formula, potential theory, and many other areas of mathematical        
-- analysis. See 
-- /The Cauchy Transform, Potential Theory, and Conformal Mapping/ by Steven R. Bell (2015), or /Wavelets and operators/ by Yves Meyer (1992), or
-- /Functional Analysis/ by Walter Rudin (1991).
--       
-- The
-- analytic signal is defined in terms of the Hilbert transform:        
-- \( \mathbb{A}(x)(t) = x(t) + j\mathbb{H}(x)(t). \) To understand why
-- this is useful let's recall some properties of the Hilbert transform.        
-- It can be shown (see Wikipedia) that 
-- \( \mathbb{H}(\cos(\omega t)) = \cos(\omega t - \frac{\pi}{2}) = \sin(\omega t) \), and        
-- \( \mathbb{H}(\sin(\omega t)) = \sin(\omega t -\frac{\pi}{2}) = -\cos(\omega t) \), when \( \omega > 0. \)
-- \( x(t) \) can be expanded in a Fourier series of complex        
-- exponentials with both positive and negative frequencies. Indeed, just
-- consider one (real) component 
-- \( \cos(\omega t) = (e^{j\omega t} + e^{-j\omega t})/2. \) 
-- It contributes both positive and negative frequencies. But its contribution to the analytic
-- signal is:        
-- \[        
-- \cos(\omega t) + j\sin(\omega t) =       
-- \frac{e^{j\omega t} + e^{-j\omega t}}{2} + j\frac{e^{j\omega t} - e^{-j\omega t}}{2 j} = e^{j\omega t},
-- \]
-- so negative frequency components do not appear. In
-- other words, forming the analytic signal effectively filters out all        
-- negative frequencies. Another way to see this is to recall the relationship
-- between the Hilbert transform and the Fourier transform (see Wikipedia)
-- \[        
-- \mathbb{F}(\mathbb{H}(x))(\omega) = -j \text{sgn}(\omega)\mathbb{F}(x)(\omega)
-- \]        
-- It follows readily from this that the Fourier transform of the analytic
-- signal        
-- \( \mathbb{A}(t) = x(t) + j\mathbb{H}(x)(t) \) is zero for negative frequencies.        
-- This relationship also shows (under some technical conditions) that
-- the Hilbert transform \( \mathbb{H}(x)(t) \) is orthogonal to the
-- original signal \( x(t). \) To see this, use the Plancherel Theorem,
-- and observe that it leads to the integral of an odd function in the
-- frequency domain, which is zero by symmetry.
--        
-- === __Examples:__
-- The analytic signal is obtained by shifting power from negative
-- frequencies to positive frequencies in the Fourier transform, then
-- applying the inverse Fourier transform. The imaginary part of the        
-- result is the Hilbert transform of the input signal. It is phase        
-- shifted by 90 degrees.        
--        
-- The real an imaginary parts can be plotted using your favorite
-- graphics software. Below we use the R interface provided by HaskellR
-- in a Jupyter notebook.        
--  
-- >>> n=1024
-- >>> dt = 2*pi/(n-1)
-- >>> x = take n $ iterate (+ dt) 0
-- >>> y = [z | k <- [0..(n-1)], let z = sin (x!!k)]
-- >>> z = analytic y
-- >>> zr = map realPart z
-- >>> zi = map imagPart z        
-- >>> [rgraph| plot(x_hs,zr_hs,xlab='t',ylab='signal', type='l',col='blue',
-- >>>          main='Orig. signal blue, Hilbert transform red') 
-- >>>          lines(x_hs,zi_hs,type='l',col='red') |]
--        
-- ![Analytic](https://humangarden.net/images/analytic.png)
--        
-- The real and imaginary parts of the analytic signal are orthogonal        
-- to each other. Let's check this...        
--        
-- >>> n=1024
-- >>> dt = 2*pi/(n-1)
-- >>> x = take n $ iterate (+ dt) 0
-- >>> y = [z | k <- [0..(n-1)], let z = sin (x!!k)]
-- >>> z = analytic y
-- >>> zr = map realPart z
-- >>> zi = map imagPart z      
-- >>> sum $ zipWith (*) zr zi
--        
-- > 1.5e-13        
--        
analytic :: [Complex Double] -> [Complex Double]
analytic x = map (/fromIntegral n) (ifft $ zipWith (*) h (fft x))
  -- n must be even here.
  where n = length x
        n' = n `div` 2
        h0 = [1.0]
        h1 = replicate (n'-1) 2.0
        h2 = [1.0]
        h3 = replicate (n-n') 0.0
        h = h0 ++ h1 ++ h2 ++ h3        
        
-- |Same as 'analytic' but uses the slow transforms 'ft1d' and 'ift1d'.
-- There are no restrictions on the input vector size.        
analytic' :: [Complex Double] -> [Complex Double]
analytic' x = map (/fromIntegral n) (ift1d $ zipWith (*) h (ft1d x))
  -- Adopts the convention employed in R's hht package when n is odd.
  -- This case is not considered in Marple (1999).            
  where n = length x
        n' = if even n then n `div` 2 else (n+1) `div` 2
        h0 = [1.0]
        h1 = replicate (n'-1) 2.0
        h2 = if even n then [1.0] else []
        h3 = replicate (n-n') 0.0
        h = h0 ++ h1 ++ h2 ++ h3        
        
-- |Rotate a list to the left, wrap to end.        
rotate :: Int -> [a] -> [a]
rotate = drop <> take

-- |Permute a list using a specified list of indices.
perm :: [Int] -> [Complex Double] -> [Complex Double]
perm [] _ = []
perm (x:xs) v = (v !! x):perm xs v

-- |Return sublist given list of consecutive indices
sublist :: [Int] -> [a] -> [a]
sublist [] _ = []
sublist (x:xs) v = (v !! x):sublist xs v

log10 :: (Floating a) => a -> a
log10 = logBase 10

