Haskell implementation of rb-j's IIR cookbook.  I have turned his text
file into a literate Haskell file.  You can find the original at:

http://www.harmony-central.com/Computer/Programming/Audio-EQ-Cookbook.txt

--Matt Donadio (m.p.donadio@ieee.org)

> -----------------------------------------------------------------------------
> -- |
> -- Module      :  DSP.Filter.IIR.IIR
> -- Copyright   :  (c) Matthew Donadio 2003
> -- License     :  GPL
> --
> -- Maintainer  :  m.p.donadio@ieee.org
> -- Stability   :  experimental
> -- Portability :  portable
> --
> -- Cookbook formulae for audio EQ biquad filter coefficients
> -- by Robert Bristow-Johnson  <robert@wavemechanics.com>
> --
> -- <http://www.harmony-central.com/Computer/Programming/Audio-EQ-Cookbook.txt>
> --
> -----------------------------------------------------------------------------


> module DSP.Filter.IIR.Cookbook where

> import DSP.Filter.IIR.IIR

> import DSP.Basic((^!))

          Cookbook formulae for audio EQ biquad filter coefficients
-----------------------------------------------------------------------------
            by Robert Bristow-Johnson  <robert@wavemechanics.com>

All filter transfer functions were derived from analog prototypes (that 
are shown below for each EQ filter type) and had been digitized using the 
Bilinear Transform.  BLT frequency warping has been taken into account 
for both significant frequency relocation and for bandwidth readjustment.

First, given a biquad transfer function defined as:

            b0 + b1*z^-1 + b2*z^-2
    H(z) = ------------------------                                     (Eq 1)
            a0 + a1*z^-1 + a2*z^-2

This shows 6 coefficients instead of 5 so, depending on your
architechture, you will likely normalize a0 to be 1 and perhaps also
b0 to 1 (and collect that into an overall gain coefficient).  Then
your transfer function would look like:

            (b0/a0) + (b1/a0)*z^-1 + (b2/a0)*z^-2
    H(z) = ---------------------------------------                      (Eq 2)
               1 + (a1/a0)*z^-1 + (a2/a0)*z^-2

or

                      1 + (b1/b0)*z^-1 + (b2/b0)*z^-2
    H(z) = (b0/a0) * ---------------------------------                  (Eq 3)
                      1 + (a1/a0)*z^-1 + (a2/a0)*z^-2


The most straight forward implementation would be the Direct I form
(using Eq 2):

    y[n] = (b0/a0)*x[n] + (b1/a0)*x[n-1] + (b2/a0)*x[n-2]
                        - (a1/a0)*y[n-1] - (a2/a0)*y[n-2]               (Eq 4)

This is probably both the best and the easiest method to implement in
the 56K.

Now, given:

    sampleRate (the sampling frequency)

    frequency ("wherever it's happenin', man."  "center" frequency 
        or "corner" (-3 dB) frequency, or shelf midpoint frequency, 
        depending on which filter type)
    
    dBgain (used only for peaking and shelving filters)

    bandwidth in octaves (between -3 dB frequencies for BPF and notch
        or between midpoint (dBgain/2) gain frequencies for peaking EQ)

     _or_ Q (the EE kind of definition, except for peakingEQ in which A*Q
        is the classic EE Q.  That adjustment in definition was done so
        that a boost of N dB followed by a cut of N dB for identical Q and
        frequency results in a perfectly flat unity gain filter or "wire".)

     _or_ S, a "shelf slope" parameter (for shelving EQ only).  When S = 1, 
        the shelf slope is as steep as it can be and remain monotonically 
        increasing or decreasing gain with frequency.  The shelf slope, in 
        dB/octave, remains proportional to S for all other values.



First compute a few intermediate variables:

    A     = sqrt[ 10^(dBgain/20) ]
          = 10^(dBgain/40)                    (for peaking and shelving EQ filters only)

    omega = 2*pi*frequency/sampleRate

    sin   = sin(omega)
    cos   = cos(omega)


    alpha = sin/(2*Q)                                      (if Q is specified)
          = sin*sinh[ ln(2)/2 * bandwidth * omega/sin ]    (if bandwidth is specified)

        The relationship between bandwidth and Q is
                1/Q = 2*sinh[ln(2)/2*bandwidth*omega/sin]  (digital filter using BLT)
        or      1/Q = 2*sinh[ln(2)/2*bandwidth])           (analog filter prototype)


    beta  = sqrt(A)/Q                                      (for shelving EQ filters only)
          = sqrt(A)*sqrt[ (A + 1/A)*(1/S - 1) + 2 ]        (if shelf slope is specified)
          = sqrt[ (A^2 + 1)/S - (A-1)^2 ]

        The relationship between shelf slope and Q is
                1/Q = sqrt[(A + 1/A)*(1/S - 1) + 2]


Then compute the coefficients for whichever filter type you want:

  The analog prototypes are shown for normalized frequency.
  The bilinear transform substitutes:

                1          1 - z^-1
  s  <-  -------------- * ----------
          tan(omega/2)     1 + z^-1

  and makes use of these trig identities:

                    sin(w)                                 1 - cos(w)
   tan(w/2)    = ------------              (tan(w/2))^2 = ------------
                  1 + cos(w)                               1 + cos(w)



LPF:        H(s) = 1 / (s^2 + s/Q + 1)

            b0 =  (1 - cos)/2
            b1 =   1 - cos
            b2 =  (1 - cos)/2
            a0 =   1 + alpha
            a1 =  -2*cos
            a2 =   1 - alpha

> {-# specialize lpf :: Float -> Float -> [Float] -> [Float] #-}
> {-# specialize lpf :: Double -> Double -> [Double] -> [Double] #-}

> lpf :: Floating a => a -> a -> [a] -> [a]
> lpf bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =  (1 - cos w) / 2
>          b1 =   1 - cos w
>          b2 =  (1 - cos w) / 2
>          a0 =   1 + alpha
>          a1 =  -2 * cos w
>          a2 =   1 - alpha
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)

HPF:        H(s) = s^2 / (s^2 + s/Q + 1)

            b0 =  (1 + cos)/2
            b1 = -(1 + cos)
            b2 =  (1 + cos)/2
            a0 =   1 + alpha
            a1 =  -2*cos
            a2 =   1 - alpha

> {-# specialize hpf :: Float -> Float -> [Float] -> [Float] #-}
> {-# specialize hpf :: Double -> Double -> [Double] -> [Double] #-}

> hpf :: Floating a => a -> a -> [a] -> [a]
> hpf bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =  (1 + cos w) / 2
>          b1 = -(1 + cos w)
>          b2 =  (1 + cos w) / 2
>          a0 =   1 + alpha
>          a1 =  -2 * cos w
>          a2 =   1 - alpha
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)

BPF:        H(s) = s / (s^2 + s/Q + 1)          (constant skirt gain, peak gain = Q)

            b0 =   sin/2  =   Q*alpha
            b1 =   0 
            b2 =  -sin/2  =  -Q*alpha
            a0 =   1 + alpha
            a1 =  -2*cos
            a2 =   1 - alpha

> {-# specialize bpf_csg :: Float -> Float -> [Float] -> [Float] #-}
> {-# specialize bpf_csg :: Double -> Double -> [Double] -> [Double] #-}

> bpf_csg :: Floating a => a -> a -> [a] -> [a]
> bpf_csg bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =   sin w / 2
>          b1 =   0
>          b2 =  -sin w / 2
>          a0 =   1 + alpha
>          a1 =  -2 * cos w
>          a2 =   1 - alpha
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)

BPF:        H(s) = (s/Q) / (s^2 + s/Q + 1)      (constant 0 dB peak gain)

            b0 =   alpha
            b1 =   0
            b2 =  -alpha
            a0 =   1 + alpha
            a1 =  -2*cos
            a2 =   1 - alpha

> {-# specialize bpf_cpg :: Float -> Float -> [Float] -> [Float] #-}
> {-# specialize bpf_cpg :: Double -> Double -> [Double] -> [Double] #-}

> bpf_cpg :: Floating a => a -> a -> [a] -> [a]
> bpf_cpg bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =   alpha
>          b1 =   0
>          b2 =  -alpha
>          a0 =   1 + alpha
>          a1 =  -2 * cos w
>          a2 =   1 - alpha
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)

notch:      H(s) = (s^2 + 1) / (s^2 + s/Q + 1)

            b0 =   1
            b1 =  -2*cos
            b2 =   1
            a0 =   1 + alpha
            a1 =  -2*cos
            a2 =   1 - alpha

> {-# specialize notch :: Float -> Float -> [Float] -> [Float] #-}
> {-# specialize notch :: Double -> Double -> [Double] -> [Double] #-}

> notch :: Floating a => a -> a -> [a] -> [a]
> notch bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =   1
>          b1 =  -2 * cos w
>          b2 =   1
>          a0 =   1 + alpha
>          a1 =  -2 * cos w
>          a2 =   1 - alpha
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)

APF:        H(s) = (s^2 - s/Q + 1) / (s^2 + s/Q + 1)

            b0 =   1 - alpha
            b1 =  -2*cos
            b2 =   1 + alpha
            a0 =   1 + alpha
            a1 =  -2*cos
            a2 =   1 - alpha

> {-# specialize apf :: Float -> Float -> [Float] -> [Float] #-}
> {-# specialize apf :: Double -> Double -> [Double] -> [Double] #-}

> apf :: Floating a => a -> a -> [a] -> [a]
> apf bw w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =   1 - alpha
>          b1 =  -2 * cos w
>          b2 =   1 + alpha
>          a0 =   1 + alpha
>          a1 =  -2 * cos w
>          a2 =   1 - alpha
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)

peakingEQ:  H(s) = (s^2 + s*(A/Q) + 1) / (s^2 + s/(A*Q) + 1)

            b0 =   1 + alpha*A
            b1 =  -2*cos
            b2 =   1 - alpha*A
            a0 =   1 + alpha/A
            a1 =  -2*cos
            a2 =   1 - alpha/A

> {-# specialize peakingEQ :: Float -> Float -> Float -> [Float] -> [Float] #-}
> {-# specialize peakingEQ :: Double -> Double -> Double -> [Double] -> [Double] #-}

> peakingEQ :: Floating a => a -> a -> a -> [a] -> [a]
> peakingEQ bw dBgain w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =   1 + alpha * a
>          b1 =  -2 * cos w
>          b2 =   1 - alpha * a
>          a0 =   1 + alpha / a
>          a1 =  -2 * cos w
>          a2 =   1 - alpha / a
>          alpha = sin w * sinh (log 2 / 2 * bw * w / sin w)
>          a = 10 ** (dBgain / 40)

lowShelf:   H(s) = A * (s^2 + (sqrt(A)/Q)*s + A) / (A*s^2 + (sqrt(A)/Q)*s + 1)

            b0 =    A*[ (A+1) - (A-1)*cos + beta*sin ]
            b1 =  2*A*[ (A-1) - (A+1)*cos            ]
            b2 =    A*[ (A+1) - (A-1)*cos - beta*sin ]
            a0 =        (A+1) + (A-1)*cos + beta*sin
            a1 =   -2*[ (A-1) + (A+1)*cos            ]
            a2 =        (A+1) + (A-1)*cos - beta*sin

> {-# specialize lowShelf :: Float -> Float -> Float -> [Float] -> [Float] #-}
> {-# specialize lowShelf :: Double -> Double -> Double -> [Double] -> [Double] #-}

> lowShelf :: Floating a => a -> a -> a -> [a] -> [a]
> lowShelf s dBgain w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =    a*( (a+1) - (a-1) * cos w + beta * sin w)
>          b1 =  2*a*( (a-1) - (a+1) * cos w               )
>          b2 =    a*( (a+1) - (a-1) * cos w - beta * sin w)
>          a0 =        (a+1) + (a-1) * cos w + beta * sin w
>          a1 =   -2*( (a-1) + (a+1) * cos w               )
>          a2 =        (a+1) + (a-1) * cos w - beta * sin w
>          beta = sqrt ((a^!2 + 1) / s - (a-1)^!2)
>          a = 10 ** (dBgain / 40)

highShelf:  H(s) = A * (A*s^2 + (sqrt(A)/Q)*s + 1) / (s^2 + (sqrt(A)/Q)*s + A)

            b0 =    A*[ (A+1) + (A-1)*cos + beta*sin ]
            b1 = -2*A*[ (A-1) + (A+1)*cos            ]
            b2 =    A*[ (A+1) + (A-1)*cos - beta*sin ]
            a0 =        (A+1) - (A-1)*cos + beta*sin
            a1 =    2*[ (A-1) - (A+1)*cos            ]
            a2 =        (A+1) - (A-1)*cos - beta*sin

> {-# specialize highShelf :: Float -> Float -> Float -> [Float] -> [Float] #-}
> {-# specialize highShelf :: Double -> Double -> Double -> [Double] -> [Double] #-}

> highShelf :: Floating a => a -> a -> a -> [a] -> [a]
> highShelf s dBgain w = biquad_df1 (a1/a0) (a2/a0) (b0/a0) (b1/a0) (b2/a0)
>    where b0 =    a*( (a+1) - (a-1) * cos w + beta * sin w)
>          b1 = -2*a*( (a-1) - (a+1) * cos w               )
>          b2 =    a*( (a+1) - (a-1) * cos w - beta * sin w)
>          a0 =        (a+1) + (a-1) * cos w + beta * sin w
>          a1 =   -2*( (a-1) + (a+1) * cos w               )
>          a2 =        (a+1) + (a-1) * cos w - beta * sin w
>          beta = sqrt ((a^!2 + 1) / s - (a-1)^!2)
>          a = 10 ** (dBgain / 40)

(This text-only file is best viewed or printed with a mono-spaced font.)
