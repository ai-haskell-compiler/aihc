-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.FIR.PolyInterp
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Polynomial interpolators.  Taken from:
--
-- Olli Niemitalo (ollinie\@freenet.hut.fi), "Polynomial Interpolators for
-- High-Quality Resampling of Oversampled Audio" Search for "deip.pdf" with
-- Google and you will find it.
--
-----------------------------------------------------------------------------

-- TODO: limit the export list

-- TODO: figure out better way to create the coeficeints where you don't
-- have to explicitly state the number of interpolation points.

module DSP.Filter.FIR.PolyInterp where

import Data.Array

import Polynomial.Basic

-- | 'mkcoef' takes the continuous impluse response function (one of the
-- functions below, @f@) and number of points in the interpolation, @p@, time
-- shifts it by @x@, samples it, and creates an array with the interpolation
-- coeficients that can be used as a FIR filter.

mkcoef :: (Num a, Ix b, Integral b) => (a -> a) -- ^ f
       -> b -- ^ p
       -> a -- ^ x
       -> Array b a -- ^ h[n]

mkcoef f p x = listArray (0,p-1) $ map f [ x - fromIntegral i | i <- [p1..p2] ]
    where p1 = -(p `div` 2 - 1)
	  p2 = p `div` 2

---------------------------------------------------------------------------------

-- The impulse responses, centered around zero

-- The following functions are named like

-- blah_ApBo or optimal_ApBoCx

-- A = number of points in the interpolation
-- B = the polynomial order
-- C = the oversampling rate that the function is designed for

---------------------------------------------------------------------------------

-- B-Splines

bspline_1p0o :: (Ord a, Fractional a) => a -> a
bspline_1p0o x | 0 <= x && x < 1 = polyeval [ 1 ] x
               | otherwise       = 0

bspline_2p1o :: (Ord a, Fractional a) => a -> a
bspline_2p1o x | 0 <= x && x < 1 = polyeval [ 1, -1 ] x
               | 1 <= x          = 0
               | otherwise       = bspline_2p1o (-x)

bspline_4p3o :: (Ord a, Fractional a) => a -> a
bspline_4p3o x | 0 <= x && x < 1 = polyeval [ 2/3,  0, -1,  1/2 ] x
               | 1 <= x && x < 2 = polyeval [ 4/3, -2,  1, -1/6 ] x
               | 2 <= x          = 0
               | otherwise       = bspline_4p3o (-x)

bspline_6p5o :: (Ord a, Fractional a) => a -> a
bspline_6p5o x | 0 <= x && x < 1 = polyeval [ 11/20,     0, -1/2,    0,  1/4,  -1/12 ] x
               | 1 <= x && x < 2 = polyeval [ 17/40,   5/8, -7/4,  5/4, -3/8,   1/24 ] x
               | 2 <= x && x < 3 = polyeval [ 81/40, -27/8,  9/4, -3/4,  1/8, -1/120 ] x
               | 3 <= x          = 0
               | otherwise       = bspline_6p5o (-x)

---------------------------------------------------------------------------------

-- Lagrange polynomials

lagrange_4p3o :: (Ord a, Fractional a) => a -> a
lagrange_4p3o x | 0 <= x && x < 1 = polyeval [ 1,  -1/2, -1,  1/2 ] x
                | 1 <= x && x < 2 = polyeval [ 1, -11/6,  1, -1/6 ] x
                | 2 <= x          = 0
		| otherwise       = lagrange_4p3o (-x)

lagrange_6p5o :: (Ord a, Fractional a) => a -> a
lagrange_6p5o x | 0 <= x && x < 1 = polyeval [ 1,    -1/3, -5/4,   5/12,  1/4,  -1/12 ] x
                | 1 <= x && x < 2 = polyeval [ 1,  -13/12, -5/8,  25/24, -3/8,   1/24 ] x
                | 2 <= x && x < 3 = polyeval [ 1, -137/60, 15/8, -17/24,  1/8, -1/120 ] x
                | 3 <= x          = 0
		| otherwise       = lagrange_6p5o (-x)

---------------------------------------------------------------------------------

-- Hermite (1st-order-osculating) polynomials

hermite_4p3o :: (Ord a, Fractional a) => a -> a
hermite_4p3o x | 0 <= x && x < 1 = polyeval [ 1,  0, -5/2,  3/2 ] x
               | 1 <= x && x < 2 = polyeval [ 2, -4,  5/2, -1/2 ] x
               | 2 <= x          = 0
	       | otherwise       = hermite_4p3o (-x)

hermite_6p3o :: (Ord a, Fractional a) => a -> a
hermite_6p3o x | 0 <= x && x < 1 = polyeval [ 1,        0, -7/3,   4/3 ] x
               | 1 <= x && x < 2 = polyeval [ 5/2, -59/12,    3, -7/12 ] x
               | 2 <= x && x < 3 = polyeval [ -3/2,   7/4, -2/3,  1/12 ] x
               | 3 <= x          = 0
               | otherwise       = hermite_6p3o (-x)

hermite_6p5o :: (Ord a, Fractional a) => a -> a
hermite_6p5o x | 0 <= x && x < 1 = polyeval [ 1,     0, -25/12,   5/12, 13/12, -5/12 ] x
               | 1 <= x && x < 2 = polyeval [ 1,  5/12,  -35/8,   35/8, -13/8,  5/24 ] x
               | 2 <= x && x < 3 = polyeval [ 3, -29/4, 155/24, -65/24, 13/24, -1/24 ] x
               | 3 <= x          = 0
               | otherwise       = hermite_6p5o (-x)

---------------------------------------------------------------------------------

-- 2nd-order-osculating polynomials

sndosc_4p5o :: (Ord a, Fractional a) => a -> a
sndosc_4p5o x | 0 <= x && x < 1 = polyeval [  1, 0,   -1, -9/2,  15/2, -3 ] x
              | 1 <= x && x < 2 = polyeval [ -4, 18, -29, 43/2, -15/2,  1 ] x
              | 2 <= x          = 0
	      | otherwise       = sndosc_4p5o (-x)

sndosc_6p5o :: (Ord a, Fractional a) => a -> a
sndosc_6p5o x | 0 <= x && x < 1 = polyeval [  1,      0,   -5/4,  -35/12,  21/4, -25/12 ] x
              | 1 <= x && x < 2 = polyeval [ -4,   75/4, -245/8,  545/24, -63/8,  25/24 ] x
              | 2 <= x && x < 3 = polyeval [ 18, -153/4,  255/8, -313/24,  21/8,  -5/24 ] x
              | 3 <= x          = 0
              | otherwise       = sndosc_6p5o (-x)

---------------------------------------------------------------------------------

-- Misc

watte_4p2o :: (Ord a, Fractional a) => a -> a
watte_4p2o x | 0 <= x && x < 1 = polyeval [ 1, -1/2, -1/2 ] x
             | 1 <= x && x < 2 = polyeval [ 1, -3/2,  1/2 ] x
             | 2 <= x          = 0
	     | otherwise       = watte_4p2o (-x)

parabolic2x_4p2o :: (Ord a, Fractional a) => a -> a
parabolic2x_4p2o x | 0 <= x && x < 1 = polyeval [ 1/2, 0, -1/4 ] x
                   | 1 <= x && x < 2 = polyeval [ 1,  -1,  1/4 ] x
                   | 2 <= x          = 0
		   | otherwise       = parabolic2x_4p2o (-x)

---------------------------------------------------------------------------------

-- Optimal designs

optimal_2p3o2x :: (Ord a, Fractional a) => a -> a
optimal_2p3o2x x | 0 <= x && x < 1 = polyeval [ 0.80607906469176971, 0.17594740788514596,
						-2.35977550974341630, 1.57015627178718420 ] x
                 | 1 <= x          = 0
		 | otherwise       = optimal_2p3o2x (-x)

optimal_2p3o4x :: (Ord a, Fractional a) => a -> a
optimal_2p3o4x x | 0 <= x && x < 1 = polyeval [ 0.88207975731800936, -0.10012219395448523,
						-1.99054787320203810, 1.32598918957298410 ] x
                 | 1 <= x          = 0
		 | otherwise       = optimal_2p3o4x (-x)

optimal_2p3o8x :: (Ord a, Fractional a) => a -> a
optimal_2p3o8x x | 0 <= x && x < 1 = polyeval [ 0.94001491168487883, -0.51213628865925998,
					        -1.10319974084152170, 0.73514591836770027 ] x
                 | 1 <= x          = 0
		 | otherwise       = optimal_2p3o8x (-x)

optimal_2p3o16x :: (Ord a, Fractional a) => a -> a
optimal_2p3o16x x | 0 <= x && x < 1 = polyeval [ 0.96964782067188493, -0.74617479745643256,
						 -0.57923093055631791, 0.38606621963374965 ] x
                  | 1 <= x          = 0
		  | otherwise       = optimal_2p3o16x (-x)

optimal_2p3o32x :: (Ord a, Fractional a) => a -> a
optimal_2p3o32x x | 0 <= x && x < 1 = polyeval [ 0.98472017575676363, -0.87053863725307623,
					         -0.29667081825572522, 0.19775766248673177 ] x
                  | 1 <= x          = 0
	          | otherwise       = optimal_2p3o32x (-x)

optimal_4p2o2x :: (Ord a, Fractional a) => a -> a
optimal_4p2o2x x | 0 <= x && x < 1 = polyeval [ 0.50061662213752656, -0.04782068534965925,
					        -0.21343978756177684 ] x
                 | 1 <= x && x < 2 = polyeval [ 0.92770135528027386, -0.88689658749623701,
					        0.21303593243799016  ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p2o2x (-x)

optimal_4p2o4x :: (Ord a, Fractional a) => a -> a
optimal_4p2o4x x | 0 <= x && x < 1 = polyeval [ 0.33820365736567115, 0.2114449807519728,
					        -0.22865399531858188  ] x
                 | 1 <= x && x < 2 = polyeval [ 1.12014639874555470, -1.01414466618792900,
					        0.22858390767180370  ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p2o4x (-x)

optimal_4p2o8x :: (Ord a, Fractional a) => a -> a
optimal_4p2o8x x | 0 <= x && x < 1 = polyeval [ 0.09224718574204172, 0.59257579283164508,
					        -0.24005206207889518  ] x
                 | 1 <= x && x < 2 = polyeval [ 1.38828036063664320, -1.17126532964206100,
					        0.24004281672637814  ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p2o8x (-x)

optimal_4p2o16x :: (Ord a, Fractional a) => a -> a
optimal_4p2o16x x | 0 <= x && x < 1 = polyeval [ -0.41849525763976203, 1.36361593203840510,
					         -0.24506117865474364  ] x
                  | 1 <= x && x < 2 = polyeval [ 1.90873339502208310, -1.44144384373471430,
					         0.24506002360805534  ] x
                  | 2 <= x          = 0
	          | otherwise       = optimal_4p2o16x (-x)

optimal_4p2o32x :: (Ord a, Fractional a) => a -> a
optimal_4p2o32x x | 0 <= x && x < 1 = polyeval [ -1.42170796824052890, 2.87083485132510450,
					         -0.24755243839713828 ] x
                  | 1 <= x && x < 2 = polyeval [ 2.91684291662070860, -1.95043794419108290,
					        0.24755229501840223 ] x
                  | 2 <= x          = 0
	          | otherwise       = optimal_4p2o32x (-x)

optimal_4p3o2x :: (Ord a, Fractional a) => a -> a
optimal_4p3o2x x | 0 <= x && x < 1 = polyeval [ 0.59244492420272321, 0.03573669883299365,
					        -0.78664888597764893, 0.36030925263849456 ] x
                 | 1 <= x && x < 2 = polyeval [ 1.20220428331406090, -1.60101160971478710,
					        0.70401463131621556, -0.10174985775982505 ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p3o2x (-x)

optimal_4p3o4x :: (Ord a, Fractional a) => a -> a
optimal_4p3o4x x | 0 <= x && x < 1 = polyeval [ 0.60304009430474115, 0.05694012453786401,
					        -0.89223007211175309, 0.42912649274763925 ] x
                 | 1 <= x && x < 2 = polyeval [ 1.31228823423882930, -1.85072890189700660,
					        0.87687351895686727, -0.13963062613760227 ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p3o4x (-x)

optimal_4p3o8x :: (Ord a, Fractional a) => a -> a
optimal_4p3o8x x | 0 <= x && x < 1 = polyeval [ 0.60658368706046584, 0.07280793921972525,
					        -0.95149675410360302, 0.46789242171187317 ] x
                 | 1 <= x && x < 2 = polyeval [ 1.35919815911169020, -1.95618744839533010,
					        0.94949311590826524, -0.15551896027602030 ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p3o8x (-x)

optimal_4p3o16x :: (Ord a, Fractional a) => a -> a
optimal_4p3o16x x | 0 <= x && x < 1 = polyeval [ 0.60844825096346644, 0.07980169577604959,
					         -0.97894238166068270, 0.48601256046234864 ] x
                  | 1 <= x && x < 2 = polyeval [ 1.37724137476464990, -1.99807048591354810,
					         0.97870442828560433, -0.16195131297091253 ] x
                  | 2 <= x          = 0
	          | otherwise       = optimal_4p3o16x (-x)

optimal_4p3o32x :: (Ord a, Fractional a) => a -> a
optimal_4p3o32x x | 0 <= x && x < 1 = polyeval [ 0.60908264223655417, 0.08298544053689563,
					         -0.99052586766084594, 0.49369595780454456 ] x
                  | 1 <= x && x < 2 = polyeval [ 1.38455689452848450, -2.01496368680360890,
					         0.99049753216621961, -0.16455902278580614 ] x
                  | 2 <= x          = 0
	          | otherwise       = optimal_4p3o32x (-x)

optimal_4p4o2x :: (Ord a, Fractional a) => a -> a
optimal_4p4o2x x | 0 <= x && x < 1 = polyeval [ 0.58448510036125145, 0.04442540676862300,
					        -0.7586487041827807, 0.29412762852131868,
					        0.04252164479749607 ] x
                 | 1 <= x && x < 2 = polyeval [ 1.06598379704160570, -1.16581445347275190,
					        0.21256821036268256, 0.13781898240764315,
					        -0.04289144034653719 ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p4o2x (-x)

optimal_4p4o4x :: (Ord a, Fractional a) => a -> a
optimal_4p4o4x x | 0 <= x && x < 1 = polyeval [ 0.61340295990566229, 0.06128937679587994,
					        -0.94057832565094635, 0.44922093286355397,
					        0.00986988334359864 ] x
                 | 1 <= x && x < 2 = polyeval [ 1.30835018075821670, -1.82814511658458520,
					        0.81943257721092366, -0.09642760567543440,
					        -0.00989340017126506 ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p4o4x (-x)

optimal_4p4o8x :: (Ord a, Fractional a) => a -> a
optimal_4p4o8x x | 0 <= x && x < 1 = polyeval [ 0.62095991632974834, 0.06389302461261143,
					       -0.98489647972932193, 0.48698871865064902,
					        0.00255074537015887 ] x
                 | 1 <= x && x < 2 = polyeval [ 1.35943398999940390, -1.97277963497287720,
					        0.95410568622888214, -0.14868053358928229,
					       -0.00255226912537286 ] x
                 | 2 <= x          = 0
	         | otherwise       = optimal_4p4o8x (-x)

optimal_4p4o16x :: (Ord a, Fractional a) => a -> a
optimal_4p4o16x x | 0 <= x && x < 1 = polyeval [ 0.62293049365660191, 0.06443376638262904,
					        -0.99620011474430481, 0.49672182806667398,
					         0.00064264050033187 ] x
                  | 1 <= x && x < 2 = polyeval [ 1.37216269878963180, -2.00931632449031920,
					         0.98847675044522398, -0.16214364417487748,
					        -0.00064273459469381 ] x
                  | 2 <= x          = 0
	          | otherwise       = optimal_4p4o16x (-x)

optimal_4p4o32x :: (Ord a, Fractional a) => a -> a
optimal_4p4o32x x | 0 <= x && x < 1 = polyeval [ 0.62342449465938121, 0.06456923251842608,
					        -0.99904509583176049, 0.49917660509564427,
					         0.00016095224137360 ] x
                  | 1 <= x && x < 2 = polyeval [ 1.37534629142898650, -2.01847637982642340,
					         0.99711292321092770, -0.16553360612350931,
					        -0.00016095810460478 ] x
                  | 2 <= x          = 0
	          | otherwise       = optimal_4p4o32x (-x)

optimal_6p4o2x :: (Ord a, Fractional a) => a -> a
optimal_6p4o2x x | 0 <= x && x < 1 = polyeval [ 0.42640922432669054, -0.0052558029434142,
					       -0.20486985491012843, 0.00255494211547300,
					        0.03134095684084392 ] x
                 | 1 <= x && x < 2 = polyeval [ 0.30902529029941583, 0.37868437559565432,
					       -0.70564644117967990, 0.31182026815653541,
					       -0.04385804833432710 ] x
                 | 2 <= x && x < 3 = polyeval [ 1.51897639740576910, -1.83761742915820410,
					        0.83217835730406542, -0.16695522597587154,
					        0.01249475765486819 ] x
                 | 3 <= x          = 0
	         | otherwise       = optimal_6p4o2x (-x)

optimal_6p4o4x :: (Ord a, Fractional a) => a -> a
optimal_6p4o4x x | 0 <= x && x < 1 = polyeval [ 0.20167941634921072, -0.06119274485321008,
					        0.56468711069379207, -0.42059475673758634,
					        0.02881527997393852 ] x
                 | 1 <= x && x < 2 = polyeval [ -0.64579641436229407, 2.33580825807694700,
					        -1.85350543411307390, 0.51926458031522660,
					        -0.04250898918476453 ] x
                 | 2 <= x && x < 3 = polyeval [ 2.76228852293285200, -3.09936092833253300,
					        1.27147464005834010, -0.22283280665600644,
					        0.01369173779618459 ] x
                 | 3 <= x          = 0
	         | otherwise       = optimal_6p4o4x (-x)

optimal_6p4o8x :: (Ord a, Fractional a) => a -> a
optimal_6p4o8x x | 0 <= x && x < 1 = polyeval [ -0.17436452172055789, -0.15190225510786248,
					         1.87551558979819120, -1.15976496200057480,
					         0.03401038103941584 ] x
                 | 1 <= x && x < 2 = polyeval [ -2.26955357035241170, 5.73320660746477540,
					        -3.92391712129699590, 0.93463067895166918,
					        -0.05090907029392906 ] x
                 | 2 <= x && x < 3 = polyeval [ 4.84834508915762540, -5.25661448354449060,
					        2.04584149450148180, -0.32814290420019698,
					        0.01689861603514873 ] x
                 | 3 <= x          = 0
	         | otherwise       = optimal_6p4o8x (-x)

optimal_6p4o16x :: (Ord a, Fractional a) => a -> a
optimal_6p4o16x x | 0 <= x && x < 1 = polyeval [ -0.94730014688427577, -0.33649680079382827,
					          4.53807483241466340, -2.64598691215356660,
					          0.03755086455339280 ] x
                  | 1 <= x && x < 2 = polyeval [ -5.55035312316726960, 12.52871168241192600,
					         -7.98288364772738750, 1.70665858343069510,
					         -0.05631219122315393 ] x
                  | 2 <= x && x < 3 = polyeval [ 8.94785524286246310, -9.37021675593126700,
					         3.44447036756440590, -0.49470749109917245,
					         0.01876132424143207 ] x
                  | 3 <= x          = 0
	          | otherwise       = optimal_6p4o16x (-x)

optimal_6p4o32x :: (Ord a, Fractional a) => a -> a
optimal_6p4o32x x | 0 <= x && x < 1 = polyeval [ -2.44391738331193720, -0.69468212315980082,
					          9.67889243081689440, -5.50592307590218160,
					          0.03957507923965987 ] x
                  | 1 <= x && x < 2 = polyeval [ -11.87524595267807600, 25.58633277328986500,
					         -15.73068663442630400, 3.15288929279855570,
					         -0.05936083498715066 ] x
                  | 2 <= x && x < 3 = polyeval [ 16.79403235763479100, -17.17264148794549100,
					         6.05175140696421730, -0.79053754554850286,
					         0.01978575568000696 ] x
                  | 3 <= x          = 0
	          | otherwise       = optimal_6p4o32x (-x)

optimal_6p5o2x :: (Ord a, Fractional a) => a -> a
optimal_6p5o2x x | 0 <= x && x < 1 = polyeval [ 0.48217702203158502, -0.00127577239632662,
					       -0.3267507171395277, -0.02014846731685776,
					        0.14640674192652170, -0.04317950185225609 ] x
                 | 1 <= x && x < 2 = polyeval [ 0.35095903476754237, 0.53534756396439365,
					       -1.22477236472789920, 0.74995484587342742,
					       -0.19234043023690772, 0.01802814255926417 ] x
                 | 2 <= x && x < 3 = polyeval [ 1.62814578813495040, -2.26168360510917840,
					        1.22220278720010690, -0.31577407091450355,
					        0.03768876199398620, -0.00152170021558204 ] x
                 | 3 <= x          = 0
	         | otherwise       = optimal_6p5o2x (-x)

optimal_6p5o4x :: (Ord a, Fractional a) => a -> a
optimal_6p5o4x x | 0 <= x && x < 1 = polyeval [ 0.50164509338655083, -0.00256790184606694,
					       -0.36229943140977111, -0.04512026308730401,
					        0.20620318519804220, -0.06607747864416924 ] x
                 | 1 <= x && x < 2 = polyeval [ 0.30718330223223800, 0.78336433172501685,
					       -1.66940481896969310, 1.08365113099941970,
					       -0.30560854964737405, 0.03255079211953620 ] x
                 | 2 <= x && x < 3 = polyeval [ 2.05191571792256240, -3.19403437421534920,
					        1.99766476840488070, -0.62765808573554227,
					        0.09909173357642603, -0.00628989632244913 ] x
		 | 3 <= x          = 0
	         | otherwise       = optimal_6p5o4x (-x)

optimal_6p5o8x :: (Ord a, Fractional a) => a -> a
optimal_6p5o8x x | 0 <= x && x < 1 = polyeval [ 0.50513183702821474, -0.00368143670114908,
					       -0.36434084624989699, -0.06070462616102962,
					        0.22942797169644802, -0.07517133281176167 ] x
                 | 1 <= x && x < 2 = polyeval [ 0.28281884957695946, 0.88385964850687193,
					       -1.82581238657617080, 1.19588167464050650,
					       -0.34363487882262922, 0.03751837438141215 ] x
                 | 2 <= x && x < 3 = polyeval [ 2.15756386503245070, -3.42137079071284810,
					        2.18592382088982260, -0.70370361187427199,
					        0.11419603882898799, -0.00747588873055296 ] x
                 | 3 <= x          = 0
	         | otherwise       = optimal_6p5o8x (-x)

optimal_6p5o16x :: (Ord a, Fractional a) => a -> a
optimal_6p5o16x x | 0 <= x && x < 1 = polyeval [ 0.50819303579369868, -0.00387117789818541,
					        -0.36990908725555449, -0.06616250180411522,
					         0.24139298776307896, -0.07990500783668089 ] x
                  | 1 <= x && x < 2 = polyeval [ 0.27758734130911511, 0.91870010875159547,
					        -1.89281840112089440, 1.24834464824612510,
					        -0.36203450650610985, 0.03994519162531633   ] x
                  | 2 <= x && x < 3 = polyeval [ 2.19284545406407450, -3.50786533926449100,
					         2.26228244623301580, -0.73559668875725392,
					         0.12064126711558003, -0.00798609327859495   ] x
                  | 3 <= x          = 0
	          | otherwise       = optimal_6p5o16x (-x)

optimal_6p5o32x :: (Ord a, Fractional a) => a -> a
optimal_6p5o32x x | 0 <= x && x < 1 = polyeval [ 0.52558916128536759, 0.00010896283126635,
					        -0.42682321682847008, -0.04095676092513167,
					         0.25041444762720882, -0.08349799235675044 ] x
                  | 1 <= x && x < 2 = polyeval [ 0.33937904183610190, 0.80946953063234006,
					        -1.86228986389877100, 1.27215033630638800,
					        -0.37562266426589430, 0.04174912841630993 ] x
                  | 2 <= x && x < 3 = polyeval [ 2.13606003964474490, -3.48774662195185850,
					         2.28912105276248390, -0.75510203509083995,
					         0.12520821766375972, -0.00834987866042734  ] x
                  | 3 <= x          = 0
	          | otherwise       = optimal_6p5o32x (-x)

---------------------------------------------------------------------------------

{-------------------

Test routines

y = [ sin $ 0.345 + 0.1234 * fromIntegral i | i <- [0..10] ]

h1 = mkcoef bspline_4p3o     4 0.2
h2 = mkcoef hermite_4p3o     4 0.2
h3 = mkcoef lagrange_4p3o    4 0.2
h4 = mkcoef hermite_4p3o     4 0.2
h5 = mkcoef sndosc_4p5o      4 0.2
h6 = mkcoef watte_4p2o       4 0.2
h7 = mkcoef parabolic2x_4p2o 4 0.2

h8  = mkcoef bspline_6p5o  6 0.2
h9  = mkcoef lagrange_6p5o 6 0.2
h10 = mkcoef hermite_6p3o  6 0.2
h11 = mkcoef hermite_6p5o  6 0.2
h12 = mkcoef sndosc_6p5o   6 0.2

h2p3o2x  = mkcoef optimal_2p3o2x  2 0.2
h2p3o4x  = mkcoef optimal_2p3o4x  2 0.2
h2p3o8x  = mkcoef optimal_2p3o8x  2 0.2
h2p3o16x = mkcoef optimal_2p3o16x 2 0.2
h2p3o32x = mkcoef optimal_2p3o32x 2 0.2

h4p2o2x  = mkcoef optimal_4p2o2x  4 0.2
h4p2o4x  = mkcoef optimal_4p2o4x  4 0.2
h4p2o8x  = mkcoef optimal_4p2o8x  4 0.2
h4p2o16x = mkcoef optimal_4p2o16x 4 0.2
h4p2o32x = mkcoef optimal_4p2o32x 4 0.2

h4p3o2x  = mkcoef optimal_4p3o2x  4 0.2
h4p3o4x  = mkcoef optimal_4p3o4x  4 0.2
h4p3o8x  = mkcoef optimal_4p3o8x  4 0.2
h4p3o16x = mkcoef optimal_4p3o16x 4 0.2
h4p3o32x = mkcoef optimal_4p3o32x 4 0.2

h4p4o2x  = mkcoef optimal_4p4o2x  4 0.2
h4p4o4x  = mkcoef optimal_4p4o4x  4 0.2
h4p4o8x  = mkcoef optimal_4p4o8x  4 0.2
h4p4o16x = mkcoef optimal_4p4o16x 4 0.2
h4p4o32x = mkcoef optimal_4p4o32x 4 0.2

h6p4o2x  = mkcoef optimal_6p4o2x  4 0.2
h6p4o4x  = mkcoef optimal_6p4o4x  4 0.2
h6p4o8x  = mkcoef optimal_6p4o8x  4 0.2
h6p4o16x = mkcoef optimal_6p4o16x 4 0.2
h6p4o32x = mkcoef optimal_6p4o32x 4 0.2

h6p5o2x  = mkcoef optimal_6p5o2x  4 0.2
h6p5o4x  = mkcoef optimal_6p5o4x  4 0.2
h6p5o8x  = mkcoef optimal_6p5o8x  4 0.2
h6p5o16x = mkcoef optimal_6p5o16x 4 0.2
h6p5o32x = mkcoef optimal_6p5o32x 4 0.2

interpolate y h = sum $ zipWith (*) y (elems h)

x1  = sin $ 0.345 + 0.1234 * 1.2
x1' = map (interpolate y) [ h1, h2, h3, h4, h5, h6, h7 ]

x2  = sin $ 0.345 + 0.1234 * 2.2
x2' = map (interpolate y) [ h8, h9, h10, h11, h12 ]

The values of all these lists should be one, or nearly one.  They
aren't for the 6p4o optimal designs, but I'm not sure why.  Olli's
paper states that these are a little screwy, though.

h_test = map (sum . elems) [ h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12 ]
h2p3o_test = map (sum . elems) [ h2p3o2x, h2p3o4x, h2p3o8x, h2p3o16x, h2p3o32x ]
h4p2o_test = map (sum . elems) [ h4p2o2x, h4p2o4x, h4p2o8x, h4p2o16x, h4p2o32x ]
h4p3o_test = map (sum . elems) [ h4p4o2x, h4p4o4x, h4p4o8x, h4p4o16x, h4p4o32x ]
h4p4o_test = map (sum . elems) [ h4p4o2x, h4p4o4x, h4p4o8x, h4p4o16x, h4p4o32x ]
h6p4o_test = map (sum . elems) [ h6p4o2x, h6p4o4x, h6p4o8x, h6p4o16x, h6p4o32x ]
h6p5o_test = map (sum . elems) [ h6p5o2x, h6p5o4x, h6p5o8x, h6p5o16x, h6p5o32x ]

-------------------}
