-- Do not edit! Automatically created with doctest-extract from gaussian/MathObj/Gaussian/Variance.hs
{-# LINE 34 "gaussian/MathObj/Gaussian/Variance.hs" #-}

module Test.MathObj.Gaussian.Variance where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 35 "gaussian/MathObj/Gaussian/Variance.hs" #-}
import     qualified MathObj.Gaussian.Variance as G
import     MathObj.Gaussian.ExponentTuple (HoelderConjugates(HoelderConjugates))
import     MathObj.Gaussian.ExponentTuple (YoungConjugates(YoungConjugates))
import     qualified Algebra.Laws as Laws
import     qualified Number.Root as Root
import     NumericPrelude.Base as P
import     NumericPrelude.Numeric as NP
import     Prelude ()
import     qualified Test.QuickCheck as QC
import     Data.Function.HT (Id, nest)

asRational     :: Id (G.T Rational)
asRational     = id

withRational     :: Id (G.T Rational -> a)
withRational     = id

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Gaussian.Variance:95: "
{-# LINE 95 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 95 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y -> G.scalarProductRoot x y <= G.norm2Root x `Root.mul` G.norm2Root y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:99: "
{-# LINE 99 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 99 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y -> G.scalarProductRoot x y <= G.norm1Root x `Root.mul` G.normInfRoot y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:100: "
{-# LINE 100 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 100 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y (HoelderConjugates p q) -> G.scalarProductRoot x y <= G.normPRoot p x `Root.mul` G.normPRoot q y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:108: "
{-# LINE 108 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 108 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x -> G.norm1Root x == G.normPRoot 1 x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:114: "
{-# LINE 114 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 114 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x -> G.norm2Root x == G.normPRoot 2 x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:186: "
{-# LINE 186 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 186 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.varianceRational (G.dilate a x) == a^2 * G.varianceRational x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:187: "
{-# LINE 187 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 187 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y -> G.varianceRational (G.convolve x y) == G.varianceRational x + G.varianceRational y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:193: "
{-# LINE 193 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 193 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (Laws.identity G.multiply G.constant . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Variance:194: "
{-# LINE 194 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 194 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (Laws.commutative G.multiply . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Variance:195: "
{-# LINE 195 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 195 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (Laws.associative G.multiply . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Variance:228: "
{-# LINE 228 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 228 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (Laws.commutative G.convolve . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Variance:229: "
{-# LINE 229 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 229 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (Laws.associative G.convolve . asRational)
 DocTest.printPrefix "MathObj.Gaussian.Variance:233: "
{-# LINE 233 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 233 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y -> G.normInfRoot (G.convolve x y) <= G.norm1Root x `Root.mul` G.normInfRoot y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:234: "
{-# LINE 234 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 234 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y (HoelderConjugates p q) -> G.normInfRoot (G.convolve x y) <= G.normPRoot p x `Root.mul` G.normPRoot q y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:235: "
{-# LINE 235 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 235 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y (YoungConjugates p q r) -> G.normPRoot r (G.convolve x y) <= G.normPRoot p x `Root.mul` G.normPRoot q y)
 DocTest.printPrefix "MathObj.Gaussian.Variance:251: "
{-# LINE 251 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 251 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y -> G.fourier (G.convolve x y) == G.multiply (G.fourier x) (G.fourier y))
 DocTest.printPrefix "MathObj.Gaussian.Variance:252: "
{-# LINE 252 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 252 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x -> nest 4 G.fourier x == x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:253: "
{-# LINE 253 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 253 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.fourier (G.dilate a x) == G.amplify a (G.shrink a (G.fourier x)))
 DocTest.printPrefix "MathObj.Gaussian.Variance:254: "
{-# LINE 254 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 254 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x y -> G.scalarProductRoot x y == G.scalarProductRoot (G.fourier x) (G.fourier y))
 DocTest.printPrefix "MathObj.Gaussian.Variance:265: "
{-# LINE 265 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 265 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x (QC.Positive a) (QC.Positive b) -> G.dilate a (G.dilate b x) == G.dilate (a*b) x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:266: "
{-# LINE 266 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 266 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.shrink a x == G.dilate (recip a) x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:273: "
{-# LINE 273 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 273 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.dilate a (G.shrink a x) == x)
 DocTest.printPrefix "MathObj.Gaussian.Variance:274: "
{-# LINE 274 "gaussian/MathObj/Gaussian/Variance.hs" #-}
 DocTest.property
{-# LINE 274 "gaussian/MathObj/Gaussian/Variance.hs" #-}
     (withRational $ \x (QC.Positive a) -> G.shrink a (G.dilate a x) == x)
