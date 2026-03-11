-- Do not edit! Automatically created with doctest-extract from src/Algebra/IntegralDomain.hs
{-# LINE 54 "src/Algebra/IntegralDomain.hs" #-}

module Test.Algebra.IntegralDomain where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 55 "src/Algebra/IntegralDomain.hs" #-}
import     Algebra.IntegralDomain (roundDown, roundUp, divUp)
import     qualified Test.QuickCheck as QC
import     NumericPrelude.Base as P
import     NumericPrelude.Numeric as NP
import     Prelude ()

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Algebra.IntegralDomain:108: "
{-# LINE 108 "src/Algebra/IntegralDomain.hs" #-}
 DocTest.property
{-# LINE 108 "src/Algebra/IntegralDomain.hs" #-}
         (\n (QC.NonZero m) -> let (q,r) = divMod n m in n == (q*m+r :: Integer))
 DocTest.printPrefix "Algebra.IntegralDomain:198: "
{-# LINE 198 "src/Algebra/IntegralDomain.hs" #-}
 DocTest.property
{-# LINE 198 "src/Algebra/IntegralDomain.hs" #-}
     (\n (QC.NonZero m) -> div n m * m == (roundDown n m :: Integer))
 DocTest.printPrefix "Algebra.IntegralDomain:208: "
{-# LINE 208 "src/Algebra/IntegralDomain.hs" #-}
 DocTest.property
{-# LINE 208 "src/Algebra/IntegralDomain.hs" #-}
     (\n (QC.NonZero m) -> divUp n m * m == (roundUp n m :: Integer))
 DocTest.printPrefix "Algebra.IntegralDomain:209: "
{-# LINE 209 "src/Algebra/IntegralDomain.hs" #-}
 DocTest.property
{-# LINE 209 "src/Algebra/IntegralDomain.hs" #-}
     (\n (QC.Positive m) -> let x = roundDown n m in  n-m < x && x <= (n :: Integer))
 DocTest.printPrefix "Algebra.IntegralDomain:210: "
{-# LINE 210 "src/Algebra/IntegralDomain.hs" #-}
 DocTest.property
{-# LINE 210 "src/Algebra/IntegralDomain.hs" #-}
     (\n (QC.NonZero m) -> - roundDown n m == (roundUp (-n) m :: Integer))
