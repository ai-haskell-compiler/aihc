-- Do not edit! Automatically created with doctest-extract from src/Algebra/Additive.hs
{-# LINE 42 "src/Algebra/Additive.hs" #-}

module Test.Algebra.Additive where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 43 "src/Algebra/Additive.hs" #-}
import     qualified Algebra.Additive as A
import     qualified Test.QuickCheck as QC

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Algebra.Additive:108: "
{-# LINE 108 "src/Algebra/Additive.hs" #-}
 DocTest.property
{-# LINE 108 "src/Algebra/Additive.hs" #-}
     (\(QC.NonEmpty ns) -> A.sum ns == (A.sum1 ns :: Integer))
 DocTest.printPrefix "Algebra.Additive:121: "
{-# LINE 121 "src/Algebra/Additive.hs" #-}
 DocTest.property
{-# LINE 121 "src/Algebra/Additive.hs" #-}
     (\ns -> A.sum ns == (A.sumNestedAssociative ns :: Integer))
 DocTest.printPrefix "Algebra.Additive:136: "
{-# LINE 136 "src/Algebra/Additive.hs" #-}
 DocTest.property
{-# LINE 136 "src/Algebra/Additive.hs" #-}
     (\ns -> A.sum ns == (A.sumNestedCommutative ns :: Integer))
