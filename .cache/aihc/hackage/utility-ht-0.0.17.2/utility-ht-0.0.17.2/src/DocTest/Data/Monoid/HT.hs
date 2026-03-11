-- Do not edit! Automatically created with doctest-extract from src/Data/Monoid/HT.hs
{-# LINE 9 "src/Data/Monoid/HT.hs" #-}

module DocTest.Data.Monoid.HT where

import Data.Monoid.HT
import qualified Test.DocTest.Driver as DocTest

{-# LINE 10 "src/Data/Monoid/HT.hs" #-}
import     qualified Test.QuickCheck as QC
import     Control.Monad (mfilter)
import     Data.Function.HT (powerAssociative)
import     Data.Monoid (mconcat, mappend, mempty)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Monoid.HT:34: "
{-# LINE 34 "src/Data/Monoid/HT.hs" #-}
 DocTest.property(
{-# LINE 34 "src/Data/Monoid/HT.hs" #-}
      \b m -> when b m == mfilter (const b) (m::Maybe Ordering)
  )
 DocTest.printPrefix "Data.Monoid.HT:35: "
{-# LINE 35 "src/Data/Monoid/HT.hs" #-}
 DocTest.property(
{-# LINE 35 "src/Data/Monoid/HT.hs" #-}
      \b m -> when b m == mfilter (const b) (m::String)
  )
 DocTest.printPrefix "Data.Monoid.HT:41: "
{-# LINE 41 "src/Data/Monoid/HT.hs" #-}
 DocTest.property(
{-# LINE 41 "src/Data/Monoid/HT.hs" #-}
      QC.forAll (QC.choose (0,20)) $ \k xs -> power (fromIntegral k) xs == mconcat (replicate k (xs::String))
  )
 DocTest.printPrefix "Data.Monoid.HT:46: "
{-# LINE 46 "src/Data/Monoid/HT.hs" #-}
 DocTest.property(
{-# LINE 46 "src/Data/Monoid/HT.hs" #-}
      QC.forAll (QC.choose (0,20)) $ \k xs -> power k xs == powerAssociative mappend mempty (xs::String) k
  )
