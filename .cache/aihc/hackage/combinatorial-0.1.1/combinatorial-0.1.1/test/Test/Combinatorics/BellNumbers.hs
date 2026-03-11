-- Do not edit! Automatically created with doctest-extract from src/Combinatorics/BellNumbers.hs
{-# LINE 8 "src/Combinatorics/BellNumbers.hs" #-}

module Test.Combinatorics.BellNumbers where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 9 "src/Combinatorics/BellNumbers.hs" #-}
import     qualified Combinatorics.BellNumbers as Bell
import     Test.Utility (equalFuncList)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics.BellNumbers:23: "
{-# LINE 23 "src/Combinatorics/BellNumbers.hs" #-}
 DocTest.property
{-# LINE 23 "src/Combinatorics/BellNumbers.hs" #-}
     (equalFuncList (\k -> round (Bell.bellSeries (fromInteger k) :: Double)) (Bell.bellRec :: [Integer]) 20)
