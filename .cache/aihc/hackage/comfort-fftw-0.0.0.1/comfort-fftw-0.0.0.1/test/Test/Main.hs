-- Do not edit! Automatically created with doctest-extract.
module Test.Main where

import qualified Test.Numeric.FFTW.Rank1
import qualified Test.Numeric.FFTW.Rank2
import qualified Test.Numeric.FFTW.Rank3
import qualified Test.Numeric.FFTW.RankN
import qualified Test.Numeric.FFTW.Batch

import qualified Test.DocTest.Driver as DocTest

main :: DocTest.T ()
main = do
    Test.Numeric.FFTW.Rank1.test
    Test.Numeric.FFTW.Rank2.test
    Test.Numeric.FFTW.Rank3.test
    Test.Numeric.FFTW.RankN.test
    Test.Numeric.FFTW.Batch.test
