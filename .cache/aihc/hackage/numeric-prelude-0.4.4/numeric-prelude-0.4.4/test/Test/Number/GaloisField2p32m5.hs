-- Do not edit! Automatically created with doctest-extract from src/Number/GaloisField2p32m5.hs
{-# LINE 33 "src/Number/GaloisField2p32m5.hs" #-}

module Test.Number.GaloisField2p32m5 where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 34 "src/Number/GaloisField2p32m5.hs" #-}
import     qualified Number.GaloisField2p32m5 as GF
import     qualified Algebra.Laws as Laws
import     Test.QuickCheck ((==>))
import     NumericPrelude.Numeric
import     NumericPrelude.Base
import     Prelude ()

gf     :: GF.T -> GF.T
gf     = id

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Number.GaloisField2p32m5:46: "
{-# LINE 46 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 46 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.identity (+) zero . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:47: "
{-# LINE 47 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 47 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.commutative (+) . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:48: "
{-# LINE 48 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 48 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.associative (+) . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:49: "
{-# LINE 49 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 49 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.inverse (+) negate zero . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:50: "
{-# LINE 50 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 50 "src/Number/GaloisField2p32m5.hs" #-}
     (\x -> Laws.inverse (+) (x-) (gf x))
 DocTest.printPrefix "Number.GaloisField2p32m5:51: "
{-# LINE 51 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 51 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.identity (*) one . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:52: "
{-# LINE 52 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 52 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.commutative (*) . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:53: "
{-# LINE 53 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 53 "src/Number/GaloisField2p32m5.hs" #-}
     (Laws.associative (*) . gf)
 DocTest.printPrefix "Number.GaloisField2p32m5:54: "
{-# LINE 54 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 54 "src/Number/GaloisField2p32m5.hs" #-}
     (\y -> gf y /= zero ==> Laws.inverse (*) recip one y)
 DocTest.printPrefix "Number.GaloisField2p32m5:55: "
{-# LINE 55 "src/Number/GaloisField2p32m5.hs" #-}
 DocTest.property
{-# LINE 55 "src/Number/GaloisField2p32m5.hs" #-}
     (\y x -> gf y /= zero ==> Laws.inverse (*) (x/) x y)
