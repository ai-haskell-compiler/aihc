module Data.BigFloatingSpec
  (main, spec)
where

import           Control.Exception     (evaluate)
import           Data.BigDecimal
import           Data.BigFloating
import           GHC.Real              (Ratio ((:%)))
import           Test.Hspec            hiding (it)
import           Data.TestUtils        (it)        -- I'm redefining it to use 1000 examples
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  -- mathematical functions on BigDecimals
  describe "sqr" $ do
    it "computes the square root of any non-negative BigDecimal" $
      property $ \x scale -> let (x', r) = (abs x, sqr x' $ halfUp scale) in abs (r*r - x') < BigDecimal 1000 scale
    it "throws an exception if applied to a negative number" $
      evaluate (sqr (-16) $ halfUp 2) `shouldThrow` anyException

  -- mathematical functions on BigDecimals
  describe "nthRoot" $ do
    it "computes the nth root of any non-negative BigDecimal" $
      property $ \x n -> let (x', n', r) = (1+ abs x, 1+abs n, nthRoot x' n' (halfUp 10)) in abs (r^n' - x') < BigDecimal (fromIntegral n'*10000) 10
    it "throws an exception if trying to get even root of a negative number" $
      evaluate (nthRoot (-16) 4 $ halfUp 2) `shouldThrow` anyException
    --it "computes odd roots of any negative BigDecimal" $
    --  property $ \x n -> let (x', n', r) = ((-1)- abs x, if even n then 1 + abs n else abs n, nthRoot x' n' (halfUp 10)) in abs (r^n' - x') < BigDecimal (n'*10000) 10


  describe "pi" $
   it "computes pi with a default precision of 100 decimal digits" $
     pi `shouldBe` fromString "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

  describe "piChudnovsky" $
   it "computes pi with arbitrary precision (demonstrating it with 1000 digits)" $
     piChudnovsky (FLOOR, Just 1000) `shouldBe` fromString "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989"
