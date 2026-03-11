
import Test.QuickCheck.Simple (defaultMain)

import qualified Test.Data.Radius.Iso as V
import qualified Test.Data.Radius.IsoNoVSA as N

main :: IO ()
main = do
  defaultMain V.tests
  defaultMain N.tests
