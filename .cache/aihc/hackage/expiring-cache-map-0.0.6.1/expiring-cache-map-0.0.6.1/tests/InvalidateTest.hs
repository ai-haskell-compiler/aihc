

import qualified InvalidateTestHashECM as TestHashECM (tests)
import qualified InvalidateTestOrdECM as TestOrdECM (tests)

invalidateTest = do
  TestHashECM.tests
  TestOrdECM.tests
  return ()

main = invalidateTest