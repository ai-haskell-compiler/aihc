

import qualified InvalidateCacheTestHashECM as TestHashECM (tests)
import qualified InvalidateCacheTestOrdECM as TestOrdECM (tests)

invalidateCacheTests = do
  TestHashECM.tests
  TestOrdECM.tests
  return ()

main = invalidateCacheTests