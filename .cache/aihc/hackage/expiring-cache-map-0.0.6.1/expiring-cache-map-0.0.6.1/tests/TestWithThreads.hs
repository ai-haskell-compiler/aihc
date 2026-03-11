--
-- Test with threads
--

import qualified TestOrdECMWithThreads as OrdTest
import qualified TestHashECMWithThreads as HashTest
import qualified TestOrdECMWithThreadsInvalidating as OrdTestInvalidating
import qualified TestHashECMWithThreadsInvalidating as HashTestInvalidating

testWithThreads = do
  HashTest.testWithThreads
  OrdTest.testWithThreads
  HashTestInvalidating.testWithThreadsInvalidating
  OrdTestInvalidating.testWithThreadsInvalidating
  return ()

main = testWithThreads
