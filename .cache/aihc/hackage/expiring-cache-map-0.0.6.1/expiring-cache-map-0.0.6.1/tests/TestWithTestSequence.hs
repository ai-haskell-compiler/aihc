
import qualified TestOrdECMWithTestSequence as OrdTest
import qualified TestHashECMWithTestSequence as HashTest
import qualified TestOrdECMWithTestSequenceInvalidating as OrdTestInvalidating
import qualified TestHashECMWithTestSequenceInvalidating as HashTestInvalidating

main = do
  HashTest.testWithTestSequence
  OrdTest.testWithTestSequence
  HashTestInvalidating.testWithTestSequenceInvalidating
  OrdTestInvalidating.testWithTestSequenceInvalidating
  return ()
