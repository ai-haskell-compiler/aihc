--------------------------------------------------------------------------------
import Test.Tasty

--------------------------------------------------------------------------------
import qualified Codec.Compression.Heatshrink.Tests

--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain tests


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Tests"
        [ Codec.Compression.Heatshrink.Tests.tests ]
