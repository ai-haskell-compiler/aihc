import AutoDiscoveredSpecs (tests)
import Protolude
import Test.Tasty.Extensions

main :: IO ()
main = tests >>= defaultMain . groupByModuleName
