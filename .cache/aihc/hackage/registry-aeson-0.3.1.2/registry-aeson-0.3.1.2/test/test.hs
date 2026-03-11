import AutoDiscoveredSpecs (tests)
import Protolude
import Test.Tasty.Hedgehogx

main = tests >>= defaultMain . groupByModuleName
