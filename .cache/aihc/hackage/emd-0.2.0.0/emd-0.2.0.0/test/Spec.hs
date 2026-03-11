
import           Test.Tasty
import           Tests.EMD
import           Tests.HHT
import           Tests.Spline

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ splineTest
    , emdTests
    , hhtTests
    ]
