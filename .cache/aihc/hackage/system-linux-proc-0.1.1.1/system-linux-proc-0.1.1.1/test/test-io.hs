
import           Hedgehog.Main (defaultMain)
import qualified Test.System.Linux.Proc


main :: IO ()
main =
  defaultMain
    [ Test.System.Linux.Proc.tests
    ]

