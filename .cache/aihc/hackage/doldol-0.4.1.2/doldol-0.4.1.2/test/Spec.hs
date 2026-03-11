import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import qualified Test.Framework.Providers.QuickCheck2 as Test
import Test.HUnit
import Test.QuickCheck

import Test.Flag
import Test.Flag.Env
import Test.Flag.Phantom

import Data.Flag
import Data.Word


main =
  Test.defaultMain
    [ Test.Flag.tests
    , Test.Flag.Phantom.tests
    ]
