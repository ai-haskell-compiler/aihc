module Spec.Data.UUIDSpec (main, spec) where

import           Data.Proxy                (Proxy (..))
import           Data.UUID.Types           (UUID)

import           Spec.Utils                (matchesTextShowSpec)

import           Test.Hspec                (Spec, describe, hspec, parallel)
import           Test.QuickCheck.Instances ()

import           TextShow.Data.UUID        ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "UUID" $
        matchesTextShowSpec (Proxy :: Proxy UUID)
