module HaskellWorks.Data.MQuerySpec (spec) where

import Control.Lens
import HaskellWorks.Data.Model.Example
import HaskellWorks.Data.MQuery
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.DList                   as DL
import qualified Data.Foldable                as F
import qualified HaskellWorks.Data.Model.Lens as L

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.Data.MQuerySpec" $ do
  it "Select into simple lens to boolean field" $ requireProperty $ do
    let q = MQuery $ DL.fromList exampleMounts
    let actual = F.toList $ q >>^. L.readOnly
    actual === [True, False, False, False]
  it "Select into simple lens to list" $ requireProperty $ do
    let q = MQuery $ DL.fromList exampleMounts
    let actual = F.toList $ q >>^. L.options
    actual === [[], ["nosuid" , "noauto"], [], ["noexec"]]
  it "Select into lens to a foldable" $ requireProperty $ do
    let q = MQuery $ DL.fromList exampleMounts
    let actual = F.toList $ q >>^.. L.options
    actual === ["nosuid" , "noauto" , "noexec"]
  it "Select into lens to a foldable" $ requireProperty $ do
    let q = MQuery $ DL.fromList exampleMounts
    let actual = F.toList $ q <&> view L.options
    actual === [[] , ["nosuid" , "noauto"] , [] , ["noexec"]]

