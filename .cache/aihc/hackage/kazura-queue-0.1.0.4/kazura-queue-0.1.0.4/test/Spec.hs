
import qualified KazuraQueueConcurrentSpec as KQCSpec
import qualified KazuraQueueSpec           as KQSpec
import qualified Test.Hspec                as HS
import qualified WVarConcurrentSpec        as WVCSpec
import qualified WVarSpec                  as WVSpec

main :: IO ()
main = HS.hspec $ do
    WVSpec.spec
    WVCSpec.spec
    KQSpec.spec
    KQCSpec.spec

