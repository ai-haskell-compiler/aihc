{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Shared where
import Control.Exception
import Data.Maybe          (listToMaybe)
import GHC.Stack           (callStack)
import GHC.Stack.Types     (getCallStack)
import Language.Haskell.TH
import Test.Inspection
import Test.Tasty.HUnit

checkInspection
  :: HasCallStack => Result -> Assertion
checkInspection Success{} = pure ()
checkInspection (Failure msg) =
  throwIO $ HUnitFailure (fmap snd $ listToMaybe $ getCallStack callStack) msg

inspecting :: String -> Obligation -> Q Exp
inspecting desc reg =
  [|testCase desc $ checkInspection $(inspectTest reg)|]

data GHCVer = GHC8_8 | GHC8_10 | GHC9_0 | GHC9_2 | GHC9_4 | GHC9_6 | GHC9_8 | GHC9_10 | GHC9_12
  deriving (Show, Eq, Ord)

ghcVer :: GHCVer
#if __GLASGOW_HASKELL__ == 912
ghcVer = GHC9_12
#elif __GLASGOW_HASKELL__ == 910
ghcVer = GHC9_10
#elif __GLASGOW_HASKELL__ == 908
ghcVer = GHC9_8
#elif __GLASGOW_HASKELL__ == 906
ghcVer = GHC9_6
#elif __GLASGOW_HASKELL__ == 904
ghcVer = GHC9_4
#elif __GLASGOW_HASKELL__ == 902
ghcVer = GHC9_2
#elif __GLASGOW_HASKELL__ == 900
ghcVer = GHC9_0
#elif __GLASGOW_HASKELL__ == 810
ghcVer = GHC8_10
#elif __GLASGOW_HASKELL__ == 808
ghcVer = GHC8_8
#else
ghcVer = error "Coudld not determine GHC Version: __GLASGOW_HASKELL__"
#endif
