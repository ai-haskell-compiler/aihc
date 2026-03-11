import Test.Hspec

import RPM.PackageSpec
import RPM.VersionSpec

main :: IO ()
main =
  mapM_ hspec [verspec,pkgspec]
