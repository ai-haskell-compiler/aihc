{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))



main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "GModule"
        version = "2.0"
        pkgName = "gi-gmodule"
        pkgVersion = "2.0.6"
        overridesFile = Nothing
        verbose = False
        outputDir = Nothing
        inheritedOverrides = []
