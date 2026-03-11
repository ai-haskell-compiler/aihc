{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))



main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "cairo"
        version = "1.0"
        pkgName = "gi-cairo"
        pkgVersion = "1.0.30"
        overridesFile = Just "cairo.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = []
