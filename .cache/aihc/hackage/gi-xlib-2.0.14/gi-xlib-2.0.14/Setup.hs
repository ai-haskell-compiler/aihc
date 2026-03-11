{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))



main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "xlib"
        version = "2.0"
        pkgName = "gi-xlib"
        pkgVersion = "2.0.14"
        overridesFile = Just "xlib.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = []
