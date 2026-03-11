{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))



main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "freetype2"
        version = "2.0"
        pkgName = "gi-freetype2"
        pkgVersion = "2.0.5"
        overridesFile = Nothing
        verbose = False
        outputDir = Nothing
        inheritedOverrides = []
