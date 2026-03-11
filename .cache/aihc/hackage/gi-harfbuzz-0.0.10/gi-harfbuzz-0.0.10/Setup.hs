{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.GLib.Config as GLib
import qualified GI.Freetype2.Config as Freetype2


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "HarfBuzz"
        version = "0.0"
        pkgName = "gi-harfbuzz"
        pkgVersion = "0.0.10"
        overridesFile = Just "HarfBuzz.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:GLib" GLib.overrides, TaggedOverride "inherited:Freetype2" Freetype2.overrides]
