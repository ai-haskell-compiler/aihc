{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))



main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Graphene"
        version = "1.0"
        pkgName = "gi-graphene"
        pkgVersion = "1.0.8"
        overridesFile = Just "Graphene.overrides"
        verbose = False
        outputDir = Nothing
        inheritedOverrides = []
