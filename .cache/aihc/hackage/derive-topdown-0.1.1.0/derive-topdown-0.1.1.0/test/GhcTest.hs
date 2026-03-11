{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
module GhcTest where

import GhcAst
import Data.Derive.TopDown.IsInstance
import GHC.Generics
import qualified "ghc" Language.Haskell.Syntax as G
import Utils
import Language.Haskell.TH
import Test.HUnit
import Types

#if __GLASGOW_HASKELL__ >= 906
genericGhcHsModule = TestCase
  (assertEqual "GhcHsModule is instance of Generic"
               True
               $((qBoolToExp $ isInstance' ''Generic [apps [(ConT ''G.HsModule), (VarT (mkName "a"))]]))
  )
#else
genericGhcHsModule = TestCase (assertEqual " " True True)
#endif
