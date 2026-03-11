{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
module Instance where

import           Data.Binary
import           Data.Derive.TopDown.IsInstance
import           Language.Haskell.TH.Syntax
import           Test.HUnit
import           Types
import           Utils
import qualified "haskell-src"  Language.Haskell.Syntax as H

binaryCompany = TestCase
  (assertEqual "Company is instance of Binary"
               True
               $((qBoolToExp $ isInstance' ''Binary [(ConT ''Company)]))
  )

binaryI = TestCase
  (assertEqual
    "I is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''I, VarT (mkName "a")]]))
  )

binaryB = TestCase
  (assertEqual
    "N is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary [apps [(ConT ''B), VarT (mkName "a"), VarT (mkName "b")]]))
  )

binaryT = TestCase
  (assertEqual
    "T is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary [apps [(ConT ''T), VarT (mkName "a"), VarT (mkName "b"), VarT (mkName "c")]]))
  )

binaryT1 = TestCase
  (assertEqual "T1 is instance of Binary"
               True
               $((qBoolToExp $ isInstance' ''Binary  [ConT ''T1]))
  )

binaryT2 = TestCase
  (assertEqual
    "T2 is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''T2, VarT (mkName "a"), VarT (mkName "b")]]))
  )

binaryT3 = TestCase
  (assertEqual
    "T3 is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''T3, VarT (mkName "k"), VarT (mkName "a"), VarT (mkName "b")]]))
  )

binaryR0 = TestCase
  (assertEqual
    "R0 is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''R0, VarT (mkName "a")]]))
  )

binaryR6 = TestCase
  (assertEqual
    "R6 is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''R6, VarT (mkName "a")]]))
  )

binaryRx0 = TestCase
  (assertEqual
    "Rx0 is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''Rx0, VarT (mkName "a"), VarT (mkName "b"), VarT (mkName "c")]]))
  )

binaryRx3 = TestCase
  (assertEqual
    "Rx3 is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''Rx3, VarT (mkName "a"), VarT (mkName "b")]]))
  )

binaryE = TestCase
  (assertEqual
    "E is instance of Binary"
    True
    $((qBoolToExp $ isInstance' ''Binary  [apps [ConT ''E, VarT (mkName "a"), VarT (mkName "b")]]))
  )

binaryHsModule = TestCase
  (assertEqual "HsModule is instance of Binary"
               True
               $((qBoolToExp $ isInstance' ''Binary  [ConT ''H.HsModule]))
  )

