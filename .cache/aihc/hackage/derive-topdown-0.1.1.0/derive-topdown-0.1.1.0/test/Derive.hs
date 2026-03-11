{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Derive where
import           Control.Monad.IO.Class
import           Data.Derive.TopDown.IsInstance
import           Language.Haskell.TH     hiding ( Exp )
import           Test.HUnit
import           Types
import           Utils
import qualified "haskell-src"  Language.Haskell.Syntax as H

eqPerson = TestCase
  (assertEqual "Person is instance of Eq"
               True
               $((qBoolToExp $ isInstance' ''Eq [(ConT ''Person)]))
  )

ordPerson = TestCase
  (assertEqual "Person is instance of Ord"
               True
               $((qBoolToExp $ isInstance' ''Ord [(ConT ''Person)]))
  )

showCompany = TestCase
  (assertEqual "Company is instance of Show"
               True
               $((qBoolToExp $ isInstance' ''Show [(ConT ''Company)]))
  )

eqE0 = TestCase
  (assertEqual "E0 is instance of Eq"
               True
               $((qBoolToExp $ isInstance' ''Eq [(ConT ''E0)]))
  )

ordE0 = TestCase
  (assertEqual "E0 is instance of Ord"
               True
               $((qBoolToExp $ isInstance' ''Ord [(ConT ''E0)]))
  )

showE0 = TestCase
  (assertEqual "E0 is instance of Show"
               True
               $((qBoolToExp $ isInstance' ''Show [(ConT ''E0)]))
  )

eqT1 = TestCase
  (assertEqual "T1 is instance of Eq"
               True
               $((qBoolToExp $ isInstance' ''Eq [(ConT ''T1)]))
  )

ordT1 = TestCase
  (assertEqual "T1 is instance of Ord"
               True
               $((qBoolToExp $ isInstance' ''Ord [(ConT ''T1)]))
  )

showT1 = TestCase
  (assertEqual "T1 is instance of Show"
               True
               $((qBoolToExp $ isInstance' ''Show [(ConT ''T1)]))
  )

eqT2 = TestCase
  (assertEqual
    "T2 is instance of Eq"
    True
    $((qBoolToExp $ isInstance' ''Eq [apps [ConT ''T2, VarT (mkName "a"), VarT (mkName "b")]]))
  )

ordT2 = TestCase
  (assertEqual
    "T2 is instance of Ord"
    True
    $((qBoolToExp $ isInstance' ''Ord [apps [ConT ''T2, VarT (mkName "a"), VarT (mkName "b")]]))
  )

ordT3 = TestCase
  (assertEqual
    "T3 is instance of Ord"
    True
    $((qBoolToExp $ isInstance' ''Ord [apps [ConT ''T3, VarT (mkName "k"), VarT (mkName "a"), VarT (mkName "b")]]))
  )


showT2 = TestCase
  (assertEqual
    "T2 is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''T2, VarT (mkName "a"), VarT (mkName "b")]]))
  )

showT3 = TestCase
  (assertEqual
    "T3 is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''T3, VarT (mkName "k"), VarT (mkName "a"), VarT (mkName "b")]]))
  )

ordR0 = TestCase
  (assertEqual
    "R0 is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''R0,  VarT (mkName "b")]]))
  )

showRx0 = TestCase
  (assertEqual
    "Rx0 is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''Rx0, VarT (mkName "c"), VarT (mkName "a"), VarT (mkName "b")]]))
  )

showColonEqColon = TestCase
  (assertEqual
    ":=: is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''(:=:), VarT (mkName "a"), VarT (mkName "b")]]))
  )

showExp = TestCase
  (assertEqual
    "Exp is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''Exp, VarT (mkName "a")]]))
  )

showDataQL = TestCase
  (assertEqual
    "Exp is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''DataQL, VarT (mkName "a")]]))
  )

showP1 = TestCase
  (assertEqual
    "P1 is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''P1, VarT (mkName "a")]]))
  )

showT6 = TestCase
  (assertEqual
    "T6 is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''Rx0, VarT (mkName "c"), VarT (mkName "a"), VarT (mkName "b")]]))
  )

showStream = TestCase
  (assertEqual
    "Stream is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''Stream, VarT  (mkName "k"), VarT (mkName "x"), VarT (mkName "a"), VarT (mkName "b")]]))
  )

showD = TestCase
  (assertEqual
    "D is instance of Show"
    True
    $((qBoolToExp $ isInstance' ''Show [apps [ConT ''D, VarT (mkName "b")]]))
  )


ioIO_ = TestCase
  (assertEqual "IO_ is instance of MonadIO"
               True
               $((qBoolToExp $ isInstance' ''MonadIO [apps [ConT ''IO_]]))
  )

realFloatF32 = TestCase
  (assertEqual "F32 is instance of RealFloat"
               True
               $((qBoolToExp $ isInstance' ''RealFloat [apps [ConT ''F32]]))
  )

ordHsModule = TestCase
  (assertEqual "HsModule is instance of Ord"
               True
               $((qBoolToExp $ isInstance' ''Ord [apps [ConT ''H.HsModule]]))
  )

ordHsDecl = TestCase
  (assertEqual "HsModule is instance of Ord"
               True
               $((qBoolToExp $ isInstance' ''Ord [apps [ConT ''H.HsDecl]]))
  )
