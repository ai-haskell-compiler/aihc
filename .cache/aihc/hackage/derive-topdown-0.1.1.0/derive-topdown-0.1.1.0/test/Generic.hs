{-# LANGUAGE TemplateHaskell, StandaloneDeriving, DeriveGeneric #-}
module Generic where
import           Data.Derive.TopDown.IsInstance
import           GHC.Generics
import           Language.Haskell.TH.Syntax
import           Test.HUnit
import           Types
import           Utils

genericCompany = TestCase
  (assertEqual "Company is instance of Generic"
               True
               $((qBoolToExp $ isInstance' ''Generic [(ConT ''Company)]))
  )

genericE0 = TestCase
  (assertEqual "E0 is instance of Generic"
               True
               $((qBoolToExp $ isInstance' ''Generic [(ConT ''E0)]))
  )

genericI = TestCase
  (assertEqual
    "I is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''I, VarT (mkName "b")]]))
  )

genericB = TestCase
  (assertEqual
    "B is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''B, VarT (mkName "a"), VarT (mkName "b")]]))
  )

genericT = TestCase
  (assertEqual
    "T is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''T, VarT (mkName "a"), VarT (mkName "b"), VarT (mkName "c")]]))
  )

genericT1 = TestCase
  (assertEqual "T1 is instance of Generic"
               True
               $((qBoolToExp $ isInstance' ''Generic [(ConT ''T1)]))
  )

genericT2 = TestCase
  (assertEqual
    "T2 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''T2, VarT (mkName "a"), VarT (mkName "b")]]))
  )

genericT3 = TestCase
  (assertEqual
    "T3 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''T3, VarT (mkName "k"), VarT (mkName "a"), VarT (mkName "b")]]))
  )

genericR0 = TestCase
  (assertEqual
    "R0 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''R0, VarT (mkName "a")]]))
  )

genericR1 = TestCase
  (assertEqual
    "R1 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''R1, VarT (mkName "a")]]))
  )

genericR2 = TestCase
  (assertEqual
    "R2 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''R2, VarT (mkName "a")]]))
  )

genericR6 = TestCase
  (assertEqual
    "R6 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''R6, VarT (mkName "a")]]))
  )

generic1R0 = TestCase
  (assertEqual "R0 is instance of Generic1"
               True
               $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''R0]]))
  )

generic1R1 = TestCase
  (assertEqual "R1 is instance of Generic1"
               True
               $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''R1]]))
  )

generic1R6 = TestCase
  (assertEqual "R6 is instance of Generic1"
               True
               $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''R6]]))
  )

generic1Rx0 = TestCase
  (assertEqual
    "Rx0 is instance of Generic1"
    True
    $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''Rx0, VarT (mkName "a"), VarT (mkName "b")]]))
  )

genericRx1 = TestCase
  (assertEqual
    "Rx1 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''Rx1, VarT (mkName "a"), VarT (mkName "b"), VarT (mkName "c")]]))
  )

genericRx2 = TestCase
  (assertEqual
    "Rx2 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''Rx2, VarT (mkName "a"), VarT (mkName "b")]]))
  )

genericColonEqColon = TestCase
  (assertEqual
    "(:=:) is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''(:=:), VarT (mkName "a"), VarT (mkName "b")]]))
  )

generic1ColonEqColon = TestCase
  (assertEqual
    "(:=:) is instance of Generic1"
    True
    $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''(:=:), VarT (mkName "a") ]]))
  )

genericP1 = TestCase
  (assertEqual
    "P1 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''P1, VarT (mkName "a")]]))
  )

genericT6 = TestCase
  (assertEqual
    "T6 is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''P1, VarT (mkName "a")]]))
  )

genericList = TestCase
  (assertEqual
    "List is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''List, VarT (mkName "a")]]))
  )

generic1List = TestCase
  (assertEqual "List is instance of Generic1"
               True
               $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''List]]))
  )

genericStream = TestCase
  (assertEqual
    "Stream is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''Stream, VarT (mkName "k"), VarT (mkName "a"), VarT (mkName "b"), VarT (mkName "c")]]))
  )

genericP = TestCase
  (assertEqual
    "P is instance of Generic"
    True
    $((qBoolToExp $ isInstance' ''Generic [apps [ConT ''P, VarT (mkName "a"), VarT (mkName "b")]]))
  )

generic1P = TestCase
  (assertEqual
    "P is instance of Generic1"
    True
    $((qBoolToExp $ isInstance' ''Generic1 [apps [ConT ''P, VarT (mkName "a")]]))
  )

functorP = TestCase
  (assertEqual
    "P is instance of Functor"
    True
    $((qBoolToExp $ isInstance' ''Functor [apps [ConT ''P, VarT (mkName "a")]]))
  )

functorE = TestCase
  (assertEqual
    "E is instance of Functor"
    True
    $((qBoolToExp $ isInstance' ''Functor [apps [ConT ''E, VarT (mkName "a")]]))
  )

functorPx = TestCase
  (assertEqual "Px is instance of Functor"
               True
               $((qBoolToExp $ isInstance' ''Functor [apps [ConT ''Px]]))
  )

