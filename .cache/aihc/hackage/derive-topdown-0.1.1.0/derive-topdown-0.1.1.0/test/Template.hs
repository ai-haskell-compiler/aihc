{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

module Template where
import           Arity
import           Data.Derive.TopDown.IsInstance
import qualified "haskell-src"  Language.Haskell.Syntax as H
import           Language.Haskell.TH.Syntax
                                         hiding ( Exp )
import           Test.HUnit
import           Types
import           Utils

arityCompany = TestCase
  (assertEqual
    "Company is instance of Arity"
    True
    $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''Company]]))
  )

arityI = TestCase
  (assertEqual "I is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''I]]))
  )

arityB = TestCase
  (assertEqual "B is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''B]]))
  )

arityT = TestCase
  (assertEqual "T is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''T]]))
  )

arityT1 = TestCase
  (assertEqual "T1 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''T1]]))
  )

arityT2 = TestCase
  (assertEqual "T2 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''T2]]))
  )

arityT3 = TestCase
  (assertEqual "T3 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''T3]]))
  )

arityR0 = TestCase
  (assertEqual "T0 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''R0]]))
  )

arityR6 = TestCase
  (assertEqual "R6 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''R6]]))
  )

arityRx0 = TestCase
  (assertEqual "Rx0 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''Rx0]]))
  )

arityRx3 = TestCase
  (assertEqual "Rx6 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''Rx3]]))
  )

arityT4 = TestCase
  (assertEqual "T4 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''T4]]))
  )

arityColonEqColon = TestCase
  (assertEqual ":=: is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''(:=:)]]))
  )

arityT5 = TestCase
  (assertEqual "T5 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''T5]]))
  )

arityExp = TestCase
  (assertEqual "Exp is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''Exp]]))
  )

arityP1 = TestCase
  (assertEqual "P1 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''P1]]))
  )

arityP2 = TestCase
  (assertEqual "P2 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''P2]]))
  )

arityList = TestCase
  (assertEqual "List is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''List]]))
  )

arityStream = TestCase
  (assertEqual
    "Stream is instance of Arity"
    True
    $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''Stream]]))
  )

arityC = TestCase
  (assertEqual "C is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''C]]))
  )

arityD = TestCase
  (assertEqual "D is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''D]]))
  )

arityIO_ = TestCase
  (assertEqual "IO_ is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''IO_]]))
  )

arityF32 = TestCase
  (assertEqual "F32 is instance of Arity"
               True
               $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''F32]]))
  )

arityHsModule = TestCase
  (assertEqual
    "HsModule is instance of Arity"
    True
    $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''H.HsModule]]))
  )

arityHsDecl = TestCase
  (assertEqual
    "HsDecl is instance of Arity"
    True
    $((qBoolToExp $ isInstance' ''TypeArity [apps [ConT ''H.HsDecl]]))
  )
