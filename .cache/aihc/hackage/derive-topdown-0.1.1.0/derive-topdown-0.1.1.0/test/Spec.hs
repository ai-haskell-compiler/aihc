{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}
module Main where

import           Control.Monad
import           Derive
import           Generic
import           Instance
import           Template
import           Test.HUnit
import           GhcTest

derive = TestList
  [ eqPerson
  , ordPerson
  , showCompany
  , eqE0
  , ordE0
  , showE0
  , eqT1
  , ordT1
  , showT1
  , eqT2
  , ordT2
  , ordT3
  , showT2
  , showT3
  , ordR0
  , showRx0
  , showColonEqColon
  , showExp
  , showDataQL
  , showP1
  , showT6
  , showStream
  , showD
  , ioIO_
  , realFloatF32
  , ordHsModule
  , ordHsDecl
  ]

generic = TestList
  [ genericCompany
  , genericE0
  , genericI
  , genericB
  , genericT
  , genericT1
  , genericT2
  , genericT3
  , genericR0
  , genericR1
  , genericR2
  , genericR6
  , generic1R0
  , generic1R1
  , generic1R6
  , generic1Rx0
  , genericRx1
  , genericRx2
  , genericColonEqColon
  , generic1ColonEqColon
  , genericP1
  , genericT6
  , genericList
  , generic1List
  , genericStream
  , genericP
  , generic1P
  , functorP
  , functorE
  , functorPx
  ]

instances = TestList
  [ binaryCompany
  , binaryI
  , binaryB
  , binaryT
  , binaryT1
  , binaryT2
  , binaryT3
  , binaryR0
  , binaryR6
  , binaryRx0
  , binaryRx3
  , binaryE
  , binaryHsModule
  ]

templates = TestList
  [ arityCompany
  , arityI
  , arityB
  , arityT
  , arityT1
  , arityT2
  , arityT3
  , arityR0
  , arityR6
  , arityRx0
  , arityRx3
  , arityT4
  , arityColonEqColon
  , arityT5
  , arityExp
  , arityP1
  , arityP2
  , arityList
  , arityStream
  , arityC
  , arityD
  , arityIO_
  , arityF32
  , arityHsModule
  , arityHsDecl
  ]

ghcAst = TestList [genericGhcHsModule]

main :: IO ()
main = do
  void $ runTestTT derive
  void $ runTestTT generic
  void $ runTestTT instances
  void $ runTestTT templates
  void $ runTestTT ghcAst
