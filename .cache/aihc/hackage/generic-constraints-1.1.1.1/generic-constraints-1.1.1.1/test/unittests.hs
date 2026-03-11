{-# LANGUAGE
    DeriveGeneric
  , TemplateHaskell
  , StandaloneDeriving
  , UndecidableInstances
#-}

import Generics.Constraints
import GHC.Generics
import Test.HUnit

data T a = T (a Int)
    deriving Generic

makeDeriving ''Eq   ''T
makeDeriving ''Ord  ''T
makeDeriving ''Show ''T

main :: IO Counts
main = runTestTT $ test
  [ assert (T [5] > T [3])
  ]
