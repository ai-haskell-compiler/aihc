module Main where

import Test.TypeSpec
import GHC.Generics
import Generic.Type.Assert
import Data.Kind

data DSum = DSum1 () | DSum2 deriving stock Generic
data DEnum = DEnum1 | DEnum2 | DEnum3 deriving stock Generic
data DNonSum = DNonSum () () deriving stock Generic
data DVoid deriving stock Generic

main :: IO ()
main = print spec

spec
    :: Expect '[ AssertPasses (GAssertNotVoid DSum)
               , AssertPasses (GAssertSum DSum)
               , AssertPasses (GAssertNotSum DNonSum)
               , AssertPasses (GAssertEnum DEnum)
               ]
spec = Valid

type AssertPasses a = Is a (() :: Constraint)

-- can't write this because GHC finds the TypeError before type-spec
--type AssertFails a = Isn't a (() :: Constraint)
