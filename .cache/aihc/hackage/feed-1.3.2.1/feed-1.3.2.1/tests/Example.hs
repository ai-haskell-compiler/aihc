module Example
  ( exampleTests
  ) where

import Prelude.Compat

import Example.CreateAtom (atomFeed)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion)

exampleTests :: Test
exampleTests =
  testGroup "Examples" [testCase "example code to create an atom feed typechecks" typeCheckAtom]

typeCheckAtom :: Assertion
typeCheckAtom =
  case atomFeed of
    Just "" -> error "createAtom returned an empty Text"
    Just _ -> return ()
    Nothing -> error "createAtom returned document with unresolved entities"
