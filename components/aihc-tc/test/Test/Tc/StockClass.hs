{-# LANGUAGE OverloadedStrings #-}

-- | The stock deriving table and the class locations of a configuration
-- have to describe the same set of classes. Neither can see the other --
-- the table is part of the type checker and the locations are library
-- knowledge -- so a class added to one and forgotten in the other is only
-- caught here.
--
-- The same configuration says where the values of a generated body come
-- from, which is checked here too: a reference of the primitive package
-- resolves against the configuration, and one of the class package against
-- the class that is being derived.
module Test.Tc.StockClass (tcStockClassTests) where

import Aihc.Parser.Syntax (NameType (..))
import Aihc.Prim.Wiring (primDerivingReferences)
import Aihc.Resolve (PackageId (..), ResolutionNamespace (..))
import Aihc.Tc.Deriving.References
  ( DerivingReference (..),
    DerivingReferences (..),
    ReferencePackage (..),
    StockClassLocation (..),
    referenceIdentity,
    stockClassLocationMatches,
  )
import Aihc.Tc.Deriving.StockClass (StockClass (..), lookupStockClass, stockClasses)
import Data.List (group, sort)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tcStockClassTests :: TestTree
tcStockClassTests = testGroup "stock deriving" [tableTests, referenceTests]

tableTests :: TestTree
tableTests =
  testGroup
    "class table"
    [ testCase "every located class has a table row" $
        assertEqual
          "classes the configuration locates but the table does not know"
          []
          [name | name <- locatedClasses, isNothing (lookupStockClass name)],
      testCase "every table row is located" $
        assertEqual
          "classes the table knows but the configuration does not locate"
          []
          [stockClassName row | row <- stockClasses, stockClassName row `notElem` locatedClasses],
      -- A generated body names values of the reference table, so the class
      -- it belongs to has to be one the configuration lists as generated
      -- rather than one it merely recognizes.
      testCase "every generated class is located as generated" $
        assertEqual
          "generated classes the configuration does not locate as generated"
          []
          [ stockClassName row
          | row <- stockClasses,
            isJust (stockClassMethods row),
            stockClassName row `notElem` generatedClasses
          ],
      testCase "no class is listed twice in the table" $
        assertEqual "duplicate table rows" [] (duplicates (map stockClassName stockClasses))
    ]
  where
    generatedClasses = map stockLocationName (derivingStockClasses references)
    locatedClasses = generatedClasses <> map snd (derivingRecognizedClasses references)

referenceTests :: TestTree
referenceTests =
  testGroup
    "reference packages"
    [ testCase "a primitive reference comes from the configuration" $
        assertEqual
          "resolved identity"
          (prim, "GHC.Types", "True")
          (referenceIdentity prim classPackage (derivingTrue references)),
      -- The helpers of a class outside the primitive package sit beside the
      -- class, and a module that derives it has that package in scope.
      testCase "a class reference comes from the derived class" $
        assertEqual
          "resolved identity"
          (classPackage, "GHC.Internal.TH.Lib", "conE")
          (referenceIdentity prim classPackage classReference),
      testCase "a location with a package demands it" $ do
        assertEqual
          "same package"
          True
          (stockClassLocationMatches ("aihc-prim", "GHC.Classes") (StockClassLocation (Just prim) "GHC.Classes" "Eq") "Eq")
        assertEqual
          "another package"
          False
          (stockClassLocationMatches ("user-package", "GHC.Classes") (StockClassLocation (Just prim) "GHC.Classes" "Eq") "Eq"),
      -- A class the configuration cannot name the package of is matched by
      -- module and name alone. A user class that repeats both is then stock,
      -- and the generator reports the helpers it does not have.
      testCase "a location without a package takes any" $
        assertEqual
          "any package"
          True
          (stockClassLocationMatches ("user-package", "GHC.Internal.TH.Lift") (StockClassLocation Nothing "GHC.Internal.TH.Lift" "Lift") "Lift"),
      testCase "a location still demands the module and the name" $ do
        assertEqual
          "another module"
          False
          (stockClassLocationMatches ("aihc-prim", "User.Classes") (StockClassLocation Nothing "GHC.Classes" "Eq") "Eq")
        assertEqual
          "another class"
          False
          (stockClassLocationMatches ("aihc-prim", "GHC.Classes") (StockClassLocation Nothing "GHC.Classes" "Eq") "Ord")
    ]
  where
    classPackage = PackageId "aihc-internal-0.1-abc"
    classReference = DerivingReference ReferenceClassPackage "GHC.Internal.TH.Lib" "conE" NameVarId ResolutionNamespaceTerm

references :: DerivingReferences
references = primDerivingReferences prim

prim :: PackageId
prim = PackageId "aihc-prim"

duplicates :: [Text] -> [Text]
duplicates names = [name | name : _ : _ <- group (sort names)]
