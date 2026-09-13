{-# LANGUAGE OverloadedStrings #-}

-- | The stock deriving table and the class locations of a configuration
-- have to describe the same set of classes. Neither can see the other --
-- the table is part of the type checker and the locations are library
-- knowledge -- so a class added to one and forgotten in the other is only
-- caught here.
module Test.Tc.StockClass (tcStockClassTests) where

import Aihc.Prim.Wiring (primDerivingReferences)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Deriving.References (DerivingReferences (..))
import Aihc.Tc.Deriving.StockClass (StockClass (..), lookupStockClass, stockClasses)
import Data.List (group, sort)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tcStockClassTests :: TestTree
tcStockClassTests =
  testGroup
    "stock deriving table"
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
      -- A generated body names the values of the reference table, so the
      -- class it belongs to must be one whose package the configuration
      -- pins. The recognized classes carry no package on purpose.
      testCase "every generated class is pinned to a package" $
        assertEqual
          "generated classes that the configuration does not pin"
          []
          [ stockClassName row
          | row <- stockClasses,
            isJust (stockClassMethods row),
            stockClassName row `notElem` pinnedClasses
          ],
      testCase "no class is listed twice in the table" $
        assertEqual "duplicate table rows" [] (duplicates (map stockClassName stockClasses))
    ]
  where
    references = primDerivingReferences (PackageId "aihc-prim")
    pinnedClasses = [name | (_, _, name) <- derivingStockClasses references]
    locatedClasses = pinnedClasses <> map snd (derivingRecognizedClasses references)

duplicates :: [Text] -> [Text]
duplicates names = [name | name : _ : _ <- group (sort names)]
