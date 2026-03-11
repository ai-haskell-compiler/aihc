{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions
      -dsuppress-type-applications
      -dsuppress-module-prefixes -dsuppress-type-signatures
      -dsuppress-uniques #-}
module Control.Subcategory.ZipSpec where
import Control.Subcategory.Zip

import qualified Data.Vector     as V
import           Shared
import           Test.Inspection
import           Test.Tasty

czipWith_vec :: (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
czipWith_vec = czipWith

zipWith_vec :: (a -> b -> c) -> V.Vector a -> V.Vector b -> V.Vector c
zipWith_vec = V.zipWith

czipWith_list :: (a -> b -> c) -> [a] -> [b] -> [c]
czipWith_list = czipWith

zipWith_list :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith_list = Prelude.zipWith

test_czipWith :: TestTree
test_czipWith = testGroup "czipWith"
  [ testGroup "list"
    [ $(inspecting "has the same representation as Prelude.zipWith"
        $ 'czipWith_list ==- 'zipWith_list
      )
    ]
  , testGroup "vector"
    [ $(inspecting "has the same representation as Prelude.zipWith"
        $ 'czipWith_vec ==- 'zipWith_vec
      )
    ]
  ]
