{-# LANGUAGE AllowAmbiguousTypes, TemplateHaskell #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions
      -dsuppress-type-applications
      -dsuppress-module-prefixes -dsuppress-type-signatures
      -dsuppress-uniques #-}
module Control.Subcategory.FoldableSpec where
import Control.Subcategory.Foldable
import Control.Subcategory.Functor

import           Data.MonoTraversable
import qualified Data.Vector           as V
import qualified Data.Vector.Generic   as G
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable  as S
import qualified Data.Vector.Unboxed   as U
import           Shared
import           Test.Inspection
import           Test.Tasty
import Data.Function ((&))
import Test.Tasty.ExpectedFailure (expectFailBecause)

cfoldr_uvec :: (Int -> b -> b) -> b -> U.Vector Int -> b
cfoldr_uvec = cfoldr

cfoldr_uvec_poly :: U.Unbox a => (a -> b -> b) -> b -> U.Vector a -> b
cfoldr_uvec_poly = cfoldr

foldr_uvec :: (Int -> b -> b) -> b -> U.Vector Int -> b
foldr_uvec = U.foldr

cfoldr_svec :: (Int -> b -> b) -> b -> S.Vector Int -> b
cfoldr_svec = cfoldr

cfoldr_svec_poly :: S.Storable a => (a -> b -> b) -> b -> S.Vector a -> b
cfoldr_svec_poly = cfoldr

foldr_svec :: (Int -> b -> b) -> b -> S.Vector Int -> b
foldr_svec = S.foldr

cfoldr_pvec :: (Int -> b -> b) -> b -> P.Vector Int -> b
cfoldr_pvec = cfoldr

cfoldr_pvec_poly :: P.Prim a => (a -> b -> b) -> b -> P.Vector a -> b
cfoldr_pvec_poly = cfoldr

foldr_pvec :: (Int -> b -> b) -> b -> P.Vector Int -> b
foldr_pvec = P.foldr

cfoldr_bvec :: (a -> b -> b) -> b -> V.Vector a -> b
cfoldr_bvec = cfoldr

foldr_bvec :: (a -> b -> b) -> b -> V.Vector a -> b
foldr_bvec = V.foldr

cfoldr_list :: (a -> b -> b) -> b -> [a] -> b
cfoldr_list = cfoldr

foldr_list :: (a -> b -> b) -> b -> [a] -> b
foldr_list = foldr

test_cfoldr :: TestTree
test_cfoldr = testGroup "cfoldr"
  [ testGroup "List"
    [ $(inspecting "has the same representation as V.foldr"
        $ 'cfoldr_list ==- 'foldr_list
      )
    , $(inspecting "has no instance dictionary"
      $ hasNoTypeClasses 'cfoldr_list
      )
    ]
  , testGroup "BVector"
    [ $(inspecting "has the same representation as V.foldr"
        $ 'cfoldr_bvec ==- 'foldr_bvec
      )
    , $(inspecting "has no instance dictionary except G.Vector"
      $ 'cfoldr_bvec `hasNoTypeClassesExcept` [''G.Vector]
      )
    ]
  , testGroup "UVector"
    [ $(inspecting "has the same representation as U.foldr (if an element is concrete)"
        $ 'cfoldr_uvec ==- 'foldr_uvec
      )
    , $(inspecting "has no instance dictionary other than Unbox (if polymorphic)"
      $ 'cfoldr_uvec_poly `hasNoTypeClassesExcept` [''U.Unbox]
      ) & if ghcVer >= GHC9_0
      then expectFailBecause "Simplified subsumption sacrifices this and fails for GHC >= 9.0.1."
      else id
    ]
  , testGroup "SVector"
    [ $(inspecting "has the same representation as S.foldr (if an element is concrete)"
        $ 'cfoldr_svec ==- 'foldr_svec
      )
    , $(inspecting "has no instance dictionary other than Storable (if polymorphic)"
      $ 'cfoldr_svec_poly `hasNoTypeClassesExcept` [''S.Storable]
      ) & if ghcVer >= GHC9_0
      then expectFailBecause "Simplified subsumption sacrifices this and fails for GHC >= 9.0.1."
      else id
    ]
  , testGroup "PVector"
    [ $(inspecting "has the same representation as P.foldr (if an element is concrete)"
        $ 'cfoldr_pvec ==- 'foldr_pvec
      )
    , $(inspecting "has no instance dictionary other than Prim (if polymorphic)"
      $ 'cfoldr_pvec_poly `hasNoTypeClassesExcept` [''P.Prim]
      ) & if ghcVer >= GHC9_0
      then expectFailBecause "Simplified subsumption sacrifices this and fails for GHC >= 9.0.1."
      else id
    ]
  ]

cfoldMap_uvec :: Monoid w => (Int -> w) -> U.Vector Int -> w
cfoldMap_uvec = cfoldMap

ofoldMap_uvec :: Monoid w => (Int -> w) -> U.Vector Int -> w
ofoldMap_uvec = ofoldMap

test_cfoldMap :: TestTree
test_cfoldMap = testGroup "cfoldMap"
    [ testGroup "UVector"
      [ $(inspecting "has the same rep as ofoldMap (if elements concrete)"
        $ 'cfoldMap_uvec ==- 'ofoldMap_uvec
        )
      ]
    ]

cinit_list :: [a] -> [a]
cinit_list = cinit

init_list :: [a] -> [a]
{-# INLINE init_list #-}
init_list = init

test_cinit :: TestTree
test_cinit = testGroup "cinit"
  [ testGroup "List"
    [ (if ghcVer >= GHC9_4 then expectFailBecause "GHC >= 9.4 does different thing on CallStack" else id)
      $(inspecting "has the same represeitation as Prelude.init"
      $ 'cinit_list ==- 'init_list
      )
    ]
  ]

ctoList_list :: [a] -> [a]
ctoList_list = ctoList

list_id :: [a] -> [a]
list_id = id

list_id_lam :: [a] -> [a]
list_id_lam = \x -> x

cfromList_list :: [a] -> [a]
cfromList_list = cfromList

test_ctoList :: TestTree
test_ctoList = testGroup "ctoList"
  [ testGroup "List"
    [ $(inspecting "has the same represeitation as Prelude.id"
      $ 'ctoList_list ==- 'list_id
      )
    ]
  ]

test_cfromList :: TestTree
test_cfromList = testGroup "cfromList"
  [ testGroup "List"
    [ $(inspecting "has the same represeitation as Prelude.id"
      $ 'cfromList_list ==- 'list_id
      )
    ]
  ]


ctoFromList_list :: [a] -> [a]
ctoFromList_list = ctoList . cfromList @[]

ctoFromList_bvec :: [a] -> [a]
ctoFromList_bvec xs = ctoList (cfromList @V.Vector xs)

ctoFromList_poly :: forall f a. (CFreeMonoid f, Dom f a) => [a] -> [a]
ctoFromList_poly xs = ctoList (cfromList @f xs)

list_id_with_constr :: forall f a. (CFreeMonoid f, Dom f a) => [a] -> [a]
list_id_with_constr = \xs -> xs

test_rules :: TestTree
test_rules = testGroup "Rewrite rules"
  [ testGroup "ctoList . cfromList = ctoList"
    [ $(inspecting "List"
        $ 'ctoFromList_list ==- 'list_id_lam
      )
    , $(inspecting "Boxed vector"
        $ 'ctoFromList_bvec ==- 'list_id_lam
      )
    , $(inspecting "Polymorphic (up to dictionary leftover)"
        $ 'ctoFromList_poly ==- 'list_id_with_constr
      )
    ]
  ]

cgen_bvec, gen_bvec :: Int -> (Int -> a) -> V.Vector a
cgen_bvec = cgenerate
gen_bvec = V.generate

test_generate :: TestTree
test_generate = testGroup "cgenerate"
  [ $(inspecting "Boxed Vector" $ 'cgen_bvec ==- 'gen_bvec)
  ]

crev_list, rev_list :: [a] -> [a]
crev_list = creverse
rev_list = reverse

test_reverse :: TestTree
test_reverse = testGroup "creverse"
  [ $(inspecting "List" $ 'crev_list ==- 'rev_list)
  ]
