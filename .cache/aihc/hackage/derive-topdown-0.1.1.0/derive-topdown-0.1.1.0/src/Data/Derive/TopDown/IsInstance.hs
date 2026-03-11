-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown.IsInstance
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown.IsInstance
  ( isInstance'
  ) where

import           Data.Derive.TopDown.Types
import           Data.Generics
import           GHC.Exts
{-
Note:
  Since GHC template-haskell isInstance function cannot work with 
  polymorphic type. It cannot check whether @Eq a => [a]@ is an 
  instance of 'Eq', here 
  
  >poly_a :: Q Bool
  >poly_a = do
  >    poly_a_t <- [t| forall a. Eq a => [a] |]
  >    isInstance ''Eq [poly_a_t]
  > $(poly_a >>= stringE.show)
  >"False"
  >poly_a' :: Q Bool
  >poly_a' = do
  >    poly_a_t <- [t| forall a. [a] |]
  >    isInstance ''Eq [poly_a_t]
  > $(poly_a >>= stringE.show)
  >"False"
  
  So, here I change all the polymorphic types in the type into 'Any'

  @type family Any :: k where {}@

  See https://gitlab.haskell.org/ghc/ghc/-/issues/10607
-}
import           Language.Haskell.TH

replace_poly_type :: Type -> Type
replace_poly_type (VarT _) = ConT ''Any
replace_poly_type x        = x

replace_poly_type_trans :: Data a => a -> a
replace_poly_type_trans = everywhere (mkT replace_poly_type)

remove_explicit_forall :: Type -> Type
remove_explicit_forall (ForallT _ _ t) = t
remove_explicit_forall t               = t

remove_explicit_forall_trans :: Type -> Type
remove_explicit_forall_trans = everywhere (mkT remove_explicit_forall)

isInstance' :: ClassName -> [Type] -> Q Bool
isInstance' cls tys =
    let trans = remove_explicit_forall_trans . replace_poly_type_trans
      in  isInstance cls (map trans tys)
