{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, TypeFamilies, KindSignatures #-}
module PromotedTypesInTypeFamilyEquations where

-- Type family with promoted empty list in equation
type family HeadList (xs :: [*]) :: * where
  HeadList '[] = Int
  HeadList (x ': xs) = x

-- Type family with promoted cons in equation
type family Append (xs :: [*]) (ys :: [*]) :: [*] where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- Type family with promoted tuple
type family FstPair (a :: *) (b :: *) :: * where
  FstPair '(a, b) = a

-- Type family with promoted constructor
type family IsTrue (b :: Bool) where
  IsTrue 'True = Int
  IsTrue 'False = Char

-- Type family combining [*] kind parameter and promoted equations
type family F (xs :: [*]) (r :: *) :: * where
  F '[] r = r
  F (x ': xs) r = x
