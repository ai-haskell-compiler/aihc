module TypeFun.Data.Eq
  ( Equal
  ) where

type family Equal (a :: k) (b :: k) :: Bool where
  Equal a a = 'True
  Equal a b = 'False
