{- ORACLE_TEST pass -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

type family Map (f :: k -> l) (xs :: [k]) = (ys :: [l]) where
  Map f (x ': xs) = f x ': Map f xs
  Map f '[] = '[]
