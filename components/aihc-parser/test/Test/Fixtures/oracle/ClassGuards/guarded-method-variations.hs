{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

class C a b where
  multiParam :: a -> b -> Bool
  multiParam x y | x == x = True
                 | otherwise = False

  infixMethod :: a -> a -> a
  x `infixMethod` y | x == y = x
                    | otherwise = y

  multipleEquations :: a -> a
  multipleEquations x = x
  multipleEquations _ = undefined
