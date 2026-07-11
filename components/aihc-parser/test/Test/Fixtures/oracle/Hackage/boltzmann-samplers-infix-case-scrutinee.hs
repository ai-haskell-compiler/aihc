{- ORACLE_TEST pass -}
module M where

infixl 9 #!

(#!) = undefined

data A = SomeData B

data B = AlgRep Int | Other

phi xedni i = case xedni #! i of
  SomeData a -> case a of
    AlgRep _ -> \_ _ -> 0
    Other -> \x _ -> x
