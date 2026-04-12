{- ORACLE_TEST xfail reason="type family equations with infix operator * not parsed correctly" -}
{-# LANGUAGE GHC2021, DataKinds, TypeFamilies, TypeOperators, NoStarIsType #-}

type family (a :: ExactPi') * (b :: ExactPi') :: ExactPi' where
  'ExactPi z p q * 'ExactPi z' p' q' = 'ExactPi undefined undefined undefined
