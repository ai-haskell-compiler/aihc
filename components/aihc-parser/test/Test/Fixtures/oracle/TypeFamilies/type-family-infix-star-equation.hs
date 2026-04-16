{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE TypeOperators #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE NoStarIsType #-}

type family (a :: ExactPi') * (b :: ExactPi') :: ExactPi' where
  'ExactPi z p q * 'ExactPi z' p' q' = 'ExactPi undefined undefined undefined
