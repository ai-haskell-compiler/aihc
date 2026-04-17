{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

type family (a :: ExactPi') * (b :: ExactPi') :: ExactPi' where
  'ExactPi z p q * 'ExactPi z' p' q' = 'ExactPi undefined undefined undefined
