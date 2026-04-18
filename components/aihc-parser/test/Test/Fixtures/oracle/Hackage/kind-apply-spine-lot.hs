{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module M where

data LoT k

data a :&&: b

type family SpineLoT (tys :: LoT k) = (tys' :: LoT k) | tys' -> tys where
  SpineLoT (a ':&&: as) = a ':&&: SpineLoT as
  SpineLoT 'LoT0 = 'LoT0
