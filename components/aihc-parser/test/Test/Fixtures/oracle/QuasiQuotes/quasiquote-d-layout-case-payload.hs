{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
module QuasiQuoteDLayoutCasePayload where

[] = [d| a | case case [] of
  $a -> 0.0 of {  } = '' C |]
