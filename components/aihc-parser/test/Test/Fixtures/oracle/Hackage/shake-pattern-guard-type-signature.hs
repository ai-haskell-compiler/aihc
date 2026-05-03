{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module ShakePatternGuardTypeSignature where

f e =
  case () of
    _
      | Nothing <- fromException e :: Maybe ShakeException
      , Just x <- fromException e ->
          x
    _ -> e
