{- ORACLE_TEST pass -}
{-# LANGUAGE LambdaCase #-}

module LambdaCaseInApplication where

describeMany :: [Maybe Int] -> [String]
describeMany =
  map
    (\case
        Just n -> "just"
        Nothing -> "nothing"
    )