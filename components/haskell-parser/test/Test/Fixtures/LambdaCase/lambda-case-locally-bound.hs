{-# LANGUAGE LambdaCase #-}

module LambdaCaseLocallyBound where

select :: Either Int Int -> Int
select e =
  let pick = \case
        Left x -> x
        Right y -> y
   in pick e
