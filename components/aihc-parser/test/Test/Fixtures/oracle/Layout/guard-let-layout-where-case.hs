{- ORACLE_TEST pass -}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module GuardLetLayoutWhereCase where

a | let
  []
    | 'J' =
      case 'm'# of
        "" -> ""
    where { (-257) ♛⸵ (x, _) = () }
  [a||] = let {  } in 0
  = ' []
