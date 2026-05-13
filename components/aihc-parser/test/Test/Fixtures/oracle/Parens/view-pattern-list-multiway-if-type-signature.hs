{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
module M where

x [if | []
        ->
         []
          :: _
   -> _] = ()
