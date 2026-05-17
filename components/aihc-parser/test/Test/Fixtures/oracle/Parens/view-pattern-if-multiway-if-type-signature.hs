{- ORACLE_TEST pass -}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE ViewPatterns #-}
module M where

x (#
     | if [] then [] else if | []
                                ->
                                 []
                                  :: _
      -> _
     |  #) = ()
