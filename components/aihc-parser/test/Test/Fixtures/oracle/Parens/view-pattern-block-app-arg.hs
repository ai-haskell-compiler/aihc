{- ORACLE_TEST pass -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternBlockAppArg where

f (#
     []
       if | let {  }
              ->
               []
       []
      -> _
     |  #) = ()
