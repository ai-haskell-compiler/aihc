{- ORACLE_TEST pass -}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module ForeignImportPrim where

import GHC.Exts (Addr#, State#, RealWorld)

foreign import prim "stg_myPrimOp" myPrimOp# :: Addr# -> State# RealWorld -> (# State# RealWorld, Int# #)
