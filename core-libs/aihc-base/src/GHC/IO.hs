{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.IO (IO (..)) where

import GHC.Prim (RealWorld, State#)

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
