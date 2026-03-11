-- |
-- Module      : Test.LeanCheck.Instances.Void
-- Copyright   : (c) 2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Declares a 'Listable' 'Void' instance.
{-# LANGUAGE CPP #-}
module Test.LeanCheck.Instances.Void () where

#if __GLASGOW_HASKELL__ < 710
-- the Void type is not available
#else
import Data.Void (Void)
import Test.LeanCheck

instance Listable Void where  list  =  []
#endif
