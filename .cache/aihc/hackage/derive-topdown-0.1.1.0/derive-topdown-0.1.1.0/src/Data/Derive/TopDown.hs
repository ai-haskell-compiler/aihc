-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Derive.TopDown
-- Copyright   :  (c) Song Zhang
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  haskell.zhang.song `at` hotmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Derive.TopDown (
   module Data.Derive.TopDown.Standalone
 , module Data.Derive.TopDown.Instance
 , module Data.Derive.TopDown.TH
 , module Data.Derive.TopDown.CxtGen
#if __GLASGOW_HASKELL__ >= 802
 , stock, anyclass, newtype_
 , DerivStrategy(StockStrategy, AnyclassStrategy, NewtypeStrategy)
#endif
)
where

#if __GLASGOW_HASKELL__ >= 802
import Language.Haskell.TH.Syntax
#endif
import Data.Derive.TopDown.Standalone
import Data.Derive.TopDown.Instance
import Data.Derive.TopDown.TH
import Data.Derive.TopDown.CxtGen

#if __GLASGOW_HASKELL__ >= 802
{-|
The name @sock@ and @anyclass@ are still allowed to be used as functions or arguments. 

See <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/DerivingStrategies>
-}
stock, anyclass, newtype_ :: DerivStrategy
stock     = StockStrategy
anyclass  = AnyclassStrategy
newtype_  = NewtypeStrategy
#endif