{-|
Module      : Control.Concurrent.STMSupply
Description : Main module
Copyright   : (c) Joseph Canero, 2016
License     : BSD-3
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX
-}

module Control.Concurrent.STMSupply
(
  STMSupply
, newSTMSupplyIO
, freshId
, splitSupply
) where

import Control.Applicative
import qualified Control.Concurrent.Supply as S
import GHC.Conc

-- | newtype wrapper around a TVar Supply.
newtype STMSupply = Supply {
    unSupply :: TVar S.Supply
  }

-- | Construct a new @STMSupply@ in the IO Monad.
newSTMSupplyIO :: IO STMSupply
newSTMSupplyIO = Supply <$> (newTVarIO =<< S.newSupply)

-- | Using an @STMSupply@, atomically get a fresh ID.
freshId :: STMSupply -> STM Int
freshId Supply{unSupply = s} = do
  (i, s') <- S.freshId <$> readTVar s
  writeTVar s s' >> return i

-- | Using an @STMSupply@, atomically split the underlying @Supply@ into two.
--  Stores one of the new supplies in the STMSupply that was the first
--  argument, and returns the second Supply.
splitSupply :: STMSupply -> STM STMSupply
splitSupply Supply{unSupply = s} = do
  (s1, s2) <- S.splitSupply <$> readTVar s
  writeTVar s s1 >> (Supply <$> newTVar s2)
