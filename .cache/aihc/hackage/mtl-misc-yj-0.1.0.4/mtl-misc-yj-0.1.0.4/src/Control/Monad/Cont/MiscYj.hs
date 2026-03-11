{-# LANGUAGE BlockArguments #-}

module Control.Monad.Cont.MiscYj where

import Control.Monad
import Control.Monad.Cont

mapContM :: (a -> (b -> m c) -> m c) -> [a] -> ([b] -> m c) -> m c
mapContM f = runContT . mapM (ContT . f)

replicateContM :: Int -> ((a -> m b) -> m b) -> (([a] -> m b) -> m b)
replicateContM n = runContT . replicateM n . ContT
