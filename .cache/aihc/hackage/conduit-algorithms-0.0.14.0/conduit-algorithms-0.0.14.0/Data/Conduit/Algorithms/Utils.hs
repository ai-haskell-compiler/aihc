{-|
Module      : Data.Conduit.Algorithms.Utils
Copyright   : 2013-2021 Luis Pedro Coelho
License     : MIT
Maintainer  : luis@luispedro.org

A few miscellaneous conduit utils
-}
module Data.Conduit.Algorithms.Utils
    ( awaitJust
    , enumerateC
    , groupC
    , dispatchC
    , dispatchC_
    ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|))
import           Data.Maybe (maybe)
import           Control.Monad (unless, void)

-- | Act on the next input (do nothing if no input). @awaitJust f@ is equivalent to
--
--
-- @ do
--      next <- C.await
--      case next of
--          Just val -> f val
--          Nothing -> return ()
-- @
--
-- This is a simple utility adapted from
-- http://neilmitchell.blogspot.de/2015/07/thoughts-on-conduits.html
awaitJust :: Monad m => (a -> C.ConduitT a b m ()) -> C.ConduitT a b m ()
awaitJust f = C.await >>= maybe (return ()) f
{-# INLINE awaitJust #-}

-- | Conduit analogue to Python's enumerate function
enumerateC :: Monad m => C.ConduitT a (Int, a) m ()
enumerateC = enumerateC' 0
    where
        enumerateC' !i = awaitJust $ \v -> do
                                        C.yield (i, v)
                                        enumerateC' (i + 1)
{-# INLINE enumerateC #-}

-- | This function is deprecated; use 'Data.Conduit.List.chunksOf'
--
-- groupC yields the input as groups of 'n' elements. If the input is not a
-- multiple of 'n', the last element will be incomplete
--
-- Example:
--
-- @
--      CC.yieldMany [0..10] .| groupC 3 .| CC.consumeList
-- @
--
-- results in @[ [0,1,2], [3,4,5], [6,7,8], [9, 10] ]@
--
groupC :: (Monad m) => Int -> C.ConduitT a [a] m ()
groupC n = loop n []
    where
        loop 0 ps = C.yield (reverse ps) >> loop n []
        loop c ps = C.await >>= \case
            Nothing -> unless (null ps) $ C.yield (reverse ps)
            Just p -> loop (c-1) (p:ps)
{-# WARNING groupC "This function is deprecated; use 'Data.Conduit.List.chunksOf'" #-}

-- | dispatchC dispatches indexed input to the respective sink
-- 
-- Example:
--
-- @
-- 	let input = [(0, "one")
-- 	            ,(1, "two")
-- 	            ,(0, "three")
-- 	            ]
-- 	    CC.yieldMany input .| dispatches [sink1, sink2]
-- @
--
-- Then 'sink1' will receive "one" and "three", while 'sink2' will receive "two"
--
-- Out of bounds indices are clipped to the 0..n-1 range (where 'n' is 'length sinks')
dispatchC :: Monad m => [C.ConduitT a C.Void m r] -> C.ConduitT (Int, a) C.Void m [r]
dispatchC sinks = C.sequenceSinks [select i .| s | (i,s) <- zip [0..] sinks]
    where
        n = length sinks
        select i = CL.mapMaybe $ \(j,val) ->
            if j == i || (i == n - 1 && j >= n) || (i == 0 && j < 0)
                then Just val
                else Nothing

-- | Version of 'dispatchC' that returns ()
dispatchC_ :: Monad m => [C.ConduitT a C.Void m ()] -> C.ConduitT (Int, a) C.Void m ()
dispatchC_ sinks = void $ dispatchC sinks

