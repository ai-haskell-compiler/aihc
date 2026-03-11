{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Spoon
-- Copyright   :  (c) Matt Morrow & Dan Peebles
-- License     :  see LICENSE
-- 
-- Maintainer  :  pumpkingod@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (Scoped Type Variables)
--
-- Two functions for catching pureish exceptions in pure values. This library
-- considers pureish to be any error call or undefined, failed pattern matches, 
-- arithmetic exceptions, and array bounds exceptions.
--
-----------------------------------------------------------------------------


module Control.Spoon
    ( Handles
    , defaultHandles
    , spoon
    , spoonWithHandles
    , teaspoon
    , teaspoonWithHandles
    ) where

import Control.Exception
import Control.DeepSeq
import System.IO.Unsafe

type Handles a = [Handler (Maybe a)]

{-# INLINEABLE defaultHandles #-}
defaultHandles :: Handles a
defaultHandles =
    [ Handler $ \(_ :: ArithException)   -> return Nothing
    , Handler $ \(_ :: ArrayException)   -> return Nothing
    , Handler $ \(_ :: ErrorCall)        -> return Nothing
    , Handler $ \(_ :: PatternMatchFail) -> return Nothing
    , Handler $ \(x :: SomeException)    -> throwIO x ]

-- | Evaluate a value to normal form and return Nothing if any exceptions are thrown during evaluation. For any error-free value, @spoon = Just@.
{-# INLINEABLE spoonWithHandles #-}
spoonWithHandles :: NFData a => Handles a -> a -> Maybe a
spoonWithHandles handles a = unsafePerformIO $
    deepseq a (Just `fmap` return a) `catches` handles

-- | Evaluate a value to normal form and return Nothing if any exceptions are thrown during evaluation. For any error-free value, @spoon = Just@.
{-# INLINE spoon #-}
spoon :: NFData a => a -> Maybe a
spoon = spoonWithHandles defaultHandles

{-# INLINEABLE teaspoonWithHandles #-}
teaspoonWithHandles :: Handles a -> a -> Maybe a
teaspoonWithHandles handles a = unsafePerformIO $
    (Just `fmap` evaluate a) `catches` handles

-- | Like 'spoon', but only evaluates to WHNF.
{-# INLINE teaspoon #-}
teaspoon :: a -> Maybe a
teaspoon = teaspoonWithHandles defaultHandles

