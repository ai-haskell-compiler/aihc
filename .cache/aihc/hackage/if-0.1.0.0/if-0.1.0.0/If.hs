{-|
Module      : If
Description : Simple if operators
Copyright   : (c) Winterland, 2016
License     : BSD
Maintainer  : drkoster@qq.com
Stability   : stable
Portability : PORTABLE

This module provides two simple operators '?' and '?>' for simple conditions.
Watch out for boolean blindness if you use them too often.
-}

module If where

import Control.Monad (when)

-- | This is the <https://wiki.haskell.org/If-then-else if-then-else> operator,
-- With it you can write @xxx ? yyy $ zzz@ instead of @if xxx then yyy else zzz@.
-- Following may or may not be cleaner to you ; )
--
-- @
--   isFoo <- checkFoo
--   isFoo ? foo
--         $ bar
-- @
--
(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y
infixr 1 ?
{-# INLINABLE (?) #-}

-- | This is the <https://mail.haskell.org/pipermail/libraries/2014-April/022700.html whenM> operator,
-- With it you can write @doesItExists ?> removeIt@ instead of @do {e <- doesItExists; when e removeIt}@.
-- There's not @unlessM@ version, so you have to use 'not'.
--
-- @
--   not \<$\> doesItExists ?> createIt
-- @
--
(?>) :: Monad m => m Bool -> m () -> m ()
mb ?> x = mb >>= \ b -> when b x
infixl 1 ?>
{-# INLINABLE (?>) #-}
