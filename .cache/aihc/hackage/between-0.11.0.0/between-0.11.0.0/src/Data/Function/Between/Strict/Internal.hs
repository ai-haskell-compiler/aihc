{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Definitions used by family of strict function combinators.
-- Copyright:    (c) 2013-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Definitions used by family of strict function combinators. These may come
-- handy, but clash with definitions available via "Prelude".
--
-- /Module available since version 0.11.0.0./
module Data.Function.Between.Strict.Internal
    ( (.)
    , flip
    )
  where

import Prelude (($!))


-- | Strict variant of function composition. Defined as:
--
-- @
-- (f . g) x = f '$!' g '$!' x
-- @
--
-- /Internally used since version 0.10.0.0. Moved to/
-- /"Data.Function.Between.Strict.Internal" module and exposed in version/
-- /0.11.0.0./
(.) :: (b -> c) -> (a -> b) -> a -> c
(f . g) x = f $! g $! x
infixr 9 .
{-# INLINE (.) #-}

-- | Strict variant of 'Data.Function.flip'. Defined as:
--
-- @
-- 'flip' f b a = f '$!' a '$!' b
-- @
--
-- /Since version 0.11.0.0./
flip :: (a -> b -> c) -> b -> a -> c
flip f b a = (f $! a) $! b
{-# INLINE flip #-}
