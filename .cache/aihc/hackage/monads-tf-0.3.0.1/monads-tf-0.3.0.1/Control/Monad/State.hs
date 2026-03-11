-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- State monads.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State (
  module Control.Monad.State.Lazy
  ) where

import Control.Monad.State.Lazy
