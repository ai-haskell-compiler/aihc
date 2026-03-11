{-# LANGUAGE Trustworthy #-}

-- |
-- Module      :  Data.Symbol
-- Copyright   :  (c) Harvard University 2009-2011
--             :  (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  Geoffrey Mainland <mainland@cs.drexel.edu>

module Data.Symbol (
    Symbol,
    intern,
    unintern
  ) where

import Data.Symbol.Unsafe
