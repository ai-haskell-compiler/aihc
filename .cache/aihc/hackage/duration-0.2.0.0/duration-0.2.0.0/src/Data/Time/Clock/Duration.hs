{-# LANGUAGE TemplateHaskell #-}

{-|
Copyright   : (c) Ryota Kameoka, 2018
License     : BSD-3
Maintainer  : kameoka.ryota@gmail.com
Stability   : experimental

A tiny compile-time time utility library, inspired by <https://github.com/zeit/ms zeit/ms>.
-}

module Data.Time.Clock.Duration
    (
    -- * Quasiquoters
      t
    , s
    , ms
    , µs
    , ns
    , ps
    ) where

import Data.Time.Clock.Duration.QQ
