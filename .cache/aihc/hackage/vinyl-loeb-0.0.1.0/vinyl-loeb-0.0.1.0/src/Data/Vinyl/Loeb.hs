{-|
Module      : Data.Vinyl.Loeb
License     : MIT
Maintainer  : dan.firth@homotopic.tech
Stability   : experimental

Loeb's theorem for vinyl extensible records.
-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Data.Vinyl.Loeb (
  rloeb
) where

import Data.Vinyl
import Data.Vinyl.Functor

-- | Version of loeb's theorem for extensible records. Can be
-- used to fill an extensible record lazily using data from
-- the result of the record itself.
rloeb :: RMap xs => Rec ((->) (Rec f xs) :. f) xs  -> Rec f xs
rloeb x = go where go = rmap (($ go) . getCompose) x
