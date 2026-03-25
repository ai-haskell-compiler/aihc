{-# LANGUAGE Arrows #-}
module Proc where

f g = proc x -> g -< x
