{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module VinylLoebRloebSection where

rloeb x = go where go = rmap (($ go) . getCompose) x
