{- ORACLE_TEST
id: vinyl-loeb-rloeb-section
category: corpus
expected: pass
reason: from vinyl-loeb; parser now supports parenthesized right sections like (($ go) ...)
-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module VinylLoebRloebSection where

rloeb x = go where go = rmap (($ go) . getCompose) x
