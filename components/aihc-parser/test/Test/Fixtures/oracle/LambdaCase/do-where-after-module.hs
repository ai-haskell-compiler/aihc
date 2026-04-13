{- ORACLE_TEST xfail reason="TH name quote of empty list constructor in where clause causes layout parser to reject subsequent function definition" -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Foreign.C.Struct.Ord where

import Language.Haskell.TH

f xs = do
	cr <- newName "checkResult"
	letE [valD (varP cr) (normalB $ g cr) []]
		undefined

g fn = do
	x <- newName "x"
	undefined
	where
	h = '[]
