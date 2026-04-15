{- ORACLE_TEST pass -}
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
