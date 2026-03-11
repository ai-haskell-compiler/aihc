module Text.Format.Flags (
	FormatFlags,
	getFlag, hasFlag,
	baseFlag, preciseFlag
	) where

import Prelude.Unicode

import Control.Applicative
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Text.Read

type FormatFlags = [String]

getFlag ∷ (String → Maybe a) → [String] → Maybe a
getFlag fn = listToMaybe ∘ mapMaybe fn

hasFlag ∷ String → [String] → Bool
hasFlag = (∈)

getFlagValue ∷ Read a ⇒ String → [String] → Maybe a
getFlagValue nm fmts = do
	f ← getFlag (stripPrefix (nm ++ "=")) fmts
	readMaybe f

baseFlag ∷ (Read a, Integral a) ⇒ [String] → a
baseFlag fmts
	| hasFlag "bin" fmts ∨ hasFlag "b" fmts = 2
	| hasFlag "octal" fmts ∨ hasFlag "o" fmts = 8
	| hasFlag "hex" fmts ∨ hasFlag "h" fmts = 16
	| otherwise = fromMaybe 10 (getFlagValue "base" fmts <|> getFlagValue "b" fmts)

preciseFlag ∷ [String] → Maybe Int
preciseFlag fmts = read <$> listToMaybe (mapMaybe preciseFlag' fmts) where
	preciseFlag' ('d':d) = Just d
	preciseFlag' _ = Nothing
