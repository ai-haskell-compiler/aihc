{-# LANGUAGE ImportQualifiedPost #-}

{- |
Module    : Data.Ini.Types
Copyright : 2011-2014 Magnus Therning
License   : BSD3
-}
module Data.Ini.Types where

import Control.Arrow (second)
import Data.Map qualified as M

type Config = M.Map SectionName Section

type SectionName = String
type Section = M.Map OptionName OptionValue

type OptionName = String
type OptionValue = String

-- useful since Map doesn't have any Serial instance
cfgFromList :: [(SectionName, [(OptionName, OptionValue)])] -> Config
cfgFromList = M.fromList . map (second M.fromList)

cfgToList :: Config -> [(SectionName, [(OptionName, OptionValue)])]
cfgToList = M.toList . M.map M.toList
