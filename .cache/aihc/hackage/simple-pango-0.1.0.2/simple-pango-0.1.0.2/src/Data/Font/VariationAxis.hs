{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Font.VariationAxis (
	-- * AXIS CLASS
	FontDescriptionAxis,
	-- * ADD AXIS
	fontDescriptionAddAxis,
	-- * SET AND GET AXIS
	Variations, variationsEmpty, variationsSetAxis, variationsGetAxis,
	-- * SHOW AND READ VARIATIONS
	showVariations, readVariations,
	-- * DEFAULT AXES
	Weight(..), Width(..), Slant(..), Italic(..), OpticalSize(..)
	) where

import Data.Font.VariationAxis.Internal

fontDescriptionAddAxis "Weight" "wght"
fontDescriptionAddAxis "Width" "wdth"
fontDescriptionAddAxis "Italic" "ital"
fontDescriptionAddAxis "OpticalSize" "opsz"
fontDescriptionAddAxis "Slant" "slnt"
