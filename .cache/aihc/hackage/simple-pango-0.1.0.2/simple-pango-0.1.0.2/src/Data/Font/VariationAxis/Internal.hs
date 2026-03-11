{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Font.VariationAxis.Internal (
	-- * AXIS CLASS
	FontDescriptionAxis,
	-- * ADD AXIS
	fontDescriptionAddAxis,
	-- * SET AND GET AXIS
	Variations, variationsEmpty, variationsSetAxis, variationsGetAxis,
	-- * SHOW AND READ VARIATIONS
	showVariations, readVariations ) where

import Control.Arrow

import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Language.Haskell.TH

class FontDescriptionAxis a where
	fontDescriptionAxisTag :: BS.ByteString
	fontDescriptionAxisToDouble :: a -> Double
	fontDescriptionAxisFromDouble :: Double -> a

newtype Variations = Variations { getVariations :: M.Map BS.ByteString Double }
	deriving Show

variationsEmpty :: Variations
variationsEmpty = Variations M.empty

fontDescriptionAddAxis :: String -> String -> DecsQ
fontDescriptionAddAxis a t = (\n i -> [n, i])
	<$> fontDescriptionAddAxisNewtype a
	<*> fontDescriptionAddAxisInstance a t

fontDescriptionAddAxisNewtype :: String -> DecQ
fontDescriptionAddAxisNewtype a =
	newtypeD (cxt [])
		(mkName a) [] Nothing (recC (mkName a) [
			varBangType (mkName $ "get" ++ a)
				$ bangType (bang noSourceUnpackedness noSourceStrictness) (conT ''Double) ])
		[derivClause Nothing [conT ''Show]]

fontDescriptionAddAxisInstance :: String -> String -> DecQ
fontDescriptionAddAxisInstance a t = instanceD (cxt []) (conT ''FontDescriptionAxis `appT` conT (mkName a)) [
	valD (varP 'fontDescriptionAxisTag) (normalB . litE $ StringL t) [],
	valD (varP 'fontDescriptionAxisToDouble) (normalB . varE . mkName $ "get" ++ a) [],
	valD (varP 'fontDescriptionAxisFromDouble) (normalB . conE $ mkName a) []
	]

showVariations :: Variations -> BS.ByteString
showVariations = BS.intercalate "," . ((\(a, v) -> a <> "=" <> v) . (id *** BSC.pack . show) <$>) . M.toList . getVariations

readVariations :: BS.ByteString -> Variations
readVariations = Variations . M.fromList . ((\[a, v] -> (a, read $ BSC.unpack v)) . BSC.split '=' <$>) . BSC.split ','

variationsSetAxis :: forall a . FontDescriptionAxis a => a -> Variations -> Variations
variationsSetAxis a = Variations . M.insert
		(fontDescriptionAxisTag @a)
		(fontDescriptionAxisToDouble a) . getVariations

variationsGetAxis ::
	forall a . FontDescriptionAxis a => Variations -> Maybe a
variationsGetAxis (Variations as) = fontDescriptionAxisFromDouble
	<$> M.lookup (fontDescriptionAxisTag @a) as
