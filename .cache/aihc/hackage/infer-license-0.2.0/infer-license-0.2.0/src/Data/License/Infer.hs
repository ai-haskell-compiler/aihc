{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
module Data.License.Infer (
  License(..)
, inferLicense
) where

import           Control.Applicative
import           Control.Monad
import           Data.Foldable
import           Data.Char
import           Data.List
import           Data.Ord (comparing)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Metrics

import           Data.License.SpdxLicenses (licenses)
import           Data.License.Type

inferLicense :: String -> Maybe License
inferLicense xs = inferLicenseByName xs <|> inferLicenseByLevenshtein xs

inferLicenseByName :: String -> Maybe License
inferLicenseByName (normalize -> xs) = asum $ map (matchName xs) licenseNames

matchName :: String -> (License, String) -> Maybe License
matchName xs (license, name) = license <$ guard (isPrefixOf name xs)

licenseNames :: [(License, String)]
licenseNames = map (fmap normalize) [
    (GPLv2, "GNU GENERAL PUBLIC LICENSE Version 2, June 1991")
  , (GPLv3, "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007")
  , (LGPLv2_1, "GNU LESSER GENERAL PUBLIC LICENSE Version 2.1, February 1999")
  , (LGPLv3, "GNU LESSER GENERAL PUBLIC LICENSE Version 3, 29 June 2007")
  , (AGPLv3, "GNU AFFERO GENERAL PUBLIC LICENSE Version 3, 19 November 2007")
  , (MPL_2_0, "Mozilla Public License Version 2.0")
  , (Apache_2_0, "Apache License Version 2.0, January 2004")
  ]

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

inferLicenseByLevenshtein :: String -> Maybe License
inferLicenseByLevenshtein (T.pack -> xs)
  | T.length xs > 2000 = Nothing
  | otherwise = case maximumBy (comparing snd) (probabilities xs) of
      (license, n) | n > 0.85 -> Just license
      _ -> Nothing

probabilities :: Text -> [(License, Double)]
probabilities license = map (fmap probability) licenses
  where
    probability = realToFrac . levenshteinNorm license
