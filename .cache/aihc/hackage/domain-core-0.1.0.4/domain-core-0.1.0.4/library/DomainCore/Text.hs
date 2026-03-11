module DomainCore.Text where

import qualified Data.Char as Char
import Data.Text
import DomainCore.Prelude

recordField :: Bool -> Bool -> Text -> Text -> Text
recordField underscore prefixWithTypeName a b =
  bool mempty "_" underscore
    <> bool b (lcFirst a <> ucFirst b) prefixWithTypeName

sumConstructor :: Text -> Text -> Text
sumConstructor a b =
  ucFirst b <> a

mapFirstChar :: (Char -> Char) -> Text -> Text
mapFirstChar fn =
  foldMap (\(a, b) -> cons (fn a) b)
    . uncons

ucFirst :: Text -> Text
ucFirst =
  mapFirstChar Char.toUpper

lcFirst :: Text -> Text
lcFirst =
  mapFirstChar Char.toLower
