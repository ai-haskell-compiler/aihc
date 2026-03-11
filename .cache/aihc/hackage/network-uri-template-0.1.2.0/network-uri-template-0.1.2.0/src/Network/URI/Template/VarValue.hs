-- |
--
-- Module      : Network.URI.Template.VarValue
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.VarValue
  ( VarValue (..)
  , varValueP
  , varValuePretty
  ) where

import Prelude

import Data.String (IsString (..))
import Data.Text (Text, pack)
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty

data VarValue
  = VarNull
  | VarValue Text
  | VarList [Text]
  | VarMap [(Text, Text)]
  deriving stock (Show)

instance IsString VarValue where
  fromString = VarValue . pack

varValueP :: Parser VarValue
varValueP =
  choice
    [ varMapP <?> "map variable value"
    , varListP <?> "list variable value"
    , varStringP <?> "string variable value"
    , varNumberP <?> "numeric variable value"
    , varNullP <?> "null variable value"
    ]

varMapP :: Parser VarValue
varMapP = VarMap <$> csvP '[' ']' kvP

kvP :: Parser (Text, Text)
kvP =
  (,)
    <$> (char '(' *> hspace *> quoted <* commaP)
    <*> (quoted <* hspace <* char ')')

varListP :: Parser VarValue
varListP = VarList <$> csvP '(' ')' quoted

varStringP :: Parser VarValue
varStringP = VarValue <$> quoted

varNumberP :: Parser VarValue
varNumberP = VarValue . pack <$> some digitChar

varNullP :: Parser VarValue
varNullP = VarNull <$ string "null"

csvP :: Char -> Char -> Parser a -> Parser [a]
csvP l r p = char l *> hspace *> sepBy p commaP <* hspace <* char r

commaP :: Parser Char
commaP = try (hspace *> char ',' <* hspace) <?> "comma"

varValuePretty :: VarValue -> Doc Ann
varValuePretty = \case
  VarNull -> annotate AnnVarName "null"
  VarValue t -> stPretty t
  VarList ts -> csvPretty "(" ")" $ map stPretty ts
  VarMap kvs -> csvPretty "[" "]" $ map (uncurry kvPretty) kvs

csvPretty :: Text -> Text -> [Doc Ann] -> Doc Ann
csvPretty l r ds =
  hcat
    [ annotate AnnPunctuation $ pretty l
    , hsep $ punctuate (annotate AnnPunctuation comma) ds
    , annotate AnnPunctuation $ pretty r
    ]

stPretty :: Text -> Doc Ann
stPretty t =
  hcat
    [ annotate AnnPunctuation "\""
    , annotate AnnString $ pretty t
    , annotate AnnPunctuation "\""
    ]

kvPretty :: Text -> Text -> Doc Ann
kvPretty k v =
  hcat
    [ annotate AnnPunctuation "("
    , annotate AnnVarName (pretty k)
    , annotate AnnPunctuation ","
    , stPretty v
    , annotate AnnPunctuation ")"
    ]
