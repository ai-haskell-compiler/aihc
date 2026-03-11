-- |
--
-- Module      : Network.URI.Template.VarName
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.VarName
  ( VarName
  , varNameP
  , varNamePretty
  , readVarName
  , unVarName
  ) where

import Prelude

import Data.Bifunctor (first)
import Data.String (IsString (..))
import Data.Text (Text, pack)
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty

newtype VarName = VarName
  { unwrap :: Text
  }
  deriving stock (Eq, Ord, Show)

instance IsString VarName where
  fromString = either error id . readVarName

instance Pretty VarName where
  pretty = pretty . (.unwrap)

readVarName :: String -> Either String VarName
readVarName = first errorBundlePretty . parse (varNameP <* eof) . pack

unVarName :: VarName -> Text
unVarName = (.unwrap)

-- |
--
-- @
-- varname       =  varchar *( ["."] varchar )
-- varchar       =  ALPHA / DIGIT / "_" / pct-encoded
-- @
varNameP :: Parser VarName
varNameP = (<?> "variable name") $ do
  v <- varCharP
  vs <- many $ (,) <$> optional (char '.') <*> varCharP

  pure
    $ VarName
    $ foldMap
      ( \case
          (mc, VarChar c) -> pack $ maybe id (:) mc [c]
          (mc, PctEncoded s) -> pack $ maybe id (:) mc s
      )
    $ (Nothing, v) : vs

varNamePretty :: VarName -> Doc Ann
varNamePretty = annotate AnnVarName . pretty . unVarName

data VarChar
  = VarChar Char
  | PctEncoded String

varCharP :: Parser VarChar
varCharP =
  choice
    [ VarChar <$> alphaNumChar
    , VarChar <$> char '_'
    , PctEncoded <$> pcntEncodedP
    ]
    <?> "variable character"

pcntEncodedP :: Parser String
pcntEncodedP =
  sequenceA
    [ char '%'
    , hexDigitChar
    , hexDigitChar
    ]
    <?> "percent-encoded triplet"
