{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Web.HttpApiData.QQ (
  url,
) where

import Data.String (fromString)
import qualified Data.Text as Text
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Web.HttpApiData (toUrlPiece)

import Web.HttpApiData.QQ.Parser

-- |
-- A quasiquoter to build a URL by interpolating values via ToHttpApiData.
-- The resulting value can be any IsString type.
--
-- Currently only supports single variable names being interpolated, not
-- arbitrary Haskell expressions.
--
-- Usage:
--
-- >>> [url|/foo/#{fooId}/bar|]
url :: QuasiQuoter
url =
  QuasiQuoter
    { quoteExp = toExpQ . either error id . parseUrlPieces
    , quotePat = error "'url' quasiquoter cannot be used as a pattern"
    , quoteType = error "'url' quasiquoter cannot be used as a type"
    , quoteDec = error "'url' quasiquoter cannot be used as a declaration"
    }
  where
    -- convert parsed URL pieces into the ExpQ to inject
    toExpQ = appE [|fromString . concat|] . listE . map urlPieceToExpQ
    urlPieceToExpQ = \case
      InterpolatedName name -> appE [|Text.unpack . toUrlPiece|] . varE . mkName $ name
      RawString s -> litE . stringL $ s
