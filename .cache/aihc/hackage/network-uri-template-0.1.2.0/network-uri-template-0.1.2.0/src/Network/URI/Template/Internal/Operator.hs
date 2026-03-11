-- |
--
-- Module      : Network.URI.Template.Internal.Operator
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.Operator
  ( Operator (..)
  , operatorP
  , operatorPretty
  , OperatorActions (..)
  , operatorActions
  , nullOperatorActions
  ) where

import Prelude

import Data.Text (Text, pack, unpack)
import Network.URI (escapeURIString, isReserved, isUnescapedInURIComponent)
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Internal.Pretty
import Network.URI.Template.VarName

data Operator
  = Reserved
  | Fragments
  | Labels
  | PathSegments
  | PathParameters
  | Query
  | QueryContinuation
  | SpecReserved Char
  deriving stock (Eq, Show)

-- |
--
-- @
-- operator      =  op-level2 / op-level3 / op-reserve
-- op-level2     =  "+" / "#"
-- op-level3     =  "." / "/" / ";" / "?" / "&"
-- op-reserve    =  "=" / "," / "!" / "@" / "|"
-- @
operatorP :: Parser Operator
operatorP =
  choice
    [ Reserved <$ char '+' <?> "reserved operator"
    , Fragments <$ char '#' <?> "fragment operator"
    , Labels <$ char '.' <?> "label operator"
    , PathSegments <$ char '/' <?> "path segment operator"
    , PathParameters <$ char ';' <?> "path parameter operator"
    , Query <$ char '?' <?> "query operator"
    , QueryContinuation <$ char '&' <?> "query continuation operator"
    , SpecReserved <$> oneOf ("=,!@|" :: String) <?> "spec-reserved operator"
    ]

operatorPretty :: Operator -> Doc ann
operatorPretty = \case
  Reserved -> "+"
  Fragments -> "#"
  Labels -> "."
  PathSegments -> "/"
  PathParameters -> ";"
  Query -> "?"
  QueryContinuation -> "&"
  SpecReserved c -> pretty c

data OperatorActions = OperatorActions
  { listPrefix :: Text
  , listIntercalate :: Text
  , renderValue :: VarName -> Doc Ann -> Doc Ann
  , escapeValue :: Text -> Text
  }

-- | 'OperatorActions' to use when there was no 'Operator'
nullOperatorActions :: OperatorActions
nullOperatorActions =
  OperatorActions
    { listPrefix = ""
    , listIntercalate = ","
    , escapeValue = escapeURIText isUnescapedInURIComponent
    , renderValue = \_ v -> v
    }

operatorActions :: Operator -> OperatorActions
operatorActions = \case
  Reserved ->
    nullOperatorActions
      { escapeValue = escapeURIText $ \c ->
          or
            [ isUnescapedInURIComponent c
            , isReserved c
            ]
      }
  Fragments ->
    nullOperatorActions
      { listPrefix = "#"
      , escapeValue = escapeURIText $ \c ->
          or
            [ isUnescapedInURIComponent c
            , isReserved c
            ]
      }
  Labels ->
    nullOperatorActions
      { listPrefix = "."
      , listIntercalate = "."
      }
  PathSegments ->
    nullOperatorActions
      { listPrefix = "/"
      , listIntercalate = "/"
      }
  PathParameters ->
    nullOperatorActions
      { listPrefix = ";"
      , listIntercalate = ";"
      , renderValue = \k v ->
          mconcat
            [ annotate AnnVarName $ pretty k
            , if isEmptyDoc v then "" else annotate AnnPunctuation "="
            , v
            ]
      }
  Query ->
    nullOperatorActions
      { listPrefix = "?"
      , listIntercalate = "&"
      , renderValue = \k v ->
          mconcat
            [ annotate AnnVarName $ pretty k
            , annotate AnnPunctuation "="
            , v
            ]
      }
  QueryContinuation ->
    nullOperatorActions
      { listPrefix = "&"
      , listIntercalate = "&"
      , renderValue = \k v ->
          mconcat
            [ annotate AnnVarName $ pretty k
            , annotate AnnPunctuation "="
            , v
            ]
      }
  SpecReserved {} -> nullOperatorActions

escapeURIText :: (Char -> Bool) -> Text -> Text
escapeURIText p = pack . escapeURIString p . unpack

isEmptyDoc :: Doc ann -> Bool
isEmptyDoc = (== "") . show -- hack
