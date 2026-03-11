{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.Error
-- Description: Parse errors
-- Stability:   experimental
--
-- This module defines datatypes representing parser errors

module Language.Egison.Parser.Pattern.Prim.Error
  ( Error(..)
  , ErrorItem(..)
  , Errors
  -- * Internal Errors
  , CustomError(..)
  -- * Conversion
  , fromParseErrorBundle
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NonEmpty
                                                ( toList )
import qualified Data.Set                      as Set
                                                ( toList
                                                , size
                                                , elemAt
                                                )

import           Language.Egison.Parser.Pattern.Prim.Source
                                                ( Tokens
                                                , Token
                                                )
import           Language.Egison.Parser.Pattern.Prim.Location
                                                ( Position
                                                , fromSourcePos
                                                )

import qualified Text.Megaparsec               as Parsec
                                                ( Stream
                                                , ErrorItem(..)
                                                , ErrorFancy(..)
                                                , SourcePos
#if MIN_VERSION_megaparsec(9,0,0)
                                                , TraversableStream (..)
#endif
                                                , ParseError(..)
                                                , ParseErrorBundle(..)
                                                , attachSourcePos
                                                , errorOffset
                                                , tokensToChunk
                                                )


-- | Token representation in 'Error'.
data ErrorItem s
  = Tokens (Tokens s)
  | Label String
  | EndOfInput

deriving instance Show (Tokens s) => Show (ErrorItem s)
deriving instance Eq (Tokens s) => Eq (ErrorItem s)


-- | Parse error.
data Error s
  = UnexpectedToken { position :: Position
                    , expected :: [ErrorItem s]
                    , found    :: Maybe (ErrorItem s)
                    }
  | ExternalError { position :: Position
                  , input :: Tokens s
                  , message :: String
                  }
  | UnexpectedEndOfFile { rest :: Tokens s
                        }

deriving instance Show (Tokens s) => Show (Error s)
deriving instance Eq (Tokens s) => Eq (Error s)

-- | Type synonym for an error list.
type Errors s = NonEmpty (Error s)


-- | Internal error type to use as a custom error in 'Text.Megaparsec.Parsec' monad.
data CustomError s = ExtParserError { input :: Tokens s
                                    , message :: String
                                    }

deriving instance Eq (Tokens s) => Eq (CustomError s)
deriving instance Ord (Tokens s) => Ord (CustomError s)


makeErrorItem
  :: forall s . Parsec.Stream s => Parsec.ErrorItem (Token s) -> ErrorItem s
makeErrorItem (Parsec.Tokens ts) =
  Tokens . Parsec.tokensToChunk (Proxy @s) $ NonEmpty.toList ts
makeErrorItem (Parsec.Label cs) = Label $ NonEmpty.toList cs
makeErrorItem Parsec.EndOfInput = EndOfInput

makeFancyError
  :: Parsec.SourcePos -> Parsec.ErrorFancy (CustomError s) -> Error s
makeFancyError pos (Parsec.ErrorCustom err) = extError
 where
  position                          = fromSourcePos pos
  ExtParserError { input, message } = err
  extError                          = ExternalError { position, input, message }
makeFancyError _ _ = error "unreachable: unused fancy error"

makeError
  :: forall s
   . Parsec.Stream s
  => (Parsec.ParseError s (CustomError s), Parsec.SourcePos)
  -> Error s
makeError (Parsec.FancyError _ es, pos) | Set.size es == 1 =
  makeFancyError pos $ Set.elemAt 0 es
makeError (Parsec.TrivialError _ mfound expectedSet, pos) = UnexpectedToken
  { position
  , expected
  , found
  }
 where
  found    = fmap (makeErrorItem @s) mfound
  expected = map (makeErrorItem @s) $ Set.toList expectedSet
  position = fromSourcePos pos
makeError _ = error "unreachable: unused error"

-- | Convert 'Parsec.ParseErrorBundle' to 'Errors'.
fromParseErrorBundle
#if MIN_VERSION_megaparsec(9,0,0)
  :: Parsec.TraversableStream s => Parsec.ParseErrorBundle s (CustomError s) -> Errors s
#else
  :: Parsec.Stream s => Parsec.ParseErrorBundle s (CustomError s) -> Errors s
#endif
fromParseErrorBundle Parsec.ParseErrorBundle { Parsec.bundleErrors = errors, Parsec.bundlePosState = posState }
  = fmap makeError errorsWithPos
 where
  (errorsWithPos, _) =
    Parsec.attachSourcePos Parsec.errorOffset errors posState
