{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}

-- |
--
-- Module:      Language.Egison.Parser.Pattern.Prim.Source
-- Description: Constraint for the source of parser
-- Stability:   experimental
--
-- A constraint and property of the source of parser

module Language.Egison.Parser.Pattern.Prim.Source
  ( Source(..)
  , Token
  , Tokens
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
                                                ( null
                                                , cons
                                                , snoc
                                                )
import qualified Text.Megaparsec               as Parsec
                                                ( Stream(..)
#if MIN_VERSION_megaparsec(9,0,0)
                                                , TraversableStream(..)
#endif
                                                )

import           Language.Egison.Parser.Pattern.Token
                                                ( IsToken )


-- | Type of token in the source.
type Token s = Parsec.Token s
-- | Type of tokens in the source.
type Tokens s = Parsec.Tokens s


-- | Constraint for the source of parser.
-- TODO: Hide these methods in haddock (see haskell/haddock#330)
#if MIN_VERSION_megaparsec(9,0,0)
class (Parsec.TraversableStream s, IsToken (Token s)) => Source s where
#else
class (Parsec.Stream s, IsToken (Token s)) => Source s where
#endif
  -- | Check if the stream is null or not.
  eof :: s -> Bool
  -- | Reify the input stream into a chunk of tokens.
  tokens :: s -> Tokens s
  -- | Add a token to the front of a chunk.
  consTokens :: Token s -> Tokens s -> Tokens s
  -- | Add a token to the back of a chunk.
  snocTokens :: Tokens s -> Token s -> Tokens s

instance Source Text where
  eof        = T.null
  tokens     = id
  consTokens = T.cons
  snocTokens = T.snoc

instance Source String where
  eof        = null
  tokens     = id
  consTokens = (:)
  snocTokens xs x = xs ++ [x]
