-- |
--
-- Module:      Language.Egison.Parser.Pattern.Parsable
-- Description: Type class providing functions for parsing
-- Stability:   experimental
--
-- This module provides a type class for parsing many types.

{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Egison.Parser.Pattern.Parsable
  ( Parsable(..)
  )
where

import           Control.Monad                  ( unless )
import           Control.Monad.Except           ( MonadError(..) )
import           Data.Bifunctor                 ( first )
import           Control.Comonad.Cofree         ( Cofree )
import           Control.Comonad.Trans.Cofree   ( CofreeF(..) )
import           Data.Functor.Foldable          ( Base
                                                , Recursive(..)
                                                , Corecursive(..)
                                                )

import           Language.Egison.Parser.Pattern.Prim.Location
                                                ( Location )
import           Language.Egison.Parser.Pattern.Prim.Source
                                                ( Source(..) )
import           Language.Egison.Parser.Pattern.Prim.Error
                                                ( Errors )
import qualified Language.Egison.Parser.Pattern.Prim.Error
                                               as Error
                                                ( Error(..) )


unAnnotate :: (Recursive x, Corecursive x) => Cofree (Base x) a -> x
unAnnotate = cata go where go (_ :< x) = embed x

-- | Type class providing functions for parsing.
class Source s => Parsable a s mode where
  -- | Parse a source stream.
  parse :: MonadError (Errors s) m => mode -> s -> m a
  -- | Parse a source stream with location annotations.
  parseWithLocation :: MonadError (Errors s) m => mode -> s -> m (Cofree (Base a) Location)
  -- | Parse a source stream non-greedily. That is, this parser will only consume the input until a is fully parsed, and return the rest of the input.
  parseNonGreedy :: MonadError (Errors s) m => mode -> s -> m (a, s)
  -- | Parse a source stream non-greedily with location annotations.
  parseNonGreedyWithLocation :: MonadError (Errors s) m => mode -> s -> m (Cofree (Base a) Location, s)

  parseWithLocation mode s = do
    (a, rest) <- parseNonGreedyWithLocation @a mode s
    unless (eof rest) . throwError . pure . Error.UnexpectedEndOfFile $ tokens rest
    pure a

  default parseNonGreedy :: (Recursive a, Corecursive a, MonadError (Errors s) m) => mode -> s -> m (a, s)
  parseNonGreedy mode = fmap (first unAnnotate) . parseNonGreedyWithLocation @a mode

  default parse :: (Recursive a, Corecursive a, MonadError (Errors s) m) => mode -> s -> m a
  parse mode = fmap unAnnotate . parseWithLocation @a mode
