

{-# LANGUAGE FlexibleInstances #-}


-- | This module defines an interface for 'WQO's on 'Op'erators,
--   for example, that are used as the precedence for an [RPQO]("Language.REST.RPO").
module Language.REST.Internal.OpOrdering (
    empty
  , OpOrdering
  , opGT
  , opEQ
  , (=.)
  , (>.)
  , (<.)
  , parseOO
  ) where

import Prelude hiding (GT, EQ)
import Data.Maybe
import qualified Data.Text as T
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)

import           Language.REST.Op
import           Language.REST.Internal.WQO as WQO


type OpOrdering   = WQO Op


-- | @opGT o f g@ returns @true@ if @f > g@ in @o@
opGT :: OpOrdering -> Op -> Op -> Bool
opGT s f g = getRelation s f g == Just QGT

-- | @opEQ o f g@ returns @true@ if @f = g@ in @o@
opEQ :: OpOrdering -> Op -> Op -> Bool
opEQ s f g = getRelation s f g == Just QEQ

-- |  @f >. g@ generates a new ordering with @f@ greater than @g@.
--   This function is undefined if f == g.
(>.) :: Op -> Op -> OpOrdering
(>.) f g = fromJust $ WQO.singleton (f, g, QGT)

-- |  @f =. g@ generates a new ordering with @f@ equal to @g@.
--   This function is undefined if f == g.
(=.) :: Op -> Op -> OpOrdering
(=.) f g = fromJust $ WQO.singleton (f, g, QEQ)

-- |  @f <. g@ generates a new ordering with @f@ less than @g@.
--   This function is undefined if f == g.
(<.) :: Op -> Op -> OpOrdering
(<.) f g = g >. f

-- | @parseOO str@ returns the ordering defined by @str@. If the input describes
--   /any/ ordering, (i.e "f = f"), then this function returns 'Nothing'.
parseOO :: String -> Maybe OpOrdering
parseOO str =
  case parse parser "" str of
    Left err -> error (show err)
    Right t  -> t

parser :: Parsec String u (Maybe OpOrdering)
parser = fmap mergeAll' (sepBy1 atom conj) where

  mergeAll' :: [Maybe OpOrdering] -> Maybe OpOrdering
  mergeAll' [x]                     = x
  mergeAll' (Just x : Just x' : xs) =
    do
      x'' <- merge x x'
      mergeAll' (Just x'' : xs)
  mergeAll' _                       = Nothing

  conj = spaces >> (char '\8743' <|> char '^') >> spaces
  eq   = spaces >> char '=' >> spaces
  gt   = spaces >> char '>' >> spaces


  atom = try gtAtom <|> try eqAtom

  eqAtom = fmap allEQ (sepBy1 sym (try eq))
    where
      mkEQ f g = WQO.singleton (f, g, QEQ)
      allEQ syms =
        let
          pairs = zipWith mkEQ syms (tail syms)
        in
          mergeAll' pairs

  gtAtom = do
    left  <- sym
    _     <- gt
    right <- sym
    return $ WQO.singleton (left, right, QGT)

  sym = fmap (Op . T.pack) (many (alphaNum <|> char '+' <|> char '*'))
