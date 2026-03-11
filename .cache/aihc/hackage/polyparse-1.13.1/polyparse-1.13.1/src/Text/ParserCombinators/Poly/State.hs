module Text.ParserCombinators.Poly.State
  ( -- * The Parser datatype
    Parser(P)   -- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)  -- internal to the parser monad
  , runParser   -- :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
    -- ** Basic parsers
  , next        -- :: Parser s t t
  , eof         -- :: Parser s t ()
  , satisfy     -- :: (t->Bool) -> Parser s t t
  , onFail      -- :: Parser s t a -> Parser s t a -> Parser s t a
    -- ** State-handling
  , stUpdate    -- :: (s->s) -> Parser s t ()
  , stQuery     -- :: (s->a) -> Parser s t a
  , stGet       -- :: Parser s t s
    -- ** Re-parsing
  , reparse     -- :: [t] -> Parser s t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where


import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Text.ParserCombinators.Poly.StateParser
import Control.Applicative

-- The only differences between a State and a StateLazy parser are the instance
-- of Applicative, and the type (and implementation) of runParser.

-- | Apply a parser to an input token sequence.
runParser :: Parser s t a -> s -> [t] -> (Either String a, s, [t])
runParser (P p) = \s-> reTuple . resultToEither . p s
  where
    reTuple (either, (z,s)) = (either, s, z)

------------------------------------------------------------------------
