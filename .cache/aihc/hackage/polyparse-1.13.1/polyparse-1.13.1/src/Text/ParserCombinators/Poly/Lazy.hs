{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module Text.ParserCombinators.Poly.Lazy
  ( -- * The Parser datatype
    Parser(P)   -- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)  -- internal to the parser monad
  , runParser   -- :: Parser t a -> [t] -> (Either String a, [t])
    -- ** Basic parsers
  , next        -- :: Parser t t
  , eof         -- :: Parser t ()
  , satisfy     -- :: (t->Bool) -> Parser t t
  , satisfyMsg  -- :: (t->Bool) -> String -> Parser t t
  , onFail      -- :: Parser t a -> Parser t a -> Parser t a

    -- ** Re-parsing
  , reparse     -- :: [t] -> Parser t ()
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where

import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import qualified Text.ParserCombinators.Poly.Parser as P
import Control.Applicative
import qualified Control.Monad.Fail as Fail

#if __GLASGOW_HASKELL__
import Control.Exception hiding (bracket)
throwE :: String -> a
throwE msg = throw (ErrorCall msg)
#else
throwE :: String -> a
throwE msg = error msg
#endif

-- | The only differences between a Plain and a Lazy parser are the instance
--   of Applicative, and the type (and implementation) of runParser.
--   We therefore need to /newtype/ the original Parser type, to allow it
--   to have a different instance.
newtype Parser t a = P (P.Parser t a)
#ifdef __GLASGOW_HASKELL__
        deriving (Functor,Monad,Fail.MonadFail,Commitment)
#else
instance Functor (Parser t) where
    fmap f (P p) = P (fmap f p)
instance Monad (Parser t) where
    return x  = P (return x)
#ifndef __MHS__
    fail      = Fail.fail
#endif
    (P f) >>= g = P (f >>= (\(P g')->g') . g)
instance Fail.MonadFail (Parser t) where
    fail e    = P (fail e)
instance Commitment (Parser t) where
    commit (P p)   = P (commit p)
    (P p) `adjustErr` f  = P (p `adjustErr` f)
#endif

-- | Apply a parser to an input token sequence.
runParser :: Parser t a -> [t] -> (a, [t])
runParser (P (P.P p)) = fromResult . p
  where
    fromResult :: Result z a -> (a, z)
    fromResult (Success z a)  =  (a, z)
    fromResult (Failure z e)  =  throwE e
    fromResult (Committed r)  =  fromResult r

instance Applicative (Parser t) where
    pure f    = return f
    --   Apply a parsed function to a parsed value.  This version
    --   is strict in the result of the function parser, but
    --   lazy in the result of the argument parser.  (Argument laziness is
    --   the distinctive feature over other implementations.)
    (P (P.P pf)) <*> px = P (P.P (continue . pf))
      where
        continue (Success z f)  = let (x,z') = runParser px z
                                  in Success z' (f x)
        continue (Committed r)  = Committed (continue r)
        continue (Failure z e)  = Failure z e
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative (Parser t) where
    empty             = fail "no parse"
    (P p) <|> (P q)   = P (p `P.onFail` q)

instance PolyParse (Parser t)

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next    ::  Parser t t
next    = P P.next

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof     :: Parser t ()
eof     = P P.eof

-- | Return the next token if it satisfies the given predicate.
satisfy :: (t->Bool) -> Parser t t
satisfy = P . P.satisfy

-- | Return the next token if it satisfies the given predicate.  The String
--   argument describes the predicate for better error messages.
satisfyMsg :: Show t => (t->Bool) -> String -> Parser t t
satisfyMsg p s = P (P.satisfyMsg p s)

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail  :: Parser t a -> Parser t a -> Parser t a
onFail (P a) (P b) = P (a `P.onFail` b)

-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse :: [t] -> Parser t ()
reparse = P . P.reparse
------------------------------------------------------------------------
