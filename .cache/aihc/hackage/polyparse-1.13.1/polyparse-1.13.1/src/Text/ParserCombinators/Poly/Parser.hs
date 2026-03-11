-- | This module contains the definitions for a generic parser, without
--   running state.  These are the parts that are shared between the Plain
--   and Lazy variations.  Do not import this module directly, but only
--   via T.P.Poly.Plain or T.P.Poly.Lazy.
module Text.ParserCombinators.Poly.Parser
  ( -- * The Parser datatype
    Parser(P)   -- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)  -- internal to the Parser Monad.
    -- ** Basic parsers
  , next        -- :: Parser t t
  , eof         -- :: Parser t ()
  , satisfy     -- :: (t->Bool) -> Parser t t
  , satisfyMsg  -- :: Show t => (t->Bool) -> String -> Parser t t
  , onFail      -- :: Parser t a -> Parser t a -> Parser t a

    -- ** Re-parsing
  , reparse     -- :: [t] -> Parser t ()
  ) where

import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Control.Applicative
import qualified Control.Monad.Fail as Fail

-- | This @Parser@ datatype is a fairly generic parsing monad with error
--   reporting.  It can be used for arbitrary token types, not just
--   String input.  (If you require a running state, use module Poly.State
--   instead)
newtype Parser t a = P ([t] -> Result [t] a)

instance Functor (Parser t) where
    fmap f (P p) = P (fmap f . p)

instance Applicative (Parser t) where
    pure x    = P (\ts-> Success ts x)
    pf <*> px = do { f <- pf; x <- px; return (f x) }
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Monad (Parser t) where
    return       = pure
    (P f) >>= g  = P (continue . f)
      where
        continue (Success ts x)             = let (P g') = g x in g' ts
        continue (Committed r)              = Committed (continue r)
        continue (Failure ts e)             = Failure ts e

#if !MIN_VERSION_base(4,13,0)
    fail         = Fail.fail
#endif

instance Fail.MonadFail (Parser t) where
    fail e       = P (\ts-> Failure ts e)

instance Alternative (Parser t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser t)

instance Commitment (Parser t) where
    commit (P p)         = P (Committed . squash . p)
      where
        squash (Committed r) = squash r
        squash r             = r
    (P p) `adjustErr` f  = P (adjust . p)
      where
        adjust (Failure z e) = Failure z (f e)
        adjust (Committed r) = Committed (adjust r)
        adjust  good         = good

    oneOf' = accum []
      where accum errs [] =
                fail ("failed to parse any of the possible choices:\n"
                            ++indent 2 (concatMap showErr (reverse errs)))
            accum errs ((e,P p):ps) =
                P (\ts-> case p ts of
                           Failure _ err ->
                                       let (P p) = accum ((e,err):errs) ps
                                       in p ts
                           r@(Success z a)    -> r
                           r@(Committed _)    -> r )
            showErr (name,err) = name++":\n"++indent 2 err

infixl 6 `onFail`       -- not sure about precedence 6?

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail :: Parser t a -> Parser t a -> Parser t a
(P p) `onFail` (P q) = P (\ts-> continue ts $ p ts)
  where
    continue ts (Failure z e) = q ts
--  continue _  (Committed r) = r   -- no, remain Committed
    continue _  r             = r


------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next :: Parser t t
next = P (\ts-> case ts of
                  []      -> Failure [] "Ran out of input (EOF)"
                  (t:ts') -> Success ts' t )

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof  :: Parser t ()
eof  = P (\ts-> case ts of
                  []      -> Success [] ()
                  (t:ts') -> Failure ts "Expected end of input (EOF)" )

-- | Return the next token if it satisfies the given predicate.
satisfy :: (t->Bool) -> Parser t t
satisfy pred = do { x <- next
                  ; if pred x then return x else fail "Parse.satisfy: failed"
                  }

-- | Return the next token if it satisfies the given predicate.  The
--   String argument describes the function, for better error messages.
satisfyMsg :: Show t => (t->Bool) -> String -> Parser t t
satisfyMsg pred s
             = do { x <- next
                  ; if pred x then return x
                              else fail $ "Parse.satisfy ("++s++") ("
                                                           ++show x++"): failed"
                  }

------------------------------------------------------------------------

-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser t ()
reparse ts  = P (\inp-> Success (ts++inp) ())

------------------------------------------------------------------------
