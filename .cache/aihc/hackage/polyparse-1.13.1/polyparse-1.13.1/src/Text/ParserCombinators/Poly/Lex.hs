-- Author: Malcolm Wallace

-- | In a strict language, where creating the entire input list of tokens
--   in one shot may be infeasible, we can use a lazy "callback" kind of
--   architecture instead.  The lexer returns a single token at a time,
--   together with a continuation.
--
--   This module defines a Parser type (capable of use with the Poly
--   combinators), specialised to the callback-lexer style of input stream.

module Text.ParserCombinators.Poly.Lex
  ( -- * The Parser datatype
    LexReturn(..)
  , Parser(P)
  , Result(..)
  , runParser
    -- ** Basic parsers
  , next
  , eof
  , satisfy
  , onFail
    -- ** Re-parsing
  , reparse
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where


import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Control.Applicative
import qualified Control.Monad.Fail as Fail

-- | In a strict language, where creating the entire input list of tokens
--   in one shot may be infeasible, we can use a lazy "callback" kind of
--   architecture instead.  The lexer returns a single token at a time,
--   together with a continuation.  The @next@ parser is responsible for
--   pulling on the token stream, applying the continuation where necessary.
data LexReturn t = LexReturn t String (String->LexReturn t)
                 | LexFinish

-- | This @Parser@ datatype is a specialised parsing monad with error
--   reporting.  This version is specialised to pre-lexed String input,
--   where the lexer has been written to yield a @LexReturn@.
newtype Parser t a = P (LexReturn t -> Result (LexReturn t) a)

-- | Apply a parser to an input token sequence.
runParser :: Parser t a -> LexReturn t -> (Either String a, String)
runParser (P p) = (\ (a,b)->(a,stripLex b)) . resultToEither . p
  where stripLex  LexFinish        = ""
        stripLex (LexReturn _ s _) = s

instance Functor (Parser t) where
    fmap f (P p) = P (fmap f . p)

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

instance Fail.MonadFail  (Parser t) where
    fail e       = P (\ts-> Failure ts e)

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
                     ++(indent 2 . unlines . map showErr . reverse $ errs))
            accum errs ((e,P p):ps) =
                P (\ts-> case p ts of
                           Failure _ err ->
                                       let (P p') = accum ((e,err):errs) ps
                                       in p' ts
                           r@(Success _ _)    -> r
                           r@(Committed _)    -> r )
            showErr (name,err) = name ++ "\n" ++ indent 2 err

infixl 6 `onFail`       -- not sure about precedence 6?

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
(P p) `onFail` (P q) = P (\ts-> continue ts $ p ts)
      where
        continue ts (Failure _ _) = q ts
    --  continue _  (Committed r) = r    -- no, remain Committed
        continue _  r             = r

instance Applicative (Parser t) where
    pure x    = P (\ts-> Success ts x)
    pf <*> px = do { f <- pf; x <- px; return (f x) }
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative (Parser t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser t)

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next :: Parser t t
next = P (\ts-> case ts of
                  LexFinish       -> Failure ts "Ran out of input (EOF)"
                  LexReturn t s k -> Success (k s) t)

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof :: Parser t ()
eof = P (\ts -> case ts of
                  LexFinish       -> Success ts ()
                  LexReturn _ _ _ -> Failure ts "Expected end of input (EOF)" )

-- | Return the next token if it satisfies the given predicate.
satisfy :: (t -> Bool) -> Parser t t
satisfy f = do { x <- next
               ; if f x then return x else fail "Parse.satisfy: failed"
               }

------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser t ()
reparse ts  = P (\inp-> Success (ts `prefix` inp) ())
  where
    (t:ts) `prefix` k = LexReturn t "" (const (ts `prefix` k))
    []     `prefix` k = k

------------------------------------------------------------------------
