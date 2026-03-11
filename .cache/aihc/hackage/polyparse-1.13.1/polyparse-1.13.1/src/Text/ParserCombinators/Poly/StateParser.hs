-- | This module contains the definitions for a generic parser, with
--   running state.  These are the parts that are shared between the State
--   and StateLazy variations.  Do not import this module directly, but only
--   via T.P.Poly.State or T.P.Poly.StateLazy.
module Text.ParserCombinators.Poly.StateParser
  ( -- * The Parser datatype
    Parser(P)   -- datatype, instance of: Functor, Monad, PolyParse
  , Result(..)  -- internal to the parser monad
    -- ** basic parsers
  , next        -- :: Parser s t t
  , eof         -- :: Parser s t ()
  , satisfy     -- :: (t->Bool) -> Parser s t t
  , onFail      -- :: Parser s t a -> Parser s t a -> Parser s t a
    -- ** State-handling
  , stUpdate    -- :: (s->s) -> Parser s t ()
  , stQuery     -- :: (s->a) -> Parser s t a
  , stGet       -- :: Parser s t s
    -- ** re-parsing
  , reparse     -- :: [t] -> Parser s t ()
  ) where


import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import Control.Applicative
import qualified Control.Monad.Fail as Fail

-- | This @Parser@ datatype is a fairly generic parsing monad with error
--   reporting, and running state.
--   It can be used for arbitrary token types, not just String input.
--   (If you do not require a running state, use module Poly.Plain instead)
newtype Parser s t a = P (s -> [t] -> Result ([t],s) a)

instance Functor (Parser s t) where
    fmap f (P p) = P (\s-> fmap f . p s)

instance Applicative (Parser s t) where
    pure x    = P (\s ts-> Success (ts,s) x)
    pf <*> px = do { f <- pf; x <- px; return (f x) }
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Monad (Parser s t) where
    return       = pure
    (P f) >>= g  = P (\s-> continue . f s)
      where
        continue (Success (ts,s) x)        = let (P g') = g x in g' s ts
        continue (Committed r)             = Committed (continue r)
        continue (Failure tss e)           = Failure tss e

#if !MIN_VERSION_base(4,13,0)
    fail         = Fail.fail
#endif

instance Fail.MonadFail (Parser s t) where
    fail e       = P (\s ts-> Failure (ts,s) e)

instance Alternative (Parser s t) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser s t)

instance Commitment (Parser s t) where
    commit (P p)         = P (\s-> Committed . squash . p s)
      where
        squash (Committed r) = squash r
        squash r             = r
    (P p) `adjustErr` f  = P (\s-> adjust . p s)
      where
        adjust (Failure zs e)  = Failure zs (f e)
        adjust (Committed r)   = Committed (adjust r)
        adjust  good           = good

    oneOf' = accum []
      where accum errs [] =
                fail ("failed to parse any of the possible choices:\n"
                            ++indent 2 (concatMap showErr (reverse errs)))
            accum errs ((e,P p):ps) =
                P (\s ts-> case p s ts of
                           Failure _ err ->
                                       let (P p) = accum ((e,err):errs) ps
                                       in p s ts
                           r@(Success _ a)  -> r
                           r@(Committed _)  -> r )
            showErr (name,err) = name++":\n"++indent 2 err

infixl 6 `onFail`       -- not sure about precedence 6?

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail :: Parser s t a -> Parser s t a -> Parser s t a
(P p) `onFail` (P q) = P (\s ts-> continue s ts $ p s ts)
      where
        continue s ts (Failure _ _) = q s ts
    --  continue _ _  (Committed r)   = r       -- no, remain Committed
        continue _ _  r               = r

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next :: Parser s t t
next = P (\s ts-> case ts of
                  []      -> Failure ([],s) "Ran out of input (EOF)"
                  (t:ts') -> Success (ts',s) t )

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof  :: Parser s t ()
eof  = P (\s ts-> case ts of
                  []      -> Success ([],s) ()
                  (t:ts') -> Failure (ts,s) "Expected end of input (eof)" )


-- | Return the next token if it satisfies the given predicate.
satisfy :: (t->Bool) -> Parser s t t
satisfy pred = do { x <- next
                  ; if pred x then return x else fail "Parse.satisfy: failed"
                  }
------------------------------------------------------------------------
-- State handling

-- | Update the internal state.
stUpdate   :: (s->s) -> Parser s t ()
stUpdate f  = P (\s ts-> Success (ts, f s) ())

-- | Query the internal state.
stQuery    :: (s->a) -> Parser s t a
stQuery f   = P (\s ts-> Success (ts,s) (f s))

-- | Deliver the entire internal state.
stGet      :: Parser s t s
stGet       = P (\s ts-> Success (ts,s) s)


------------------------------------------------------------------------
-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: [t] -> Parser s t ()
reparse ts  = P (\s inp-> Success ((ts++inp),s) ())

------------------------------------------------------------------------
