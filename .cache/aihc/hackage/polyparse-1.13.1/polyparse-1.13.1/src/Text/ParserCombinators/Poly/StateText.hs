module Text.ParserCombinators.Poly.StateText
  ( -- * The Parser datatype
    Parser(P)
  , Result(..)
  , runParser
    -- ** Basic parsers
  , next
  , eof
  , satisfy
  , onFail
    -- ** Derived parsers (but implemented more efficiently)
  , manySatisfy
  , many1Satisfy
    -- ** State-handling
  , stUpdate    -- :: (s->s) -> Parser s t ()
  , stQuery     -- :: (s->a) -> Parser s t a
  , stGet       -- :: Parser s t s
    -- ** Re-parsing
  , reparse
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  , module Control.Applicative
  ) where


import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Control.Applicative
import qualified Control.Monad.Fail as Fail

-- | This @Parser@ datatype is a specialised parsing monad with error
--   reporting.  Whereas the standard version can be used for arbitrary
--   token types, this version is specialised to Text input only.
newtype Parser s a = P (s -> Text -> Result (Text,s) a)

-- | Apply a parser to an input token sequence.
runParser :: Parser s a -> s -> Text -> (Either String a, s, Text)
runParser (P p) = \s -> reTuple . resultToEither . p s
  where
    reTuple (either, (z,s)) = (either, s, z)

instance Functor (Parser s) where
    fmap f (P p) = P (\s-> fmap f . p s)

instance Monad (Parser s) where
    return       = pure
    (P f) >>= g  = P (\s-> continue . f s)
      where
        continue (Success (ts,s) x)         = let (P g') = g x in g' s ts
        continue (Committed r)              = Committed (continue r)
        continue (Failure ts e)             = Failure ts e

#if !MIN_VERSION_base(4,13,0)
    fail         = Fail.fail
#endif

instance Fail.MonadFail (Parser s) where
    fail e       = P (\s ts-> Failure (ts,s) e)

instance Commitment (Parser s) where
    commit (P p)         = P (\s-> Committed . squash . p s)
      where
        squash (Committed r) = squash r
        squash r             = r
    (P p) `adjustErr` f  = P (\s-> adjust . p s)
      where
        adjust (Failure z e) = Failure z (f e)
        adjust (Committed r) = Committed (adjust r)
        adjust  good         = good

    oneOf' = accum []
      where accum errs [] =
                fail ("failed to parse any of the possible choices:\n"
                            ++indent 2 (concatMap showErr (reverse errs)))
            accum errs ((e,P p):ps) =
                P (\s ts-> case p s ts of
                           Failure _ err ->
                                       let (P p') = accum ((e,err):errs) ps
                                       in p' s ts
                           r@(Success _ _)    -> r
                           r@(Committed _)    -> r )
            showErr (name,err) = name++":\n"++indent 2 err

instance Applicative (Parser s) where
    pure x    = P (\s ts-> Success (ts,s) x)
    pf <*> px = do { f <- pf; x <- px; return (f x) }
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative (Parser s) where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse (Parser s)

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next :: Parser s Char
next = P (\s bs-> case T.uncons bs of
                  Nothing       -> Failure (bs,s) "Ran out of input (EOF)"
                  Just (c, bs') -> Success (bs',s) c )

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof :: Parser s ()
eof = P (\s bs -> if T.null bs
                  then Success (bs,s) ()
                  else Failure (bs,s) "Expected end of input (EOF)" )

-- | Return the next token if it satisfies the given predicate.
satisfy :: (Char -> Bool) -> Parser s Char
satisfy f = do { x <- next
               ; if f x then return x else fail "Parse.satisfy: failed"
               }

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail :: Parser s a -> Parser s a -> Parser s a
(P p) `onFail` (P q) = P (\s ts-> continue s ts $ p s ts)
  where continue s ts (Failure _ _) = q s ts
    --  continue _ _  (Committed r) = r -- no, remain Committed
        continue _ _  r             = r

------------------------------------------------------------------------

-- | @manySatisfy p@ is a more efficient fused version of @many (satisfy p)@
manySatisfy :: (Char->Bool) -> Parser s Text
manySatisfy f = P (\s bs-> let (pre,suf) = T.span f bs in Success (suf,s) pre)

-- | @many1Satisfy p@ is a more efficient fused version of @many1 (satisfy p)@
many1Satisfy :: (Char->Bool) -> Parser s Text
many1Satisfy f = do x <- manySatisfy f
                    if T.null x then fail "Parse.many1Satisfy: failed"
                                else return x

------------------------------------------------------------------------
-- State handling

-- | Update the internal state.
stUpdate   :: (s->s) -> Parser s ()
stUpdate f  = P (\s bs-> Success (bs, f s) ())

-- | Query the internal state.
stQuery    :: (s->a) -> Parser s a
stQuery f   = P (\s bs-> Success (bs,s) (f s))

-- | Deliver the entire internal state.
stGet      :: Parser s s
stGet       = P (\s bs-> Success (bs,s) s)

------------------------------------------------------------------------

-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: Text -> Parser s ()
reparse ts  = P (\s inp-> Success (ts `T.append` inp,s) ())

------------------------------------------------------------------------
