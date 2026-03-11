module Text.ParserCombinators.Poly.ByteString
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
    -- ** Re-parsing
  , reparse
    -- * Re-export all more general combinators
  , module Text.ParserCombinators.Poly.Base
  ) where


import Text.ParserCombinators.Poly.Base
import Text.ParserCombinators.Poly.Result
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import Control.Applicative
import qualified Control.Monad.Fail as Fail
import Data.Word

-- | This @Parser@ datatype is a specialised parsing monad with error
--   reporting.  Whereas the standard version can be used for arbitrary
--   token types, this version is specialised to ByteString input only.
newtype Parser a = P (ByteString -> Result ByteString a)

-- | Apply a parser to an input token sequence.
runParser :: Parser a -> ByteString -> (Either String a, ByteString)
runParser (P p) = resultToEither . p

instance Functor Parser where
    fmap f (P p) = P (fmap f . p)

instance Monad Parser where
    return       = pure
    (P f) >>= g  = P (continue . f)
      where
        continue (Success ts x)             = let (P g') = g x in g' ts
        continue (Committed r)              = Committed (continue r)
        continue (Failure ts e)             = Failure ts e

#if !MIN_VERSION_base(4,13,0)
    fail         = Fail.fail
#endif

instance Fail.MonadFail Parser where
    fail e       = P (\ts-> Failure ts e)

instance Commitment Parser where
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
                                       let (P p') = accum ((e,err):errs) ps
                                       in p' ts
                           r@(Success _ _)    -> r
                           r@(Committed _)    -> r )
            showErr (name,err) = name++":\n"++indent 2 err

instance Applicative Parser where
    pure x    = P (\ts-> Success ts x)
    pf <*> px = do { f <- pf; x <- px; return (f x) }
#if defined(GLASGOW_HASKELL) && GLASGOW_HASKELL > 610
    p  <*  q  = p `discard` q
#endif

instance Alternative Parser where
    empty     = fail "no parse"
    p <|> q   = p `onFail` q

instance PolyParse Parser

------------------------------------------------------------------------

-- | Simply return the next token in the input tokenstream.
next :: Parser Word8
next = P (\bs-> case BS.uncons bs of
                Nothing     -> Failure bs "Ran out of input (EOF)"
                Just (h, t) -> Success t h )

-- | Succeed if the end of file/input has been reached, fail otherwise.
eof :: Parser ()
eof = P (\bs -> if BS.null bs
                then Success bs ()
                else Failure bs "Expected end of input (EOF)" )

-- | Return the next token if it satisfies the given predicate.
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f = do { x <- next
               ; if f x then return x else fail "Parse.satisfy: failed"
               }

-- | @p `onFail` q@ means parse p, unless p fails, in which case
--   parse q instead.
--   Can be chained together to give multiple attempts to parse something.
--   (Note that q could itself be a failing parser, e.g. to change the error
--   message from that defined in p to something different.)
--   However, a severe failure in p cannot be ignored.
onFail :: Parser a -> Parser a -> Parser a
(P p) `onFail` (P q) = P (\ts-> continue ts $ p ts)
  where continue ts (Failure _ _) = q ts
    --  continue _  (Committed r) = r   -- no, remain Committed
        continue _  r             = r

------------------------------------------------------------------------

-- | @manySatisfy p@ is a more efficient fused version of @many (satisfy p)@
manySatisfy :: (Word8->Bool) -> Parser ByteString
manySatisfy f = P (\bs-> let (pre,suf) = BS.span f bs in Success suf pre)

-- | @many1Satisfy p@ is a more efficient fused version of @many1 (satisfy p)@
many1Satisfy :: (Word8->Bool) -> Parser ByteString
many1Satisfy f = do x <- manySatisfy f
                    if BS.null x then fail "Parse.many1Satisfy: failed"
                                 else return x

------------------------------------------------------------------------

-- | Push some tokens back onto the front of the input stream and reparse.
--   This is useful e.g. for recursively expanding macros.  When the
--   user-parser recognises a macro use, it can lookup the macro
--   expansion from the parse state, lex it, and then stuff the
--   lexed expansion back down into the parser.
reparse    :: ByteString -> Parser ()
reparse ts  = P (\inp-> Success (ts `BS.append` inp) ())

------------------------------------------------------------------------
