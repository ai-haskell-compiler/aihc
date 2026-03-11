module Text.ParserCombinators.Poly.Base
  ( -- * The PolyParse classes
    Commitment(..)      -- class of all two-level-error values
  , PolyParse           -- class of all monadic two-level-error parsers

    -- * Combinators general to all parser types.
    -- ** Simple combinators
  , apply       -- :: PolyParse p => p (a->b) -> p a -> p b
  , discard     -- :: PolyParse p => p a      -> p b -> p a
    -- ** Error-handling
  , failBad     -- :: PolyParse p => String -> p a
  , adjustErrBad-- :: PolyParse p => p a -> (String->String) -> p a
  , indent      -- :: Int -> String -> String
    -- ** Choices
  , oneOf       -- :: PolyParse p => [p a] -> p a
    -- ** Sequences
  , exactly     -- :: PolyParse p => Int -> p a -> p [a]
  , upto        -- :: PolyParse p => Int -> p a -> p [a]
  , many1       -- :: PolyParse p => p a -> p [a]
  , sepBy       -- :: PolyParse p => p a -> p sep -> p [a]
  , sepBy1      -- :: PolyParse p => p a -> p sep -> p [a]
  , bracketSep  -- :: PolyParse p => p bra -> p sep -> p ket -> p a -> p [a]
  , bracket     -- :: PolyParse p => p bra -> p ket -> p a -> p a
  , manyFinally -- :: PolyParse p => p a -> p z -> p [a]
  , manyFinally'-- :: PolyParse p => p a -> p z -> p [a]
  ) where

import Control.Applicative
import qualified Control.Monad.Fail as Fail

#ifdef __NHC__
default (Integer,Double,[])     -- hack to avoid bizarre type defaulting error
instance Commitment []
instance PolyParse []
#endif

-- | The @Commitment@ class is an abstraction over all the current
--   concrete representations of monadic/applicative parser combinators in this
--   package.  The common feature is two-level error-handling.
--   Some primitives must be implemented specific to each parser type
--   (e.g. depending on whether the parser has a running state, or
--   whether it is lazy).  But given those primitives, large numbers of
--   combinators do not depend any further on the internal structure of
--   the particular parser.
class Commitment p where
    -- | Commit is a way of raising the severity of any errors found within
    --   its argument.  Used in the middle of a parser definition, it means that
    --   any operations prior to commitment fail softly, but after commitment,
    --   they fail hard.
    commit    :: p a -> p a
    -- | @p `adjustErr` f@ applies the transformation @f@ to any error message
    --   generated in @p@, having no effect if @p@ succeeds.
    adjustErr :: p a -> (String -> String) -> p a
    -- | Parse the first alternative that succeeds, but if none succeed,
    --   report only the severe errors, and if none of those, then report
    --   all the soft errors.
    oneOf'    :: [(String, p a)] -> p a

-- | The @PolyParse@ class is an abstraction gathering all of the common
--   features that a two-level error-handling parser requires:
--   the applicative parsing interface, the monadic interface, and commitment.
--
--   There are two additional basic combinators that we expect to be implemented
--   afresh for every concrete type, but which (for technical reasons)
--   cannot be class methods.  They are @next@ and @satisfy@.
class (Functor p, Monad p, Fail.MonadFail p, Applicative p, Alternative p, Commitment p) =>
      PolyParse p

infixl 3 `apply`
infixl 3 `discard`

-- | Apply a parsed function to a parsed value.
--   Rather like ordinary function application lifted into parsers.
apply  :: PolyParse p => p (a->b) -> p a -> p b
apply = (<*>)

-- | @x `discard` y@ parses both x and y, but discards the result of y.
--   Rather like @const@ lifted into parsers.
discard :: PolyParse p => p a -> p b -> p a
px `discard` py = do { x <- px; y <- py; y `seq` return x; }

{-
-- Combinators we expect most concrete parser types to implement.
--   For technical reasons, they cannot be class members.

-- | Yield the next token
next      :: PolyParse p => p t
  --  where t is constrained to be the input token type

-- | One token satisfying a predicate.
satisfy :: PolyParse p => (t->Bool) -> p t t
satisfy p = do{ x <- next
              ; if p x then return x else fail "Parse.satisfy: failed"
              }
  -- note: must be re-defined for each implementation because
  --       its type cannot be expressed otherwise.
-}

-- | When a simple fail is not strong enough, use failBad for emphasis.
--   An emphasised (severe) error cannot be overridden by choice
--   operators.
failBad :: PolyParse p => String -> p a
failBad e = commit (Fail.fail e)

-- | @adjustErrBad@ is just like @adjustErr@ except it also raises the
--   severity of the error.
adjustErrBad :: PolyParse p => p a -> (String->String) -> p a
p `adjustErrBad` f = commit (p `adjustErr` f)

-- | Parse the first alternative in the list that succeeds.
oneOf :: PolyParse p => [p a] -> p a
oneOf []     = Fail.fail ("failed to parse any of the possible choices")
oneOf (p:ps) = p <|> oneOf ps
--oneOf :: Show t => [Parser t a] -> Parser t a
--oneOf []     = do { n <- next
--                  ; fail ("failed to parse any of the possible choices"
--                         ++"\n  next token is "++show n)
--                  }
--oneOf (p:ps) = p `onFail` oneOf ps

-- | Helper for formatting error messages: indents all lines by a fixed amount.
indent :: Int -> String -> String
indent n = unlines . map (replicate n ' ' ++) . lines

-- | 'exactly n p' parses precisely n items, using the parser p, in sequence.
exactly :: PolyParse p => Int -> p a -> p [a]
exactly 0 p = return []
exactly n p = return (:) `apply`  (p `adjustErr` (("When expecting exactly "
                                                    ++show n++" more items")++))
                         `apply`  exactly (n-1) p

-- | 'upto n p' parses n or fewer items, using the parser p, in sequence.
upto :: PolyParse p => Int -> p a -> p [a]
upto 0 p = return []
upto n p = do x <- p; return (x:) `apply` upto (n-1) p
           <|> return []


{- is in Control.Applicative
-- | 'optional' indicates whether the parser succeeded through the Maybe type.
optional :: PolyParse p => p a -> p (Maybe a)
optional p = fmap Just p `onFail` return Nothing
-}
{- is in Control.Applicative
-- | 'many p' parses a list of elements with individual parser p.
--   Cannot fail, since an empty list is a valid return value.
many :: PolyParse p => p a -> p [a]
many p = many1 p `onFail` return []
-}

-- | Parse a non-empty list of items.
many1 :: PolyParse p => p a -> p [a]
many1 p = do { x <- p `adjustErr` (("In a sequence:\n"++). indent 2)
             ; return (x:) `apply` many p
             }
--       `adjustErr` ("When looking for a non-empty sequence:\n\t"++)

-- | Parse a list of items separated by discarded junk.
sepBy :: PolyParse p => p a -> p sep -> p [a]
sepBy p sep = do sepBy1 p sep <|> return []

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: PolyParse p => p a -> p sep -> p [a]
sepBy1 p sep = do { x <- p
                  ; return (x:) `apply` many (do {sep; p})
                  }
         `adjustErr` ("When looking for a non-empty sequence with separators:\n\t"++)

-- | Parse a list of items, discarding the start, end, and separator
--   items.
bracketSep :: PolyParse p => p bra -> p sep -> p ket -> p a -> p [a]
bracketSep open sep close p =
    do { open; close; return [] }
       <|>
    do { open    `adjustErr` ("Missing opening bracket:\n\t"++)
       ; x <- p  `adjustErr` ("After first bracket in a group:\n\t"++)
       ; return (x:)
           `apply` manyFinally (do {sep; p})
              (close `adjustErrBad` ("When looking for closing bracket:\n\t"++))
       }

-- | Parse a bracketed item, discarding the brackets.
--   If everything matches /except/ the closing bracket, the whole
--   parse fails soft, which can give less-than-satisfying error messages.
--   If you want better error messages, try calling with e.g.
--     @bracket open (commit close) item@
bracket :: PolyParse p => p bra -> p ket -> p a -> p a
bracket open close p = do
    do { open    `adjustErr` ("Missing opening bracket:\n\t"++)
       ; p `discard` (close `adjustErr` ("Missing closing bracket:\n\t"++))
       }

-- | @manyFinally e t@ parses a possibly-empty sequence of @e@'s,
--   terminated by a @t@.  The final @t@ is discarded.  Any parse failures
--   could be due either to a badly-formed terminator or a badly-formed
--   element, so it raises both possible errors.
manyFinally :: PolyParse p => p a -> p z -> p [a]
{-
-- This implementation is incorrect.  If at least one item has been
-- parsed, but the terminator is missing, then this erroneously succeeds
-- returning the empty list.
manyFinally p t =
    (many p `discard` t)
      <|>
    oneOf' [ ("sequence terminator", do { t; return [] } )
           , ("item in a sequence",  do { p; return [] } )
           ]
-}

manyFinally p t =
    do { xs <- many p
       ; oneOf' [ ("sequence terminator", do { t; return () } )
                , ("item in a sequence",  do { p; return () } )
                ]
       ; return xs
       }

-- | @manyFinally'@ is like @manyFinally@, except when the terminator
--   parser overlaps with the element parser.  In @manyFinally e t@,
--   the parser @t@ is tried only when parser @e@ fails, whereas in
--   @manyFinally' e t@, the parser @t@ is always tried first, then
--   parser @e@ only if the terminator is not found.  For instance,
--   @manyFinally (accept "01") (accept "0")@ on input @"0101010"@ returns
--   @["01","01","01"]@, whereas @manyFinally'@ with the same arguments
--   and input returns @[]@.
manyFinally' :: (PolyParse p, Show a) => p a -> p z -> p [a]
manyFinally' p t = fmap reverse $ go []
  where
    go acc = ( do t; return acc )
             <|>
             ( do { x <- p
                         <|>
                         oneOf' [ ( "terminator in a manyFinally' sequence"
                                  , do { t; return undefined }
                                  )
                                , ( "item in a manyFinally' sequence", p)
                                ]
                         `adjustErr` (("After successful partial sequence "
                                      ++show (reverse acc)++",\n")++)
                  ; go (x: acc)
                  }
             )


------------------------------------------------------------------------
