{-# LANGUAGE CPP, OverloadedStrings, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.String.Combinators
-- Copyright   :  (c) 2009-2011 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
--------------------------------------------------------------------------------

module Data.String.Combinators
    ( -- * Combining
      (<>)
    , mid
    , (<+>)
    , ($$)
    , intercalate
    , hcat
    , unwords
    , unlines
    , punctuate

     -- * Wrapping in delimiters
    , between

    , parens
    , thenParens
    , brackets
    , braces
    , angleBrackets
    , quotes
    , doubleQuotes

      -- * From characters
    , char

    , semi
    , colon
    , comma
    , space
    , newline
    , equals
    , lparen
    , rparen
    , lbrack
    , rbrack
    , lbrace
    , rbrace
    , labrack
    , rabrack

      -- * From showable values
    , fromShow

    , int
    , integer
    , float
    , double
    , rational
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.List     ( foldr )
import Data.Bool     ( Bool(False, True) )
import Data.Char     ( Char )
import Data.Function ( id, (.) )
import Data.Int      ( Int )
import Data.Ratio    ( Rational )
import Data.String   ( IsString, fromString )
import Data.Monoid   ( Monoid, mempty )
import Text.Show     ( Show, show )
import Prelude       ( Integer, Float, Double )

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ( (<>) )
#else
import Data.Monoid ( mappend )
-- | Put two string-likes besides eachother.
--
-- Note that: @'<>' = 'mappend'@.
(<>) :: Monoid s => s -> s -> s
(<>) = mappend
infixl 6 <>
#endif

--------------------------------------------------------------------------------
-- * Combining
--------------------------------------------------------------------------------

-- | @mid m x y@ Puts @x@ and @y@ around @m@.
--
-- Note that: @mid m x y = 'between' x y m@.
mid :: Monoid s => s -> (s -> s -> s)
mid m x y = between x y m

-- | Put two string-likes besides eachother separated by a 'space'.
(<+>) :: (Monoid s, IsString s) => s -> s -> s
(<+>) = mid space

-- | Put two string-likes above eachother (separated by a 'newline').
($$) :: (Monoid s, IsString s) => s -> s -> s
($$) = mid newline

infixl 6 <+>
infixl 5 $$

{-| Combine the string-likes with a given function.

@intercalate f [s1, ... sn] = s1 \`f\` (s2 \`f\` (... (sn-1 \`f\` sn)))@
-}
intercalate :: Monoid s => (s -> s -> s) -> [s] -> s
intercalate f = go
    where
      go []     = mempty
      go (s:[]) = s
      go (s:ss) = s `f` go ss

-- | List version of '<>'.
--
-- Note that: @hcat = 'intercalate' ('<>')@.
hcat :: Monoid s => [s] -> s
hcat = intercalate (<>)

-- | List version of '<+>'.
--
-- Note that: @unwords = 'intercalate' ('<+>')@.
unwords :: (Monoid s, IsString s) => [s] -> s
unwords = intercalate (<+>)

-- | List version of '$$'.
--
-- Note that: @unlines = foldr ('$$') mempty@
unlines :: (Monoid s, IsString s) =>  [s] -> s
unlines = foldr ($$) mempty

-- | @punctuate p [s1, ... sn] = [s1 '<>' p, s2 '<>' p, ... sn-1 '<>' p, sn]@.
--
-- (Idea and implementation taken from the @pretty@ package.)
punctuate :: (Monoid s) => s -> [s] -> [s]
punctuate _ []     = []
punctuate p (d:ds) = go d ds
    where
      go d' []     = [d']
      go d' (e:es) = (d' <> p) : go e es


--------------------------------------------------------------------------------
-- * Wrapping in delimiters
--------------------------------------------------------------------------------

-- | @between b c s@ wraps the string-like @s@ between @b@ and @c@.
between :: (Monoid s) => s -> s -> (s -> s)
between open close = \x -> open <> x <> close


-- | Wrap a string-like in @(...)@.
parens :: (Monoid s, IsString s) => s -> s
parens = between "(" ")"

-- | Wrap a string-like in @[...]@.
brackets :: (Monoid s, IsString s) => s -> s
brackets = between "[" "]"

-- | Wrap a string-like in @{...}@.
braces :: (Monoid s, IsString s) => s -> s
braces = between "{" "}"

-- | Wrap a string-like in @\<...\>@.
angleBrackets :: (Monoid s, IsString s) => s -> s
angleBrackets = between "<" ">"

-- | Wrap a string-like in @\'...\'@.
quotes :: (Monoid s, IsString s) => s -> s
quotes = between "'" "'"

-- | Wrap a string-like in @\"...\"@.
doubleQuotes :: (Monoid s, IsString s) => s -> s
doubleQuotes = between "\"" "\""


{-| Like @showParen@ conditionally wraps a string-like in @(...)@

This function is supposed to be used infix as in:

@(precedence >= 10) \`thenParens\` (\"fun\" \<+\> \"arg\")@
-}
thenParens :: (Monoid s, IsString s) => Bool -> s -> s
thenParens True  = parens
thenParens False = id


--------------------------------------------------------------------------------
-- * From characters
--------------------------------------------------------------------------------

-- | Convert a character to a string-like.
char :: IsString s => Char -> s
char c = fromString [c]


-- | A ';' character.
semi :: IsString s => s
semi = char ';'

-- | A ':' character.
colon :: IsString s => s
colon = char ':'

-- | A ',' character.
comma :: IsString s => s
comma = char ','

-- | A ' ' character.
space :: IsString s => s
space = char ' '

-- | A '\n' character.
newline :: IsString s => s
newline = char '\n'

-- | A '=' character.
equals :: IsString s => s
equals = char '='

-- | A '(' character.
lparen :: IsString s => s
lparen = char '('

-- | A ')' character.
rparen :: IsString s => s
rparen = char ')'

-- | A '[' character.
lbrack :: IsString s => s
lbrack = char '['

-- | A ']' character.
rbrack :: IsString s => s
rbrack = char ']'

-- | A '{' character.
lbrace :: IsString s => s
lbrace = char '{'

-- | A '}' character.
rbrace :: IsString s => s
rbrace = char '}'

-- | A \'<\' character.
labrack :: IsString s => s
labrack = char '<'

-- | A \'>\' character.
rabrack :: IsString s => s
rabrack = char '>'


--------------------------------------------------------------------------------
-- * From showable values
--------------------------------------------------------------------------------

-- | Convert a @Show@able value to a string-like.
fromShow :: (Show a, IsString s) => a -> s
fromShow = fromString . show

-- | Convert an @Int@ to a string-like.
int :: IsString s => Int -> s
int = fromShow

-- | Convert an @Integer@ to a string-like.
integer :: IsString s => Integer -> s
integer = fromShow

-- | Convert a @Float@ to a string-like.
float :: IsString s => Float -> s
float = fromShow

-- | Convert a @Double@ to a string-like.
double :: IsString s => Double -> s
double = fromShow

-- | Convert a @Rational@ to a string-like.
rational :: IsString s => Rational -> s
rational = fromShow
