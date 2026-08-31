-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE Safe #-}

module Text.PrettyPrint
  ( Doc,
    render,
    empty,
    semi,
    comma,
    colon,
    space,
    equals,
    lparen,
    rparen,
    lbrack,
    rbrack,
    lbrace,
    rbrace,
    text,
    ptext,
    char,
    int,
    integer,
    float,
    double,
    rational,
    parens,
    brackets,
    braces,
    quotes,
    doubleQuotes,
    (<>),
    (<+>),
    ($$),
    ($+$),
    hcat,
    hsep,
    vcat,
    cat,
    sep,
    fcat,
    fsep,
    nest,
    hang,
    punctuate,
    isEmpty,
  )
where

import Prelude hiding ((<>))

newtype Doc = Doc String

render :: Doc -> String
render (Doc value) = value

empty, semi, comma, colon, space, equals :: Doc
empty = Doc ""
semi = Doc ";"
comma = Doc ","
colon = Doc ":"
space = Doc " "
equals = Doc "="

lparen, rparen, lbrack, rbrack, lbrace, rbrace :: Doc
lparen = Doc "("
rparen = Doc ")"
lbrack = Doc "["
rbrack = Doc "]"
lbrace = Doc "{"
rbrace = Doc "}"

text, ptext :: String -> Doc
text = Doc
ptext = Doc

char :: Char -> Doc
char value = Doc [value]

int :: Int -> Doc
int = text . show

integer :: Integer -> Doc
integer = text . show

float :: Float -> Doc
float _ = error "Text.PrettyPrint.float is not supported"

double :: Double -> Doc
double _ = error "Text.PrettyPrint.double is not supported"

rational :: Rational -> Doc
rational = text . show

parens, brackets, braces, quotes, doubleQuotes :: Doc -> Doc
parens = enclose lparen rparen
brackets = enclose lbrack rbrack
braces = enclose lbrace rbrace
quotes = enclose (Doc "'") (Doc "'")
doubleQuotes = enclose (Doc "\"") (Doc "\"")

enclose :: Doc -> Doc -> Doc -> Doc
enclose left right value = left <> value <> right

infixl 6 <>, <+>

infixl 5 $$, $+$

(<>) :: Doc -> Doc -> Doc
Doc left <> Doc right = Doc (left ++ right)

(<+>) :: Doc -> Doc -> Doc
left <+> right = appendWith space left right

($$), ($+$) :: Doc -> Doc -> Doc
left $$ right = appendWith (Doc "\n") left right
($+$) = ($$)

appendWith :: Doc -> Doc -> Doc -> Doc
appendWith separator left right =
  if isEmpty left then right else appendToNonEmpty separator left right

appendToNonEmpty :: Doc -> Doc -> Doc -> Doc
appendToNonEmpty separator left right =
  if isEmpty right then left else left <> separator <> right

hcat, hsep, vcat, cat, sep, fcat, fsep :: [Doc] -> Doc
hcat = foldr (<>) empty
hsep = foldr (<+>) empty
vcat = foldr ($$) empty
cat = hcat
sep = hsep
fcat = hcat
fsep = hsep

nest :: Int -> Doc -> Doc
nest amount (Doc value) = Doc (indentAfterNewline amount value)

indentAfterNewline :: Int -> String -> String
indentAfterNewline amount = go
  where
    padding = replicate amount ' '
    go [] = []
    go ('\n' : rest) = '\n' : padding ++ go rest
    go (character : rest) = character : go rest

hang :: Doc -> Int -> Doc -> Doc
hang first amount second = first $$ nest amount second

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [value] = [value]
punctuate separator (value : rest) = (value <> separator) : punctuate separator rest

isEmpty :: Doc -> Bool
isEmpty (Doc value) = null value
