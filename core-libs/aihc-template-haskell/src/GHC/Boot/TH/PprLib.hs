-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

-- | Monadic front-end to Text.PrettyPrint
module GHC.Boot.TH.PprLib
  ( -- * The document type
    Doc, -- Abstract, instance of Show
    PprM,

    -- * Primitive Documents
    empty,
    semi,
    comma,
    colon,
    dcolon,
    space,
    equals,
    arrow,
    lparen,
    rparen,
    lbrack,
    rbrack,
    lbrace,
    rbrace,

    -- * Converting values into documents
    text,
    char,
    ptext,
    int,
    integer,
    float,
    double,
    rational,

    -- * Wrapping documents in delimiters
    parens,
    brackets,
    braces,
    quotes,
    doubleQuotes,

    -- * Combining documents
    (<>),
    (<+>),
    hcat,
    hsep,
    ($$),
    ($+$),
    vcat,
    sep,
    cat,
    fsep,
    fcat,
    nest,
    hang,
    punctuate,

    -- * Predicates on documents
    isEmpty,
    to_HPJ_Doc,
    pprName,
    pprName',
  )
where

import Control.Monad (ap, liftM, liftM2)
import GHC.Boot.TH.Lib.Map (Map)
import GHC.Boot.TH.Lib.Map qualified as Map (empty, insert, lookup)
import GHC.Boot.TH.Syntax
  ( Name (..),
    NameFlavour (..),
    NameIs (..),
    Uniq,
    showName',
  )
import Text.PrettyPrint qualified as HPJ
import Prelude hiding ((<>))

infixl 6 <>

infixl 6 <+>

infixl 5 $$, $+$

-- ---------------------------------------------------------------------------
-- The interface

-- The primitive Doc values

instance Show Doc where
  show d = HPJ.render (to_HPJ_Doc d)

isEmpty ::
  Doc ->
  -- | Returns 'True' if the document is empty
  PprM Bool
empty ::
  -- | An empty document
  Doc
semi ::
  -- | A ';' character
  Doc
comma ::
  -- | A ',' character
  Doc
colon ::
  -- | A ':' character
  Doc
dcolon ::
  -- | A "::" string
  Doc
space ::
  -- | A space character
  Doc
equals ::
  -- | A '=' character
  Doc
arrow ::
  -- | A "->" string
  Doc
lparen ::
  -- | A '(' character
  Doc
rparen ::
  -- | A ')' character
  Doc
lbrack ::
  -- | A '[' character
  Doc
rbrack ::
  -- | A ']' character
  Doc
lbrace ::
  -- | A '{' character
  Doc
rbrace ::
  -- | A '}' character
  Doc
text :: String -> Doc
ptext :: String -> Doc
char :: Char -> Doc
int :: Int -> Doc
integer :: Integer -> Doc
float :: Float -> Doc
double :: Double -> Doc
rational :: Rational -> Doc
parens ::
  Doc ->
  -- | Wrap document in @(...)@
  Doc
brackets ::
  Doc ->
  -- | Wrap document in @[...]@
  Doc
braces ::
  Doc ->
  -- | Wrap document in @{...}@
  Doc
quotes ::
  Doc ->
  -- | Wrap document in @\'...\'@
  Doc
doubleQuotes ::
  Doc ->
  -- | Wrap document in @\"...\"@
  Doc
-- Combining @Doc@ values

(<>) ::
  Doc ->
  Doc ->
  -- | Beside
  Doc
hcat ::
  [Doc] ->
  -- | List version of '<>'
  Doc
(<+>) ::
  Doc ->
  Doc ->
  -- | Beside, separated by space
  Doc
hsep ::
  [Doc] ->
  -- | List version of '<+>'
  Doc
($$) ::
  Doc ->
  Doc ->
  -- | Above; if there is no
  --  overlap it \"dovetails\" the two
  Doc
($+$) ::
  Doc ->
  Doc ->
  -- | Above, without dovetailing.
  Doc
vcat ::
  [Doc] ->
  -- | List version of '$$'
  Doc
cat ::
  [Doc] ->
  -- | Either hcat or vcat
  Doc
sep ::
  [Doc] ->
  -- | Either hsep or vcat
  Doc
fcat ::
  [Doc] ->
  -- | \"Paragraph fill\" version of cat
  Doc
fsep ::
  [Doc] ->
  -- | \"Paragraph fill\" version of sep
  Doc
nest ::
  Int ->
  Doc ->
  -- | Nested
  Doc
-- GHC-specific ones.

hang ::
  Doc ->
  Int ->
  Doc ->
  -- | @hang d1 n d2 = sep [d1, nest n d2]@
  Doc
punctuate ::
  Doc ->
  [Doc] ->
  -- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
  [Doc]

-- ---------------------------------------------------------------------------
-- The "implementation"

type State = (Map Name Name, Uniq)

data PprM a = PprM {runPprM :: State -> (a, State)}

pprName :: Name -> Doc
pprName = pprName' Alone

pprName' :: NameIs -> Name -> Doc
pprName' ni n@(Name o (NameU _)) =
  PprM $ \s@(fm, i) ->
    let (n', s') = case Map.lookup n fm of
          Just d -> (d, s)
          Nothing ->
            let n'' = Name o (NameU i)
             in (n'', (Map.insert n n'' fm, i + 1))
     in (HPJ.text $ showName' ni n', s')
pprName' ni n = text $ showName' ni n

{-
instance Show Name where
  show (Name occ (NameU u))    = occString occ ++ "_" ++ show (I# u)
  show (Name occ NameS)        = occString occ
  show (Name occ (NameG ns m)) = modString m ++ "." ++ occString occ

data Name = Name OccName NameFlavour

data NameFlavour
  | NameU Int#                  -- A unique local name
-}

to_HPJ_Doc :: Doc -> HPJ.Doc
to_HPJ_Doc d = fst $ runPprM d (Map.empty, 0)

instance Functor PprM where
  fmap = liftM

instance Applicative PprM where
  pure x = PprM $ \s -> (x, s)
  (<*>) = ap

instance Monad PprM where
  m >>= k = PprM $ \s ->
    let (x, s') = runPprM m s
     in runPprM (k x) s'

type Doc = PprM HPJ.Doc

-- The primitive Doc values

isEmpty = liftM HPJ.isEmpty

empty = return HPJ.empty

semi = return HPJ.semi

comma = return HPJ.comma

colon = return HPJ.colon

dcolon = return $ HPJ.text "::"

space = return HPJ.space

equals = return HPJ.equals

arrow = return $ HPJ.text "->"

lparen = return HPJ.lparen

rparen = return HPJ.rparen

lbrack = return HPJ.lbrack

rbrack = return HPJ.rbrack

lbrace = return HPJ.lbrace

rbrace = return HPJ.rbrace

text = return . HPJ.text

ptext = return . HPJ.ptext

char = return . HPJ.char

int = return . HPJ.int

integer = return . HPJ.integer

float = return . HPJ.float

double = return . HPJ.double

rational = return . HPJ.rational

parens = liftM HPJ.parens

brackets = liftM HPJ.brackets

braces = liftM HPJ.braces

quotes = liftM HPJ.quotes

doubleQuotes = liftM HPJ.doubleQuotes

-- Combining @Doc@ values

(<>) = liftM2 (HPJ.<>)

hcat = liftM HPJ.hcat . sequence

(<+>) = liftM2 (HPJ.<+>)

hsep = liftM HPJ.hsep . sequence

($$) = liftM2 (HPJ.$$)

($+$) = liftM2 (HPJ.$+$)

vcat = liftM HPJ.vcat . sequence

cat = liftM HPJ.cat . sequence

sep = liftM HPJ.sep . sequence

fcat = liftM HPJ.fcat . sequence

fsep = liftM HPJ.fsep . sequence

nest n = liftM (HPJ.nest n)

hang d1 n d2 = do
  d1' <- d1
  d2' <- d2
  return (HPJ.hang d1' n d2')

-- punctuate uses the same definition as Text.PrettyPrint
punctuate _ [] = []
punctuate p (d : ds) = go d ds
  where
    go d' [] = [d']
    go d' (e : es) = (d' <> p) : go e es
