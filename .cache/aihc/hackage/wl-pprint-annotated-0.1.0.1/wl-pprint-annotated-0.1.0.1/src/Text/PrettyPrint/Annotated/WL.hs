-----------------------------------------------------------------------------
-- |
-- Module      :  Text.PrettyPrint.Annotated.WL
-- Copyright   :  Google, Inc. (c) 2013,
--                Edward Kmett (c) 2011,
--                Daan Leijen  (c) 2000
--
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty print module based on Daan Leijen's implementation of Philip Wadler's
-- \"prettier printer\"
--
-- @
--      \"A prettier printer\"
--      Draft paper, April 1997, revised March 1998.
--      <http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf>
-- @
--
-- This is an implementation of the pretty printing combinators
-- described by Philip Wadler (1997). In their bare essence, the
-- combinators of Wadler are not expressive enough to describe some
-- commonly occurring layouts. The PPrint library adds new primitives
-- to describe these layouts and works well in practice.
--
-- The library is based on a single way to concatenate documents,
-- which is associative and has both a left and right unit.  This
-- simple design leads to an efficient and short implementation. The
-- simplicity is reflected in the predictable behaviour of the
-- combinators which make them easy to use in practice.
--
-- A thorough description of the primitive combinators and their
-- implementation can be found in Philip Wadler's paper
-- (1997). Additions and the main differences with his original paper
-- are:
--
-- * The nil document is called 'mempty'. We cannot use 'empty'
-- for compatibility with base.
--
-- * The operator '</>' is used
-- for soft line breaks.
--
-- * There are three new primitives: 'align', 'fill' and
-- 'fillBreak'. These are very useful in practice.
--
-- * Lots of other useful combinators, like 'fillSep' and 'list'.
--
-- * There are three renderers, 'renderPretty/renderPrettyDefault' and 'renderSmart'
-- for pretty printing and 'renderCompact' for compact output. The pretty printing algorithm
-- also uses a ribbon-width now for even prettier output.
--
-- * There are display routines 'displayS' and 'display' for strings,
-- 'displayT' for lazy text, 'displayIO' for file based output.
-- Generalized display routines for display with annotations
-- are provided, i.e., 'displayDecoratedA' and 'displayDecorated'.
-- Furthermore 'displaySpans' exists which creates a monoid and a SpanList
-- of the annotations.
--
-- * There is a 'Pretty' class which creates documents without annotations.
--
-- * The implementation uses optimised representations and strictness
-- annotations.
--
-- * There is the wl-pprint-console package, based on this package,
-- which provides additional display routines, e.g., colorful output using
-- ANSI escape sequences.
--
-----------------------------------------------------------
module Text.PrettyPrint.Annotated.WL (
  -- * Documents
  Doc(..), putDoc, hPutDoc

  -- * Basic combinators
  , char, text, nest, line, linebreak, group, softline
  , softbreak, hardline, flatAlt, flatten

  -- * Annotations
  , annotate, noAnnotate, docMapAnn
  , simpleDocMapAnn, simpleDocScanAnn

  -- * Alignment
  --
  -- The combinators in this section can not be described by Wadler's
  -- original combinators. They align their output relative to the
  -- current output position - in contrast to @nest@ which always
  -- aligns to the current nesting level. This deprives these
  -- combinators from being \`optimal\'. In practice however they
  -- prove to be very useful. The combinators in this section should
  -- be used with care, since they are more expensive than the other
  -- combinators. For example, @align@ shouldn't be used to pretty
  -- print all top-level declarations of a language, but using @hang@
  -- for let expressions is fine.
  , align, hang, indent, encloseSep, list, tupled, semiBraces

  -- * Operators
  , (<+>), (</>), (<//>), (<#>), (<##>)

  -- * List combinators
  , hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate

  -- * Fillers
  , fill, fillBreak

  -- * Bracketing combinators
  , enclose, squotes, dquotes, parens, angles, braces, brackets

  -- * Character documents
  , lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket
  , squote, dquote, semi, colon, comma, space, dot, backslash, equals

  -- * Pretty class
  , Pretty(..)

  -- * Rendering
  , SimpleDoc(..), renderPrettyDefault, renderPretty, renderCompact, renderSmart
  , display, displayS, displayT, displayIO, displayDecoratedA, displayDecorated
  , SpanList, displaySpans

  -- * Undocumented

  , column, nesting, width, columns, ribbon

  -- * Re-exported standard functions
  , mempty, (<>)
  ) where

import Data.Foldable hiding (fold)
import Data.Traversable
import Data.Int
import Data.Word
import Data.Bifunctor
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Control.Applicative
import Data.Sequence (Seq)
import Data.Semigroup
import System.IO (Handle,hPutStr,stdout)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.String (IsString(..))

infixr 5 </>, <//>, <#>, <##>
infixr 6 <+>

-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------

-- | The document @(list xs)@ comma separates the documents @xs@ and
-- encloses them in square brackets. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
list :: Foldable f => f (Doc a) -> Doc a
list = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
tupled :: Foldable f => f (Doc a) -> Doc a
tupled = encloseSep lparen rparen comma

(<+>) :: Doc a -> Doc a -> Doc a
x <+> y = x <> space <> y

-- | The document @(semiBraces xs)@ separates the documents @xs@ with
-- semi colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: Foldable f => f (Doc a) -> Doc a
semiBraces = encloseSep lbrace rbrace semi

-- | The document @(encloseSep l r sep xs)@ concatenates the documents
-- @xs@ separated by @sep@ and encloses the resulting document by @l@
-- and @r@. The documents are rendered horizontally if that fits the
-- page. Otherwise they are aligned vertically. All separators are put
-- in front of the elements. For example, the combinator 'list' can be
-- defined with @encloseSep@:
--
-- > list xs = encloseSep lbracket rbracket comma xs
-- > test    = text "list" <+> (list (map int [10,200,3000]))
--
-- Which is layed out with a page width of 20 as:
--
-- @
-- list [10, 200, 3000]
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- list [ 10
--      , 200
--      , 3000 ]
-- @
encloseSep :: Foldable f => Doc a -> Doc a -> Doc a -> f (Doc a) -> Doc a
encloseSep left right sp ds0
    = case toList ds0 of
        []  -> left <> right
        [d] -> left <> d <> right
        ds  -> group $ align $ left'
                 <> vcat (zipWith (<>) (mempty : repeat (sp <> space)) ds)
                 <> right'
          where left'  = left <> flatAlt space mempty
                right' = flatAlt space mempty <> right


-----------------------------------------------------------
-- punctuate p [d1,d2,...,dn] => [d1 <> p,d2 <> p, ... ,dn]
-----------------------------------------------------------


-- | @(punctuate p xs)@ concatenates all documents in @xs@ with
-- document @p@ except for the last document.
--
-- > someText = map text ["words","in","a","tuple"]
-- > test     = parens (align (cat (punctuate comma someText)))
--
-- This is layed out on a page width of 20 as:
--
-- @
-- (words,in,a,tuple)
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- (words,
--  in,
--  a,
--  tuple)
-- @
--
-- (If you want put the commas in front of their elements instead of
-- at the end, you should use 'tupled' or, in general, 'encloseSep'.)
punctuate :: Traversable f => Doc a -> f (Doc a) -> f (Doc a)
punctuate p xs = snd $ mapAccumL (\(d:ds) _ -> (ds, if null ds then d else d <> p)) (toList xs) xs

-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically with
-- @(\<\#\>)@.
--
-- > sep xs  = group (vsep xs)
sep :: Foldable f => f (Doc a) -> Doc a
sep = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
-- horizontally with @(\<+\>)@ as long as its fits the page, then
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
--
-- > fillSep xs  = foldr (</>) mempty xs
fillSep :: Foldable f => f (Doc a) -> Doc a
fillSep = fold (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
-- horizontally with @(\<+\>)@.
hsep :: Foldable f => f (Doc a) -> Doc a
hsep = fold (<+>)

-- | The document @(vsep xs)@ concatenates all documents @xs@
-- vertically with @(\<\#\>)@. If a 'group' undoes the line breaks
-- inserted by @vsep@, all documents are separated with a space.
--
-- > someText = map text (words ("text to lay out"))
-- >
-- > test     = text "some" <+> vsep someText
--
-- This is layed out as:
--
-- @
-- some text
-- to
-- lay
-- out
-- @
--
-- The 'align' combinator can be used to align the documents under
-- their first element
--
-- > test = text "some" <+> align (vsep someText)
--
-- Which is printed as:
--
-- @
-- some text
--      to
--      lay
--      out
-- @
vsep :: Foldable f => f (Doc a) -> Doc a
vsep = fold (<#>)

-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @(\<\#\#\>)@.
--
-- > cat xs  = group (vcat xs)
cat :: Foldable f => f (Doc a) -> Doc a
cat = group . vcat

-- | The document @(fillCat xs)@ concatenates documents @xs@
-- horizontally with @(\<\>)@ as long as its fits the page, then inserts
-- a @linebreak@ and continues doing that for all documents in @xs@.
--
-- > fillCat xs  = foldr (<//>) mempty xs
fillCat :: Foldable f => f (Doc a) -> Doc a
fillCat = fold (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: Foldable f => f (Doc a) -> Doc a
hcat = fold (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @(\<\#\#\>)@. If a 'group' undoes the line breaks
-- inserted by @vcat@, all documents are directly concatenated.
vcat :: Foldable f => f (Doc a) -> Doc a
vcat = fold (<##>)

fold :: Foldable f => (Doc a -> Doc a -> Doc a) -> f (Doc a) -> Doc a
fold f xs | null xs = mempty
          | otherwise = foldr1 f xs

instance Semigroup (Doc a) where
  -- | The document @(x \<\> y)@ concatenates document @x@ and document
  -- @y@. It is an associative operation having 'mempty' as a left and
  -- right unit.  (infixl 6)
  (<>) = Cat

instance Monoid (Doc a) where
  mappend = Cat
  mempty = Empty
  mconcat = hcat

-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@ with a
-- 'softline' in between. This effectively puts @x@ and @y@ either
-- next to each other (with a @space@ in between) or underneath each
-- other. (infixr 5)
(</>) :: Doc a -> Doc a -> Doc a
x </> y = x <> softline <> y

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@ with
-- a 'softbreak' in between. This effectively puts @x@ and @y@ either
-- right next to each other or underneath each other. (infixr 5)
(<//>) :: Doc a -> Doc a -> Doc a
x <//> y = x <> softbreak <> y

-- | The document @(x \<\#\> y)@ concatenates document @x@ and @y@ with a
-- 'line' in between. (infixr 5)
(<#>) :: Doc a -> Doc a -> Doc a
x <#> y = x <> line <> y

-- | The document @(x \<\#\#\> y)@ concatenates document @x@ and @y@ with
-- a @linebreak@ in between. (infixr 5)
(<##>) :: Doc a -> Doc a -> Doc a
x <##> y = x <> linebreak <> y

-- | The document @softline@ behaves like 'space' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softline = group line
softline :: Doc a
softline = group line

-- | The document @softbreak@ behaves like 'mempty' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softbreak  = group linebreak
softbreak :: Doc a
softbreak = group linebreak

-- | Document @(squotes x)@ encloses document @x@ with single quotes
-- \"'\".
squotes :: Doc a -> Doc a
squotes = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
-- '\"'.
dquotes :: Doc a -> Doc a
dquotes = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
-- \"}\".
braces :: Doc a -> Doc a
braces = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
-- and \")\".
parens :: Doc a -> Doc a
parens = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
-- \"\>\".
angles :: Doc a -> Doc a
angles = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
-- \"[\" and \"]\".
brackets :: Doc a -> Doc a
brackets = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
-- documents @l@ and @r@ using @(\<\>)@.
--
-- > enclose l r x   = l <> x <> r
enclose :: Doc a -> Doc a -> Doc a -> Doc a
enclose l r x = l <> x <> r

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc a
lparen = char '('

-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc a
rparen = char ')'

-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc a
langle = char '<'

-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc a
rangle = char '>'

-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc a
lbrace = char '{'

-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc a
rbrace = char '}'

-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc a
lbracket = char '['

-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc a
rbracket = char ']'

-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc a
squote = char '\''

-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc a
dquote = char '"'

-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc a
semi = char ';'

-- | The document @colon@ contains a colon, \":\".
colon :: Doc a
colon = char ':'

-- | The document @comma@ contains a comma, \",\".
comma :: Doc a
comma = char ','

-- | The document @space@ contains a single space, \" \".
--
-- > x <+> y   = x <> space <> y
space :: Doc a
space = char ' '

-- | The document @dot@ contains a single dot, \".\".
dot :: Doc a
dot = char '.'

-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc a
backslash = char '\\'

-- | The document @equals@ contains an equal sign, \"=\".
equals :: Doc a
equals = char '='

docMapAnn :: (a -> Doc a' -> Doc a') -> Doc a -> Doc a'
docMapAnn an = go
 where
  go Empty          = Empty
  go (Char x)       = Char x
  go (Text i s)     = Text i s
  go Line           = Line
  go (FlatAlt l r)  = FlatAlt (go l) (go r)
  go (Cat l r)      = Cat (go l) (go r)
  go (Nest i d)     = Nest i (go d)
  go (Union l r)    = Union (go l) (go r)
  go (Annotate a d) = an a (go d)
  go (Column f)     = Column (go . f)
  go (Nesting k)    = Nesting (go . k)
  go (Columns k)    = Columns (go . k)
  go (Ribbon k)     = Ribbon (go . k)

instance IsString (Doc a) where
  fromString = pretty

-----------------------------------------------------------
-- overloading "pretty"
-----------------------------------------------------------

-- | The member @prettyList@ is only used to define the @instance Pretty
-- a => Pretty [a]@. In normal circumstances only the @pretty@ function
-- is used.
class Pretty a where
  pretty     :: a   -> Doc b
  prettyList :: [a] -> Doc b
  prettyList = list . map pretty

  default pretty :: Show a => a -> Doc b
  pretty = text . show

instance Pretty (Doc a) where
  pretty = noAnnotate

instance Pretty a => Pretty [a] where
  pretty = prettyList

instance Pretty T.Text where
  pretty = pretty . T.unpack

instance Pretty TL.Text where
  pretty = pretty . TL.unpack

instance Pretty () where
  pretty () = text "()"

instance Pretty Char where
  pretty = char
  prettyList "" = mempty
  prettyList ('\n':s) = line <> prettyList s
  prettyList s = let (xs,ys) = span (/='\n') s in text xs <> prettyList ys

instance Pretty a => Pretty (Seq a) where
  pretty = prettyList . toList

instance Pretty a => Pretty (NonEmpty a) where
  pretty = prettyList . toList

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (x, y) = tupled [pretty x, pretty y]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pretty (x, y, z) = tupled [pretty x, pretty y, pretty z]

instance Pretty a => Pretty (Maybe a) where
  pretty = maybe mempty pretty

instance Pretty Bool
instance Pretty Int
instance Pretty Int8
instance Pretty Int16
instance Pretty Int32
instance Pretty Int64
instance Pretty Word
instance Pretty Word8
instance Pretty Word16
instance Pretty Word32
instance Pretty Word64
instance Pretty Integer
instance Pretty Natural
instance Pretty Float
instance Pretty Double
instance Pretty Rational

-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------

-- | The document @(fillBreak i x)@ first renders document @x@. It
-- then appends @space@s until the width is equal to @i@. If the
-- width of @x@ is already larger than @i@, the nesting level is
-- increased by @i@ and a @line@ is appended. When we redefine @ptype@
-- in the previous example to use @fillBreak@, we get a useful
-- variation of the previous output:
--
-- > ptype (name,tp)
-- >        = fillBreak 6 (text name) <+> text "::" <+> text tp
--
-- The output will now be:
--
-- @
-- let mempty  :: Doc a
--     nest   :: Int -> Doc a -> Doc a
--     linebreak
--            :: Doc a
-- @
fillBreak :: Int -> Doc a -> Doc a
fillBreak f x = width x $ \w ->
                  if w > f then nest f linebreak
                           else text (spaces (f - w))


-- | The document @(fill i x)@ renders document @x@. It then appends
-- @space@s until the width is equal to @i@. If the width of @x@ is
-- already larger, nothing is appended. This combinator is quite
-- useful in practice to output a list of bindings. The following
-- example demonstrates this.
--
-- > types  = [("mempty","Doc a")
-- >          ,("nest","Int -> Doc a -> Doc a")
-- >          ,("linebreak","Doc a")]
-- >
-- > ptype (name,tp)
-- >        = fill 6 (text name) <+> text "::" <+> text tp
-- >
-- > test   = text "let" <+> align (vcat (map ptype types))
--
-- Which is layed out as:
--
-- @
-- let mempty  :: Doc a
--     nest   :: Int -> Doc a -> Doc a
--     linebreak :: Doc a
-- @
fill :: Int -> Doc a -> Doc a
fill f d = width d $ \w ->
                     if w >= f
                     then mempty
                     else text (spaces (f - w))

width :: Doc a -> (Int -> Doc a) -> Doc a
width d f = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))


-----------------------------------------------------------
-- semi primitive: Alignment and indentation
-----------------------------------------------------------

-- | The document @(indent i x)@ indents document @x@ with @i@ spaces.
--
-- > test  = indent 4 (fillSep (map text
-- >         (words "the indent combinator indents these words !")))
--
-- Which lays out with a page width of 20 as:
--
-- @
--     the indent
--     combinator
--     indents these
--     words !
-- @
indent :: Int -> Doc a -> Doc a
indent i d = hang i (text (spaces i) <> d)

-- | The hang combinator implements hanging indentation. The document
-- @(hang i x)@ renders document @x@ with a nesting level set to the
-- current column plus @i@. The following example uses hanging
-- indentation for some text:
--
-- > test  = hang 4 (fillSep (map text
-- >         (words "the hang combinator indents these words !")))
--
-- Which lays out on a page with a width of 20 characters as:
--
-- @
-- the hang combinator
--     indents these
--     words !
-- @
--
-- The @hang@ combinator is implemented as:
--
-- > hang i x  = align (nest i x)
hang :: Int -> Doc a -> Doc a
hang i d = align (nest i d)

-- | The document @(align x)@ renders document @x@ with the nesting
-- level set to the current column. It is used for example to
-- implement 'hang'.
--
-- As an example, we will put a document right above another one,
-- regardless of the current nesting level:
--
-- > x $$ y = align (x <#> y)
--
-- > test = text "hi" <+> (text "nice" $$ text "world")
--
-- which will be layed out as:
--
-- @
-- hi nice
--    world
-- @
align :: Doc a -> Doc a
align d = column $ \k ->
         nesting $ \i -> nest (k - i) d   --nesting might be negative :-)

-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

-- | The data type @Doc@ represents pretty documents.
--
-- @Doc@ is an instance of the 'Show' class. @(show doc)@ pretty
-- prints document @doc@ with a page width of 100 characters and a
-- ribbon width of 40 characters.
--
-- > show (text "hello" <#> text "world")
--
-- Which would return the string \"hello\\nworld\", i.e.
--
-- @
-- hello
-- world
-- @
data Doc a
  = Empty
  | Char {-# UNPACK #-} !Char       -- invariant: char is not '\n'
  | Text {-# UNPACK #-} !Int String -- invariant: text doesn't contain '\n'
  | Line
  | FlatAlt (Doc a) (Doc a)         -- Render the first doc, but when flattened, render the second.
  | Cat (Doc a) (Doc a)
  | Nest {-# UNPACK #-} !Int (Doc a)
  | Union (Doc a) (Doc a) -- invariant: first lines of first doc longer than the first lines of the second doc
  | Annotate a (Doc a)
  | Column  (Int -> Doc a)
  | Nesting (Int -> Doc a)
  | Columns (Maybe Int -> Doc a)
  | Ribbon  (Maybe Int -> Doc a)
  deriving (Generic, Functor)

instance NFData a => NFData (Doc a)

-- | Annotate a document.
annotate :: a -> Doc a -> Doc a
annotate = Annotate

-- | Remove the annotations from a document.
noAnnotate :: Doc a -> Doc a'
noAnnotate = docMapAnn $ const id

-- | The data type @SimpleDoc@ represents rendered documents and is
-- used by the display functions.
--
-- The @Int@ in @SText@ contains the length of the string. The @Int@
-- in @SLine@ contains the indentation for that line. The library
-- provides two default display functions 'displayS' and
-- 'displayIO'. You can provide your own display function by writing a
-- function from a @SimpleDoc@ to your own output format.
data SimpleDoc a
  = SEmpty
  | SChar {-# UNPACK #-} !Char (SimpleDoc a)
  | SText {-# UNPACK #-} !Int String (SimpleDoc a)
  | SLine {-# UNPACK #-} !Int (SimpleDoc a)
  | SPushAnn a (SimpleDoc a)
  | SPopAnn  a (SimpleDoc a)
  deriving (Generic, Functor, Foldable, Traversable)

instance NFData a => NFData (SimpleDoc a)

-- | The document @(char c)@ contains the literal character @c@. The
-- character shouldn't be a newline (@'\n'@), the function 'line'
-- should be used for line breaks.
char :: Char -> Doc a
char '\n' = line
char c = Char c

-- | The document @(text s)@ contains the literal string @s@. The
-- string shouldn't contain any newline (@'\n'@) characters. If the
-- string contains newline characters, the function 'pretty' should be
-- used.
text :: String -> Doc a
text "" = Empty
text s  = Text (length s) s

-- | The @line@ document advances to the next line and indents to the
-- current nesting level. Document @line@ behaves like @(text \" \")@
-- if the line break is undone by 'group'.
line :: Doc a
line = FlatAlt Line space

-- | The @linebreak@ document advances to the next line and indents to
-- the current nesting level. Document @linebreak@ behaves like
-- 'mempty' if the line break is undone by 'group'.
linebreak :: Doc a
linebreak = FlatAlt Line mempty

-- | A linebreak that can not be flattened; it is guaranteed to be
-- rendered as a newline.
hardline :: Doc a
hardline = Line

-- | The document @(nest i x)@ renders document @x@ with the current
-- indentation level increased by i (See also 'hang', 'align' and
-- 'indent').
--
-- > nest 2 (text "hello" <#> text "world") <#> text "!"
--
-- outputs as:
--
-- @
-- hello
--   world
-- !
-- @
nest :: Int -> Doc a -> Doc a
nest = Nest

column, nesting :: (Int -> Doc a) -> Doc a
column = Column
nesting = Nesting

columns :: (Maybe Int -> Doc a) -> Doc a
columns = Columns

ribbon :: (Maybe Int -> Doc a) -> Doc a
ribbon = Ribbon

-- | The @group@ combinator is used to specify alternative
-- layouts. The document @(group x)@ undoes all line breaks in
-- document @x@. The resulting line is added to the current line if
-- that fits the page. Otherwise, the document @x@ is rendered without
-- any changes.
group :: Doc a -> Doc a
group x = Union (flatten x) x

-- | @flatAlt@ creates a document that changes when flattened; normally
-- it is rendered as the first argument, but when flattened is rendered
-- as the second.
flatAlt :: Doc a -> Doc a -> Doc a
flatAlt = FlatAlt

flatten :: Doc a -> Doc a
flatten (FlatAlt _ y)   = y
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Union x _)     = flatten x
flatten (Annotate a x)  = Annotate a (flatten x)
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten (Columns f)     = Columns (flatten . f)
flatten (Ribbon f)      = Ribbon (flatten . f)
flatten a@Empty{}       = a
flatten a@Char{}        = a
flatten a@Text{}        = a
flatten a@Line{}        = a

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc)]
data Docs a e
  = Nil
  | Cons {-# UNPACK #-} !Int (Doc a) (Docs a e)

-- | This is the default pretty printer which is used by 'show',
-- 'putDoc' and 'hPutDoc'. @(renderPretty ribbonfrac width x)@ renders
-- document @x@ with a page width of @width@ and a ribbon width of
-- @(ribbonfrac * width)@ characters. The ribbon width is the maximal
-- amount of non-indentation characters on a line. The parameter
-- @ribbonfrac@ should be between @0.0@ and @1.0@. If it is lower or
-- higher, the ribbon width will be 0 or @width@ respectively.
renderPretty :: Float -> Int -> Doc a -> SimpleDoc a
renderPretty = renderFits nicest1

-- | This is the default pretty printer which is used by 'show',
-- 'putDoc' and 'hPutDoc'. This routine uses a page width of 100 characters
-- and a ribbon width of 40 characters.
renderPrettyDefault :: Doc a -> SimpleDoc a
renderPrettyDefault = renderPretty 0.4 100

-- | A slightly smarter rendering algorithm with more lookahead. It provides
-- provide earlier breaking on deeply nested structures.
-- For example, consider this python-ish pseudocode:
-- @fun(fun(fun(fun(fun([abcdefg, abcdefg])))))@
-- If we put a softbreak (+ nesting 2) after each open parenthesis, and align
-- the elements of the list to match the opening brackets, this will render with
-- @renderPretty@ and a page width of 20c as:
-- @
-- fun(fun(fun(fun(fun([
--                     | abcdef,
--                     | abcdef,
--                     ]
--   )))))             |
-- @
-- Where the 20c. boundary has been marked with |. Because @renderPretty@ only
-- uses one-line lookahead, it sees that the first line fits, and is stuck
-- putting the second and third lines after the 20c mark. In contrast,
-- @renderSmart@ will continue to check the potential document up to the end of
-- the indentation level. Thus, it will format the document as:
--
-- @
-- fun(                |
--   fun(              |
--     fun(            |
--       fun(          |
--         fun([       |
--               abcdef,
--               abcdef,
--             ]       |
--   )))))             |
-- @
-- Which fits within the 20c. mark.
-- In addition, @renderSmart@ uses this lookahead to minimize the number of
-- lines printed, leading to more compact and visually appealing output.
-- Consider this example using the same syntax as above:
-- @aaaaaaaaaaa([abc, def, ghi])@
-- When rendered with @renderPretty@ and a page width of 20c, we get:
-- @
-- aaaaaaaaaaa([ abc
--             , def
--             , ghi ])
-- @
-- Whereas when rendered with @renderSmart@ and a page width of 20c, we get:
-- @
-- aaaaaaaaaaa(
--   [abc, def, ghi])
-- @
renderSmart :: Int -> Doc a -> SimpleDoc a
renderSmart = renderFits nicestR 1.0

renderFits :: (Int -> Int -> Int -> Int -> SimpleDoc a -> SimpleDoc a
               -> SimpleDoc a)
              -> Float -> Int -> Doc a -> SimpleDoc a
renderFits nicest rfrac w x
    = best 0 0 (\_ _ -> SEmpty) (Cons 0 x Nil)
    where
      -- r :: the ribbon width in characters
      r  = max 0 (min w (round (fromIntegral w * rfrac)))

      -- best :: n = indentation of current line
      --         k = current column
      --        (ie. (k >= n) && (k - n == count of inserted characters)
      best n k z Nil           = z n k
      best n k z (Cons i d ds) =
        case d of
          Empty         -> best n k z ds
          Char c        -> let k' = k+1 in seq k' (SChar c (best n k' z ds))
          Text l s      -> let k' = k+l in seq k' (SText l s (best n k' z ds))
          Line          -> SLine i (best i i z ds)
          FlatAlt l _   -> best n k z (Cons i l ds)
          Cat x' y      -> best n k z (Cons i x' (Cons i y ds))
          Nest j x'     -> let i' = i+j in seq i' (best n k z (Cons i' x' ds))
          Annotate a d' -> let z' n' k' = SPopAnn a $ best n' k' z ds
                           in SPushAnn a (best n k z' (Cons i d' Nil))
          Union p q     -> nicest n k w r (best n k z (Cons i p ds))
                                          (best n k z (Cons i q ds))
          Column f      -> best n k z (Cons i (f k) ds)
          Nesting f     -> best n k z (Cons i (f i) ds)
          Columns f     -> best n k z (Cons i (f $ Just w) ds)
          Ribbon f      -> best n k z (Cons i (f $ Just r) ds)

-- @nicest1@ compares the first lines of the two documents.
-- n = nesting, k = column, p = pagewidth
nicest1 :: Int -> Int -> Int -> Int -> SimpleDoc a -> SimpleDoc a -> SimpleDoc a
nicest1 n k p r x' y | fits (min n k) wid x' = x'
                     | otherwise = y
  where wid = min (p - k) (r - k + n)
        fits _ w _        | w < 0 = False
        fits _ _ SEmpty           = True
        fits m w (SChar _ x)      = fits m (w - 1) x
        fits m w (SText l _ x)    = fits m (w - l) x
        fits _ _ (SLine _ _)      = True
        fits m w (SPushAnn _ x)   = fits m w x
        fits m w (SPopAnn  _ x)   = fits m w x

-- @nicestR@ compares the initial lines of the two documents that are nested at
-- least as deep as the current nesting level. If the initial lines of both
-- documents fit within the page width, the document that takes fewer lines is
-- prefered, with preference toward the first.
nicestR :: Int -> Int -> Int -> Int -> SimpleDoc a -> SimpleDoc a -> SimpleDoc a
nicestR n k p r x' y =
  if fits (min n k) wid x' <= fits (min n k) wid y then x' else y
  where wid = min (p - k) (r - k + n)
        inf = 1.0/0 :: Double
        -- @fits@ has a little more lookahead: assuming that nesting roughly
        -- corresponds to syntactic depth, @fitsR@ checks that not only the
        -- current line fits, but the entire syntactic structure being formatted
        -- at this level of indentation fits. If we were to remove the second
        -- case for @SLine@, we would check that not only the current structure
        -- fits, but also the rest of the document, which would be slightly more
        -- intelligent but would have exponential runtime (and is prohibitively
        -- expensive in practice).
        -- m = minimum nesting level to fit in
        -- w = the width in which to fit the first line
        fits _ w _           | w < 0     = inf
        fits _ _ SEmpty                  = 0
        fits m w (SChar _ x)             = fits m (w - 1) x
        fits m w (SText l _ x)           = fits m (w - l) x
        fits m _ (SLine i x) | m < i     = 1 + fits m (p - i) x
                             | otherwise = 0
        fits m w (SPushAnn _ x)          = fits m w x
        fits m w (SPopAnn  _ x)          = fits m w x


-----------------------------------------------------------
-- renderCompact: renders documents without indentation
--  fast and fewer characters output, good for machines
-----------------------------------------------------------


-- | @(renderCompact x)@ renders document @x@ without adding any
-- indentation. Since no \'pretty\' printing is involved, this
-- renderer is very fast. The resulting output contains fewer
-- characters than a pretty printed version and can be used for output
-- that is read by other programs.
renderCompact :: Doc a -> SimpleDoc a
renderCompact x
    = scan SEmpty 0 [x]
    where
      scan z _ []     = z
      scan z k (d:ds) =
        case d of
          Empty         -> scan z k ds
          Char c        -> let k' = k+1 in seq k' (SChar c (scan z k' ds))
          Text l s      -> let k' = k+l in seq k' (SText l s (scan z k' ds))
          Annotate a d' -> SPushAnn a (scan (SPopAnn a $ scan z k ds) k [d'])
          Line          -> SLine 0 (scan z 0 ds)
          FlatAlt y _   -> scan z k (y:ds)
          Cat y z'      -> scan z k (y:z':ds)
          Nest _ y      -> scan z k (y:ds)
          Union _ y     -> scan z k (y:ds)
          Column f      -> scan z k (f k:ds)
          Nesting f     -> scan z k (f 0:ds)
          Columns f     -> scan z k (f Nothing:ds)
          Ribbon  f     -> scan z k (f Nothing:ds)

-----------------------------------------------------------
-- Displayers:  displayS and displayIO
-----------------------------------------------------------

simpleDocMapAnn :: (r -> a -> r)                            -- ^ SPushAnn state update
                -> (r -> a -> r)                            -- ^ SPopAnn state update
                -> (r -> SimpleDoc a' -> SimpleDoc a')      -- ^ SPushAnn processor
                -> (r -> SimpleDoc a' -> SimpleDoc a')      -- ^ SPopAnn processor
                -> r                                        -- ^ Initial state
                -> SimpleDoc a -> SimpleDoc a'
simpleDocMapAnn upPush upPop push pop = go
 where
  go _ SEmpty         = SEmpty
  go r (SChar c x)    = SChar c   (go r x)
  go r (SText l s x)  = SText l s (go r x)
  go r (SLine i x)    = SLine i   (go r x)
  go r (SPushAnn a x) = let r' = upPush r a in push r' $ go r' x
  go r (SPopAnn  a x) = let r' = upPop  r a in pop  r' $ go r' x

simpleDocScanAnn :: (r -> a -> r) -- ^ SPushAnn state merge
                 -> r             -- ^ Initial state
                 -> SimpleDoc a
                 -> SimpleDoc r
simpleDocScanAnn f r0 = simpleDocMapAnn merge pop (SPushAnn . head) (SPopAnn . head) [r0]
  where merge rs@(r:_) x = f r x : rs
        merge []     _ = error "Stack underflow"
        pop   (_:rs) _ = rs
        pop   []     _ = error "Stack underflow"

-- | Display a rendered document.
--
-- This function takes a means of pushing an annotated region, a means of ending it,
-- and a means of displaying a string, with effects @f@ to display or compute the output @o@.
displayDecoratedA :: (Applicative f, Monoid o)
                  => (a -> f o)        -- ^ How to push an annotated region
                  -> (a -> f o)        -- ^ How to end an annotated region
                  -> (String -> f o)   -- ^ How to display a string (from document or whitespace)
                  -> SimpleDoc a
                  -> f o
displayDecoratedA push pop str = go
 where
  go SEmpty         = pure mempty
  go (SChar c x)    = str (pure c) <++> go x
  go (SText _ s x)  = str s <++> go x
  go (SLine i x)    = str ('\n':spaces i) <++> go x
  go (SPushAnn a x) = push a <++> go x
  go (SPopAnn  a x) = pop  a <++> go x
  (<++>) = liftA2 mappend
{-# SPECIALIZE displayDecoratedA :: Monoid o => (a -> Identity o) -> (a -> Identity o) -> (String -> Identity o) -> SimpleDoc a -> Identity o #-}
{-# SPECIALIZE displayDecoratedA :: Monoid o => (a -> (o -> o)) -> (a -> (o -> o)) -> (String -> (o -> o)) -> SimpleDoc a -> (o -> o) #-}

-- | Display a rendered document.
--
-- This function takes a means of pushing an annotated region, a means of ending it,
-- and a means of displaying a string to compute the output @o@.
displayDecorated :: Monoid o
                 => (a -> o)        -- ^ How to push an annotated region
                 -> (a -> o)        -- ^ How to end an annotated region
                 -> (String -> o)   -- ^ How to display a string (from document or whitespace)
                 -> SimpleDoc a
                 -> o
displayDecorated push pop str = runIdentity .
  displayDecoratedA (pure . push) (pure . pop) (pure . str)

-- | @(displayIO handle simpleDoc)@ writes @simpleDoc@ to the file
-- handle @handle@, discarding all annotations. This function
-- is used for example by 'hPutDoc':
--
-- > hPutDoc handle doc = displayIO handle (renderPrettyDefault doc)
displayIO :: Handle -> SimpleDoc a -> IO ()
displayIO handle = displayDecoratedA cpu cpu (hPutStr handle)
 where cpu = const $ pure ()

-- | @(displayS simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and transforms it to a 'ShowS' type (for use in
-- the 'Show' class). Along the way, all annotations are
-- discarded.
displayS :: SimpleDoc a -> ShowS
displayS = displayDecoratedA ci ci (++)
 where ci = const id

-- | @(display simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and outputs a 'String'. Along the way, all annotations are
-- discarded.
display :: SimpleDoc a -> String
display = flip displayS ""

-- | @(display simpleDoc)@ takes the output @simpleDoc@ from a
-- rendering function and outputs a 'Text'. Along the way, all annotations are
-- discarded.
displayT :: SimpleDoc a -> TL.Text
displayT = TL.toLazyText . displayDecorated cm cm TL.fromString
 where cm = const mempty

-- | The type alias @SpanList@ is used by @displaySpan@
--
-- First element is the starting position, second the length
-- and third the annotation at the given range.
type SpanList a = [(Int, Int, a)]

-- | Generate a pair of a string and a list of source span/annotation pairs
displaySpans :: Monoid o => (String -> o) -> SimpleDoc a -> (o, SpanList a)
displaySpans str = go 0 []
 where
  go _ []          SEmpty          = (mempty, [])
  go i stk         (SChar c x)     = first (mappend $ str $ pure c) $ go (i+1) stk x
  go i stk         (SText l s x)   = first (mappend $ str s) $ go (i + l) stk x
  go i stk         (SLine ind x)   = first (mappend $ str $ '\n':spaces ind) $ go (1+i+ind) stk x
  go i stk         (SPushAnn _ x)  = go i (i:stk) x
  go i (start:stk) (SPopAnn ann x) = second ((start, i-start, ann):) $ go i stk x
  go _ _           SEmpty          = error "Stack not empty"
  go _ []          (SPopAnn _ _)   = error "Stack underflow"

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show (Doc a) where
  showsPrec _ = displayS . renderPrettyDefault

-- | The action @(putDoc doc)@ pretty prints document @doc@ to the
-- standard output, with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main :: IO ()
-- > main = do{ putDoc (text "hello" <+> text "world") }
--
-- Which would output
--
-- @
-- hello world
-- @
putDoc :: Doc a -> IO ()
putDoc = hPutDoc stdout

-- | @(hPutDoc handle doc)@ pretty prints document @doc@ to the file
-- handle @handle@ with a page width of 100 characters and a ribbon
-- width of 40 characters.
--
-- > main = do{ handle <- openFile "MyFile" WriteMode
-- >          ; hPutDoc handle (vcat (map text
-- >                            ["vertical","text"]))
-- >          ; hClose handle
-- >          }
hPutDoc :: Handle -> Doc a -> IO ()
hPutDoc handle = displayIO handle . renderPrettyDefault

-----------------------------------------------------------
-- insert spaces
-----------------------------------------------------------
spaces :: Int -> String
spaces n | n <= 0    = ""
         | otherwise = replicate n ' '
