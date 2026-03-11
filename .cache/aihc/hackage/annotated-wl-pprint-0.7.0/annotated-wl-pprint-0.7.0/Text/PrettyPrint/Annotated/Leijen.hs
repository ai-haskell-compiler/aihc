module Text.PrettyPrint.Annotated.Leijen (
  -- * Documents, parametrized by their annotations
  Doc, putDoc, hPutDoc,

  -- * Basic combinators
  empty, char, text, (<>), nest, line, linebreak, group, softline,
  softbreak,
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
  align, hang, indent, encloseSep, list, tupled, semiBraces,
  -- * Operators
  (<+>), (<$>), (</>), (<$$>), (<//>),
  -- * List combinators
  hsep, vsep, fillSep, sep, hcat, vcat, fillCat, cat, punctuate,

  -- * Fillers
  fill, fillBreak,

  -- * Bracketing combinators
  enclose, squotes, dquotes, parens, angles, braces, brackets,

  -- * Character documents
  lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
  squote, dquote, semi, colon, comma, space, dot, backslash, equals,
  pipe,

  -- * Primitive type documents
  string, int, integer, float, double, rational, bool,

  -- * Pretty class
  --Pretty(..),

  -- * Semantic annotations
  annotate, noAnnotate,

  -- * Rendering
  SimpleDoc(..), renderPretty, renderCompact, displayDecorated, displayDecoratedA, display, displayS, displayIO,
  SpanList(..), displaySpans

  -- * Undocumented


  , column, nesting, width
) where

import System.IO (Handle,hPutStr,hPutChar,stdout)
import Data.String

-- Import Prelude names by hand, because "import Prelude hiding ((<$>)) fails on GHC 7.4
import Prelude ((.), ($), (/=), (<), (<=), (>), (>=), (-), (*), (+), (++),
                Bool(..), Char, Double, Float, Functor, Int, Integer, IO, Rational, Show, ShowS,
                id, error, flip, foldr1, fromIntegral, length, max, min, otherwise, repeat, replicate,
                return, round, seq, show, showChar, showString, showsPrec, span, zipWith)

import Control.Applicative (Applicative(..), liftA2)
import Data.Monoid (Monoid(..))

infixr 5 </>,<//>,<$>,<$$>
infixr 6 <>,<+>


instance IsString (Doc a) where
    fromString = text

-----------------------------------------------------------
-- list, tupled and semiBraces pretty print a list of
-- documents either horizontally or vertically aligned.
-----------------------------------------------------------


-- | The document @(list xs)@ comma separates the documents @xs@ and
-- encloses them in square brackets. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
list :: [Doc a] -> Doc a
list            = encloseSep lbracket rbracket comma

-- | The document @(tupled xs)@ comma separates the documents @xs@ and
-- encloses them in parenthesis. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All comma separators are put in front of the elements.
tupled :: [Doc a] -> Doc a
tupled          = encloseSep lparen   rparen  comma


-- | The document @(semiBraces xs)@ separates the documents @xs@ with
-- semi colons and encloses them in braces. The documents are rendered
-- horizontally if that fits the page. Otherwise they are aligned
-- vertically. All semi colons are put in front of the elements.
semiBraces :: [Doc a] -> Doc a
semiBraces      = encloseSep lbrace   rbrace  semi

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
-- list [10,200,3000]
-- @
--
-- But when the page width is 15, it is layed out as:
--
-- @
-- list [10
--      ,200
--      ,3000]
-- @
encloseSep :: Doc a -> Doc a -> Doc a -> [Doc a] -> Doc a
encloseSep left right sep ds
   = case ds of
       []  -> left <> right
       [d] -> left <> d <> right
       _   -> align (cat (zipWith (<>) (left : repeat sep) ds) <> right)


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
punctuate :: Doc a -> [Doc a] -> [Doc a]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds


-----------------------------------------------------------
-- high-level combinators
-----------------------------------------------------------


-- | The document @(sep xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<+\>)@, if it fits the page, or vertically with
-- @(\<$\>)@.
--
-- > sep xs  = group (vsep xs)
sep :: [Doc a] -> Doc a
sep             = group . vsep

-- | The document @(fillSep xs)@ concatenates documents @xs@
-- horizontally with @(\<+\>)@ as long as its fits the page, than
-- inserts a @line@ and continues doing that for all documents in
-- @xs@.
--
-- > fillSep xs  = foldr (</>) empty xs
fillSep :: [Doc a] -> Doc a
fillSep         = fold (</>)

-- | The document @(hsep xs)@ concatenates all documents @xs@
-- horizontally with ('<+>').
hsep :: [Doc a] -> Doc a
hsep            = fold (<+>)


-- | The document @(vsep xs)@ concatenates all documents @xs@
-- vertically with @(\<$\>)@. If a 'group' undoes the line breaks
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
-- > test     = text "some" <+> align (vsep someText)
--
-- Which is printed as:
--
-- @
-- some text
--      to
--      lay
--      out
-- @
vsep :: [Doc a] -> Doc a
vsep            = fold (<$>)

-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @(\<$$\>)@.
--
-- > cat xs  = group (vcat xs)
cat :: [Doc a] -> Doc a
cat             = group . vcat

-- | The document @(fillCat xs)@ concatenates documents @xs@
-- horizontally with @(\<\>)@ as long as its fits the page, than inserts
-- a @linebreak@ and continues doing that for all documents in @xs@.
--
-- > fillCat xs  = foldr (\<\/\/\>) empty xs
fillCat :: [Doc a] -> Doc a
fillCat         = fold (<//>)

-- | The document @(hcat xs)@ concatenates all documents @xs@
-- horizontally with @(\<\>)@.
hcat :: [Doc a] -> Doc a
hcat            = fold (<>)

-- | The document @(vcat xs)@ concatenates all documents @xs@
-- vertically with @(\<$$\>)@. If a 'group' undoes the line breaks
-- inserted by @vcat@, all documents are directly concatenated.
vcat :: [Doc a] -> Doc a
vcat            = fold (<$$>)

fold f []       = empty
fold f ds       = foldr1 f ds

-- | The document @(x \<\> y)@ concatenates document @x@ and document
-- @y@. It is an associative operation having 'empty' as a left and
-- right unit.  (infixr 6)
(<>) :: Doc a -> Doc a -> Doc a
x <> y          = x `beside` y

-- | The document @(x \<+\> y)@ concatenates document @x@ and @y@ with a
-- @space@ in between.  (infixr 6)
(<+>) :: Doc a -> Doc a -> Doc a
x <+> y         = x <> space <> y

-- | The document @(x \<\/\> y)@ concatenates document @x@ and @y@ with a
-- 'softline' in between. This effectively puts @x@ and @y@ either
-- next to each other (with a @space@ in between) or underneath each
-- other. (infixr 5)
(</>) :: Doc a -> Doc a -> Doc a
x </> y         = x <> softline <> y

-- | The document @(x \<\/\/\> y)@ concatenates document @x@ and @y@ with
-- a 'softbreak' in between. This effectively puts @x@ and @y@ either
-- right next to each other or underneath each other. (infixr 5)
(<//>) :: Doc a -> Doc a -> Doc a
x <//> y        = x <> softbreak <> y

-- | The document @(x \<$\> y)@ concatenates document @x@ and @y@ with a
-- 'line' in between. (infixr 5)
(<$>) :: Doc a -> Doc a -> Doc a
x <$> y         = x <> line <> y

-- | The document @(x \<$$\> y)@ concatenates document @x@ and @y@ with
-- a @linebreak@ in between. (infixr 5)
(<$$>) :: Doc a -> Doc a -> Doc a
x <$$> y        = x <> linebreak <> y

-- | The document @softline@ behaves like 'space' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softline = group line
softline :: Doc a
softline        = group line

-- | The document @softbreak@ behaves like 'empty' if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softbreak  = group linebreak
softbreak :: Doc a
softbreak       = group linebreak

-- | Document @(squotes x)@ encloses document @x@ with single quotes
-- \"'\".
squotes :: Doc a -> Doc a
squotes         = enclose squote squote

-- | Document @(dquotes x)@ encloses document @x@ with double quotes
-- '\"'.
dquotes :: Doc a -> Doc a
dquotes         = enclose dquote dquote

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and
-- \"}\".
braces :: Doc a -> Doc a
braces          = enclose lbrace rbrace

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\"
-- and \")\".
parens :: Doc a -> Doc a
parens          = enclose lparen rparen

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and
-- \"\>\".
angles :: Doc a -> Doc a
angles          = enclose langle rangle

-- | Document @(brackets x)@ encloses document @x@ in square brackets,
-- \"[\" and \"]\".
brackets :: Doc a -> Doc a
brackets        = enclose lbracket rbracket

-- | The document @(enclose l r x)@ encloses document @x@ between
-- documents @l@ and @r@ using @(\<\>)@.
--
-- > enclose l r x   = l <> x <> r
enclose :: Doc a -> Doc a -> Doc a -> Doc a
enclose l r x   = l <> x <> r

-- | The document @lparen@ contains a left parenthesis, \"(\".
lparen :: Doc a
lparen          = char '('
-- | The document @rparen@ contains a right parenthesis, \")\".
rparen :: Doc a
rparen          = char ')'
-- | The document @langle@ contains a left angle, \"\<\".
langle :: Doc a
langle          = char '<'
-- | The document @rangle@ contains a right angle, \">\".
rangle :: Doc a
rangle          = char '>'
-- | The document @lbrace@ contains a left brace, \"{\".
lbrace :: Doc a
lbrace          = char '{'
-- | The document @rbrace@ contains a right brace, \"}\".
rbrace :: Doc a
rbrace          = char '}'
-- | The document @lbracket@ contains a left square bracket, \"[\".
lbracket :: Doc a
lbracket        = char '['
-- | The document @rbracket@ contains a right square bracket, \"]\".
rbracket :: Doc a
rbracket        = char ']'


-- | The document @squote@ contains a single quote, \"'\".
squote :: Doc a
squote          = char '\''
-- | The document @dquote@ contains a double quote, '\"'.
dquote :: Doc a
dquote          = char '"'
-- | The document @semi@ contains a semi colon, \";\".
semi :: Doc a
semi            = char ';'
-- | The document @colon@ contains a colon, \":\".
colon :: Doc a
colon           = char ':'
-- | The document @comma@ contains a comma, \",\".
comma :: Doc a
comma           = char ','
-- | The document @space@ contains a single space, \" \".
--
-- > x <+> y   = x <> space <> y
space :: Doc a
space           = char ' '
-- | The document @dot@ contains a single dot, \".\".
dot :: Doc a
dot             = char '.'
-- | The document @backslash@ contains a back slash, \"\\\".
backslash :: Doc a
backslash       = char '\\'
-- | The document @equals@ contains an equal sign, \"=\".
equals :: Doc a
equals          = char '='
-- | The document @pipe@ contains a pipe character, \"\|\".
pipe :: Doc a
pipe            = char '|'

-----------------------------------------------------------
-- Combinators for prelude types
-----------------------------------------------------------

-- string is like "text" but replaces '\n' by "line"

-- | The document @(string s)@ concatenates all characters in @s@
-- using @line@ for newline characters and @char@ for all other
-- characters. It is used instead of 'text' whenever the text contains
-- newline characters.
string :: String -> Doc a
string ""       = empty
string ('\n':s) = line <> string s
string s        = case (span (/='\n') s) of
                   (xs,ys) -> text xs <> string ys

-- | The document @(bool b)@ is @text "True"@ when @b@ is true, and @text
-- "False"@ otherwise.
bool :: Bool -> Doc a
bool b          = text (show b)

-- | The document @(int i)@ shows the literal integer @i@ using
-- 'text'.
int :: Int -> Doc a
int i           = text (show i)

-- | The document @(integer i)@ shows the literal integer @i@ using
-- 'text'.
integer :: Integer -> Doc a
integer i       = text (show i)

-- | The document @(float f)@ shows the literal float @f@ using
-- 'text'.
float :: Float -> Doc a
float f         = text (show f)

-- | The document @(double d)@ shows the literal double @d@ using
-- 'text'.
double :: Double -> Doc a
double d        = text (show d)

-- | The document @(rational r)@ shows the literal rational @r@ using
-- 'text'.
rational :: Rational -> Doc a
rational r      = text (show r)





-----------------------------------------------------------
-- semi primitive: fill and fillBreak
-----------------------------------------------------------

-- | The document @(fillBreak i x)@ first renders document @x@. It
-- than appends @space@s until the width is equal to @i@. If the
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
-- let empty  :: Doc a
--     nest   :: Int -> Doc a -> Doc a
--     linebreak
--            :: Doc a
-- @
fillBreak :: Int -> Doc a -> Doc a
fillBreak f x   = width x (\w ->
                 if (w > f) then nest f linebreak
                            else text (spaces (f - w)))


-- | The document @(fill i x)@ renders document @x@. It than appends
-- @space@s until the width is equal to @i@. If the width of @x@ is
-- already larger, nothing is appended. This combinator is quite
-- useful in practice to output a list of bindings. The following
-- example demonstrates this.
--
-- > types  = [("empty","Doc a")
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
-- let empty  :: Doc a
--     nest   :: Int -> Doc a -> Doc a
--     linebreak :: Doc a
-- @
fill :: Int -> Doc a -> Doc a
fill f d        = width d (\w ->
                 if (w >= f) then empty
                             else text (spaces (f - w)))

width :: Doc a -> (Int -> Doc a) -> Doc a
width d f       = column (\k1 -> d <> column (\k2 -> f (k2 - k1)))


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
indent i d      = hang i (text (spaces i) <> d)

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
hang i d        = align (nest i d)

-- | The document @(align x)@ renders document @x@ with the nesting
-- level set to the current column. It is used for example to
-- implement 'hang'.
--
-- As an example, we will put a document right above another one,
-- regardless of the current nesting level:
--
-- > x $$ y  = align (x <$> y)
--
-- > test    = text "hi" <+> (text "nice" $$ text "world")
--
-- which will be layed out as:
--
-- @
-- hi nice
--    world
-- @
align :: Doc a -> Doc a
align d         = column (\k ->
                 nesting (\i -> nest (k - i) d))   --nesting might be negative :-)



-----------------------------------------------------------
-- Primitives
-----------------------------------------------------------

-- | The abstract data type @Doc a@ represents pretty documents.
--
-- @Doc a@ is an instance of the 'Show' class. @(show doc)@ pretty
-- prints document @doc@ with a page width of 100 characters and a
-- ribbon width of 40 characters.
--
-- > show (text "hello" <$> text "world")
--
-- Which would return the string \"hello\\nworld\", i.e.
--
-- @
-- hello
-- world
-- @
data Doc a     = Empty
               | Char Char             -- invariant: char is not '\n'
               | Text !Int String      -- invariant: text doesn't contain '\n'
               | Line !Bool            -- True <=> when undone by group, do not insert a space
               | Cat (Doc a) (Doc a)
               | Nest !Int (Doc a)
               | Union (Doc a) (Doc a)         -- invariant: first lines of first doc longer than the first lines of the second doc
               | Column  (Int -> Doc a)
               | Nesting (Int -> Doc a)
               | Annotate a (Doc a) -- The contained document, annotated by the info
               | AnnotEnd           -- Only used during rendering - indicates the end of an annotation
  deriving Functor
type SpanList a = [(Int, Int, a)]

-- | The data type @SimpleDoc a@ represents rendered documents and is
-- used by the display functions.
--
-- The @Int@ in @SText@ contains the length of the string. The @Int@
-- in @SLine@ contains the indentation for that line. The library
-- provides two default display functions 'displayS' and
-- 'displayIO'. You can provide your own display function by writing a
-- function from a @SimpleDoc a@ to your own output format.
data SimpleDoc a  = SEmpty
               | SChar Char (SimpleDoc a)
               | SText !Int String (SimpleDoc a)
               | SLine !Int (SimpleDoc a)
               | SAnnotStart a (SimpleDoc a)
               | SAnnotStop (SimpleDoc a)
  deriving Functor

-- | The empty document is, indeed, empty. Although @empty@ has no
-- content, it does have a \'height\' of 1 and behaves exactly like
-- @(text \"\")@ (and is therefore not a unit of @\<$\>@).
empty :: Doc a
empty           = Empty

-- | The document @(char c)@ contains the literal character @c@. The
-- character shouldn't be a newline (@'\n'@), the function 'line'
-- should be used for line breaks.
char :: Char -> Doc a
char '\n'       = line
char c          = Char c

-- | The document @(text s)@ contains the literal string @s@. The
-- string shouldn't contain any newline (@'\n'@) characters. If the
-- string contains newline characters, the function 'string' should be
-- used.
text :: String -> Doc a
text ""         = Empty
text s          = Text (length s) s

-- | The @line@ document advances to the next line and indents to the
-- current nesting level. Doc aument @line@ behaves like @(text \" \")@
-- if the line break is undone by 'group'.
line :: Doc a
line            = Line False

-- | The @linebreak@ document advances to the next line and indents to
-- the current nesting level. Document @linebreak@ behaves like
-- 'empty' if the line break is undone by 'group'.
linebreak :: Doc a
linebreak       = Line True

beside x y      = Cat x y

-- | The document @(nest i x)@ renders document @x@ with the current
-- indentation level increased by i (See also 'hang', 'align' and
-- 'indent').
--
-- > nest 2 (text "hello" <$> text "world") <$> text "!"
--
-- outputs as:
--
-- @
-- hello
--   world
-- !
-- @
nest :: Int -> Doc a -> Doc a
nest i x        = Nest i x

column, nesting :: (Int -> Doc a) -> Doc a
column f        = Column f
nesting f       = Nesting f

-- | The @group@ combinator is used to specify alternative
-- layouts. The document @(group x)@ undoes all line breaks in
-- document @x@. The resulting line is added to the current line if
-- that fits the page. Otherwise, the document @x@ is rendered without
-- any changes.
group :: Doc a -> Doc a
group x         = Union (flatten x) x

flatten :: Doc a -> Doc a
flatten (Cat x y)       = Cat (flatten x) (flatten y)
flatten (Nest i x)      = Nest i (flatten x)
flatten (Line break)    = if break then Empty else Text 1 " "
flatten (Union x y)     = flatten x
flatten (Column f)      = Column (flatten . f)
flatten (Nesting f)     = Nesting (flatten . f)
flatten other           = other                     --Empty,Char,Text

-----------------------------------------------------------
-- Semantic annotations
-----------------------------------------------------------

annotate :: a -> Doc a -> Doc a
annotate = Annotate

-- | Strip annotations from a document. This is useful for re-using the
-- textual formatting of some sub-document, but applying a different
-- high-level annotation.
noAnnotate :: Doc a -> Doc a
noAnnotate (Cat x y) = Cat (noAnnotate x) (noAnnotate y)
noAnnotate (Nest i x) = Nest i (noAnnotate x)
noAnnotate (Union x y) = Union (noAnnotate x) (noAnnotate y)
noAnnotate (Column f) = Column (noAnnotate . f)
noAnnotate (Nesting f) = Nesting (noAnnotate . f)
noAnnotate (Annotate _ x) = noAnnotate x
noAnnotate other = other

-----------------------------------------------------------
-- Renderers
-----------------------------------------------------------

-----------------------------------------------------------
-- renderPretty: the default pretty printing algorithm
-----------------------------------------------------------

-- list of indentation/document pairs; saves an indirection over [(Int,Doc a)]
data Docs a = Nil
            | Cons !Int (Doc a) (Docs a)


-- | This is the default pretty printer which is used by 'show',
-- 'putDoc' and 'hPutDoc'. @(renderPretty ribbonfrac width x)@ renders
-- document @x@ with a page width of @width@ and a ribbon width of
-- @(ribbonfrac * width)@ characters. The ribbon width is the maximal
-- amount of non-indentation characters on a line. The parameter
-- @ribbonfrac@ should be between @0.0@ and @1.0@. If it is lower or
-- higher, the ribbon width will be 0 or @width@ respectively.
renderPretty :: Float -> Int -> Doc a -> SimpleDoc a
renderPretty rfrac w x
   = best 0 0 (Cons 0 x Nil)
   where
     -- r :: the ribbon width in characters
     r  = max 0 (min w (round (fromIntegral w * rfrac)))

     -- best :: n = indentation of current line
     --         k = current column
     --        (ie. (k >= n) && (k - n == count of inserted characters)
     best n k Nil      = SEmpty
     best n k (Cons i d ds)
       = case d of
           Empty       -> best n k ds
           Char c      -> let k' = k+1 in seq k' (SChar c (best n k' ds))
           Text l s    -> let k' = k+l in seq k' (SText l s (best n k' ds))
           Line _      -> SLine i (best i i ds)
           Cat x y     -> best n k (Cons i x (Cons i y ds))
           Nest j x    -> let i' = i+j in seq i' (best n k (Cons i' x ds))
           Union x y   -> nicest n k (best n k (Cons i x ds))
                                     (best n k (Cons i y ds))

           Column f    -> best n k (Cons i (f k) ds)
           Nesting f   -> best n k (Cons i (f i) ds)
           Annotate a d' -> SAnnotStart a (best n k (Cons i d' (Cons i AnnotEnd ds)))
           AnnotEnd    -> SAnnotStop (best n k ds)

     --nicest :: r = ribbon width, w = page width,
     --          n = indentation of current line, k = current column
     --          x and y, the (simple) documents to chose from.
     --          precondition: first lines of x are longer than the first lines of y.
     nicest n k x y    | fits width x  = x
                       | otherwise     = y
                       where
                         width = min (w - k) (r - k + n)


fits w x        | w < 0         = False
fits w SEmpty                   = True
fits w (SChar c x)              = fits (w - 1) x
fits w (SText l s x)            = fits (w - l) x
fits w (SLine i x)              = True
fits w (SAnnotStart _ x)        = fits w x
fits w (SAnnotStop x)           = fits w x


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
   = scan 0 [x]
   where
     scan k []     = SEmpty
     scan k (d:ds) = case d of
                       Empty         -> scan k ds
                       Char c        -> let k' = k+1 in seq k' (SChar c (scan k' ds))
                       Text l s      -> let k' = k+l in seq k' (SText l s (scan k' ds))
                       Line _        -> SLine 0 (scan 0 ds)
                       Cat x y       -> scan k (x:y:ds)
                       Nest j x      -> scan k (x:ds)
                       Union x y     -> scan k (y:ds)
                       Column f      -> scan k (f k:ds)
                       Nesting f     -> scan k (f 0:ds)
                       Annotate a d' -> SAnnotStart a (scan k (d':AnnotEnd:ds))
                       AnnotEnd      -> SAnnotStop $ scan k ds



-----------------------------------------------------------
-- Displayers:  displayS and displayIO (and display)
-----------------------------------------------------------

-- | @(display simpleDoc)@ transforms the @simpleDoc@ to a 'String'.
display :: SimpleDoc a -> String
display = flip displayS ""

-- | @(displayS simpleDoc a)@ takes the output @simpleDoc a@ from a
-- rendering function and transforms it to a 'ShowS' type (for use in
-- the 'Show' class).
--
-- > showWidth :: Int -> Doc a -> String
-- > showWidth w x   = displayS (renderPretty 0.4 w x) ""
displayS :: SimpleDoc a -> ShowS
displayS SEmpty             = id
displayS (SChar c x)        = showChar c . displayS x
displayS (SText l s x)      = showString s . displayS x
displayS (SLine i x)        = showString ('\n':indentation i) . displayS x
displayS (SAnnotStart _ x)  = displayS x
displayS (SAnnotStop x)     = displayS x

-- | @(displayIO handle simpleDoc a)@ writes @simpleDoc a@ to the file
-- handle @handle@. This function is used for example by 'hPutDoc a':
--
-- > hPutDoc a handle doc  = displayIO handle (renderPretty 0.4 100 doc)
displayIO :: Handle -> SimpleDoc a -> IO ()
displayIO handle simpleDoc
   = display simpleDoc
   where
     display SEmpty            = return ()
     display (SChar c x)       = do{ hPutChar handle c; display x}
     display (SText l s x)     = do{ hPutStr handle s; display x}
     display (SLine i x)       = do{ hPutStr handle ('\n':indentation i); display x}
     display (SAnnotStart _ x) = display x
     display (SAnnotStop x)    = display x

-- | Generate a pair of a string and a list of source span/annotation pairs
displaySpans :: SimpleDoc a -> (String, SpanList a)
displaySpans sd = display 0 [] sd
  where display :: Int -> [(Int, a)] -> SimpleDoc a -> (String, SpanList a)
        display i []                 SEmpty              = ("", [])
        display i stk                (SChar c x)         = let (str, spans) = display (i+1) stk x
                                                           in (c:str, spans)
        display i stk                (SText l s x)       = mapFst (s++) (display (i + l) stk x)
        display i stk                (SLine ind x)       = mapFst (('\n':indentation ind)++) (display (1+i+ind) stk x)
        display i stk                (SAnnotStart ann x) = display i ((i, ann):stk) x
        display i ((start, ann):stk) (SAnnotStop x)      = mapSnd ((start, i-start, ann):) (display i stk x)

        -- malformed documents
        display _ []  (SAnnotStop _) = error "stack underflow"
        display _ stk SEmpty         = error "Stack not consumed by rendering"

        mapFst :: (a -> b) -> (a, c) -> (b, c)
        mapFst f (x, y) = (f x, y)

        mapSnd :: (a -> b) -> (c, a) -> (c, b)
        mapSnd f (x, y) = (x, f y)

-- | Render a string, where annotated regions are decorated by a user-provided function.
displayDecorated :: (a -> String -> String) -> SimpleDoc a -> String
displayDecorated decor sd = display id id [] sd ""
  where display s d []                SEmpty              = d . s
        display s d stk               (SChar c x)         = display (s . showChar c) d stk x
        display s d stk               (SText l str x)     = display (s . showString str) d stk x
        display s d stk               (SLine ind x)       = display (s . showString ('\n':indentation ind)) d stk x
        display s d stk               (SAnnotStart ann x) = display id (decor ann) ((s, d):stk) x
        display s d ((sf', d'):stk)   (SAnnotStop x)      = let formatted = d (s "")
                                                            in display (sf' . showString formatted) d' stk x
        -- malformed documents
        display _ _ [] (SAnnotStop _) = error "stack underflow"
        display _ _ stk SEmpty = error "stack not consumed by rendering"

displayDecoratedA :: (Applicative f, Monoid b)
                  => (String -> f b) -> (a -> f b) -> (a -> f b)
                  -> SimpleDoc a -> f b
displayDecoratedA str start end sd = display [] sd
  where display []        SEmpty              = pure mempty
        display stk       (SChar c x)         = (str [c]) <++> (display stk x)
        display stk       (SText l s x)       = (str s) <++> (display stk x)
        display stk       (SLine ind x)       = (str ('\n':indentation ind)) <++> (display stk x)
        display stk       (SAnnotStart ann x) = (start ann) <++> (display (ann:stk) x)
        display (ann:stk) (SAnnotStop x)      = (end ann) <++> (display stk x)

        -- malformed documents
        display []        (SAnnotStop _)      = error "stack underflow"
        display stk       SEmpty              = error "stack not consumed by rendering"

        (<++>) = liftA2 mappend

-----------------------------------------------------------
-- default pretty printers: show, putDoc and hPutDoc
-----------------------------------------------------------
instance Show (Doc a) where
 showsPrec d doc       = displayS (renderPretty 0.4 80 doc)

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
putDoc doc              = hPutDoc stdout doc

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
hPutDoc handle doc      = displayIO handle (renderPretty 0.4 80 doc)



-----------------------------------------------------------
-- insert spaces
-- "indentation" used to insert tabs but tabs seem to cause
-- more trouble than they solve :-)
-----------------------------------------------------------
spaces n       | n <= 0    = ""
               | otherwise = replicate n ' '

indentation n   = spaces n

--indentation n   | n >= 8    = '\t' : indentation (n-8)
--                | otherwise = spaces n
