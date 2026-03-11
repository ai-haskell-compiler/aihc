{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | This module is based, in part, on some of the interface for
-- "Text.PrettyPrint.Annotated.Leijen".
--
module Text.PrettyPrint.Leijen.Extended
  (
    -- * Pretty-print typeclass
    Pretty (..)

    -- * Documents annotated by a style
  , StyleDoc (..)
  , StyleAnn (..)
  , displayAnsi
  , displayPlain
  , renderDefault

    -- * Selective use of the "Text.PrettyPrint.Annotated.Leijen" interface
    --
    -- | Documented omissions by reference to package
    -- @annotated-wl-pprint-0.7.0@.

    -- ** Documents, parametrized by their annotations
    --
    -- | Omitted compared to original:
    --
    -- @
    -- Doc, putDoc, hPutDoc
    -- @

    -- ** Basic combinators
    --
    -- | Omitted compared to the original:
    --
    -- @
    -- empty, char, text, (<>)
    -- @
    --
    -- Instead of @empty@, use 'mempty'.
    --
    -- Instead of @char@ and @text@, use 'fromString'.
    --
    -- A 'Monoid' instance for 'StyleDoc' is defined.
  , nest
  , line
  , linebreak
  , group
  , softline
  , softbreak

    -- ** Alignment
    --
    -- | The combinators in this section can not be described by Wadler's
    -- original combinators. They align their output relative to the current
    -- output position - in contrast to 'nest' which always aligns to the
    -- current nesting level. This deprives these combinators from being
    -- \`optimal\'. In practice however they prove to be very useful. The
    -- combinators in this section should be used with care, since they are more
    -- expensive than the other combinators. For example, 'align' shouldn't be
    -- used to pretty print all top-level declarations of a language, but using
    -- 'hang' for @let@ expressions is fine.
    --
    -- Omitted compared to the original:
    --
    -- @
    -- list, tupled, semiBraces
    -- @
  , align
  , hang
  , indent
  , encloseSep

    -- ** Operators
    --
    -- | Omitted compared to the original:
    --
    -- @
    -- (\<$\>), (\<\/\>), (\<$$\>), (\<\/\/\>)
    -- @
  , (<+>)

    -- ** List combinators
  , hsep
  , vsep
  , fillSep
  , sep
  , hcat
  , vcat
  , fillCat
  , cat
  , punctuate

    -- ** Fillers
  , fill
  , fillBreak

    -- ** Bracketing combinators
  , enclose
  , squotes
  , dquotes
  , parens
  , angles
  , braces
  , brackets

    -- ** Character documents
    -- | Entirely omitted:
    --
    -- @
    -- lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket,
    -- squote, dquote, semi, colon, comma, space, dot, backslash, equals,
    -- pipe
    -- @

    -- ** Primitive type documents
    -- | Omitted compared to the original:
    --
    -- @
    -- int, integer, float, double, rational, bool
    -- @
  , string

    -- ** Semantic annotations
  , annotate
  , noAnnotate
  , styleAnn

  -- ** Rendering
  -- | Entirely omitted:
  --
  -- @
  -- SimpleDoc (..), renderPretty, renderCompact, displayDecorated,
  -- displayDecoratedA, display, displayS, displayIO, SpanList (..),
  -- displaySpans
  -- @

  -- ** Undocumented
  -- | Entirely omitted:
  --
  -- @
  -- column, nesting, width
  -- @
  ) where

import           Control.Monad.Reader ( local, runReader )
import           Data.Array.IArray ( (!), (//) )
import qualified Data.Text as T
import           Distribution.ModuleName ( ModuleName )
import           Distribution.System ( Arch (..), OS (..) )
import qualified Distribution.Text ( display )
import           Distribution.Utils.Generic ( lowercase )
import           Path ( Dir, File, Path, SomeBase, prjSomeBase, toFilePath )
import           RIO
import qualified RIO.Map as M
import           RIO.PrettyPrint.DefaultStyles ( defaultStyles )
import           RIO.PrettyPrint.Types ( Style (Dir, File), Styles )
import           RIO.PrettyPrint.StylesUpdate
                   ( HasStylesUpdate, StylesUpdate (..), stylesUpdateL )
import           System.Console.ANSI ( ConsoleLayer (..), SGR (..), setSGRCode )
import qualified Text.PrettyPrint.Annotated.Leijen as P
import           Text.PrettyPrint.Annotated.Leijen ( Doc, SimpleDoc (..) )

-- TODO: consider smashing together the code for wl-annotated-pprint and
-- wl-pprint-text. The code here already handles doing the
-- ansi-wl-pprint stuff (better!) atop wl-annotated-pprint. So the
-- result would be a package unifying 3 different wl inspired packages.
--
-- Perhaps it can still have native string support, by adding a type
-- parameter to Doc?

instance Semigroup StyleDoc where
  StyleDoc x <> StyleDoc y = StyleDoc (x P.<> y)

instance Monoid StyleDoc where
  mappend = (<>)
  mempty = StyleDoc P.empty

--------------------------------------------------------------------------------
-- Pretty-Print class

class Pretty a where
  pretty :: a -> StyleDoc
  default pretty :: Show a => a -> StyleDoc
  pretty = StyleDoc . fromString . show

instance Pretty StyleDoc where
  pretty = id

instance Pretty (Path b File) where
  pretty = styleAnn File . StyleDoc . fromString . toFilePath

instance Pretty (Path b Dir) where
  pretty = styleAnn Dir . StyleDoc . fromString . toFilePath

instance Pretty (SomeBase File) where
  pretty = prjSomeBase pretty

instance Pretty (SomeBase Dir) where
  pretty = prjSomeBase pretty

instance Pretty ModuleName where
  pretty = StyleDoc . fromString . Distribution.Text.display

instance Pretty Arch where
  pretty (OtherArch name) = fromString name
  pretty other = fromString $ lowercase $ show other

instance Pretty OS where
  pretty (OtherOS name) = fromString name
  pretty other = fromString $ lowercase $ show other

--------------------------------------------------------------------------------
-- Style Doc

-- | A style annotation.
newtype StyleAnn = StyleAnn (Maybe Style)
  deriving (Eq, Show, Semigroup)

instance Monoid StyleAnn where
  mempty = StyleAnn Nothing
  mappend = (<>)

-- | A document annotated by a style.
newtype StyleDoc = StyleDoc { unStyleDoc :: Doc StyleAnn }
  deriving (IsString, Show)

-- | An ANSI code(s) annotation.
newtype AnsiAnn = AnsiAnn [SGR]
  deriving (Eq, Show, Semigroup, Monoid)

-- | Convert a 'SimpleDoc' annotated with 'StyleAnn' to one annotated with
-- 'AnsiAnn', by reference to a 'Styles'.
toAnsiDoc :: Styles -> SimpleDoc StyleAnn -> SimpleDoc AnsiAnn
toAnsiDoc styles = go
 where
  go SEmpty        = SEmpty
  go (SChar c d)   = SChar c (go d)
  go (SText l s d) = SText l s (go d)
  go (SLine i d)   = SLine i (go d)
  go (SAnnotStart (StyleAnn (Just s)) d) =
    SAnnotStart (AnsiAnn (snd $ styles ! s)) (go d)
  go (SAnnotStart (StyleAnn Nothing) d) = SAnnotStart (AnsiAnn []) (go d)
  go (SAnnotStop d) = SAnnotStop (go d)

displayPlain ::
     ( Pretty a, HasLogFunc env, HasStylesUpdate env, MonadReader env m
     , HasCallStack
     )
  => Int -> a -> m Utf8Builder
displayPlain w =
  displayAnsiSimple . renderDefault w . fmap (const mempty) . unStyleDoc . pretty

-- TODO: tweak these settings more?
-- TODO: options for settings if this is released as a lib

renderDefault :: Int -> Doc a -> SimpleDoc a
renderDefault = P.renderPretty 1

displayAnsi ::
     ( Pretty a, HasLogFunc env, HasStylesUpdate env, MonadReader env m
     , HasCallStack
     )
  => Int -> a -> m Utf8Builder
displayAnsi w =
  displayAnsiSimple . renderDefault w . unStyleDoc . pretty

{- Not used --------------------------------------------------------------------

hDisplayAnsi
    :: (Display a, HasAnsiAnn (Ann a), MonadIO m)
    => Handle -> Int -> a -> m ()
hDisplayAnsi h w x = liftIO $ do
    useAnsi <- hSupportsANSI h
    T.hPutStr h $ if useAnsi then displayAnsi w x else displayPlain w x

-}

displayAnsiSimple ::
     (HasLogFunc env, HasStylesUpdate env, MonadReader env m, HasCallStack)
  => SimpleDoc StyleAnn
  -> m Utf8Builder
displayAnsiSimple doc = do
  update <- view stylesUpdateL
  let styles = defaultStyles // stylesUpdate update
      doc' = toAnsiDoc styles doc
  return $ flip runReader mempty $ displayDecoratedWrap go doc'
 where
  go (AnsiAnn sgrs) inner = do
    old <- ask
    let sgrs' = mapMaybe (\sgr -> if sgr == Reset
                                    then Nothing
                                    else Just (getSGRTag sgr, sgr)) sgrs
        new = if Reset `elem` sgrs
                then M.fromList sgrs'
                else foldl' (\mp (tag, sgr) -> M.insert tag sgr mp) old sgrs'
    (extra, contents) <- local (const new) inner
    return (extra, transitionCodes old new <> contents <> transitionCodes new old)
  transitionCodes old new =
    case (null removals, null additions) of
      (True, True) -> mempty
      (True, False) -> fromString (setSGRCode additions)
      (False, _) -> fromString (setSGRCode (Reset : M.elems new))
   where
    (removals, additions) = partitionEithers $ M.elems $
      M.mergeWithKey
        (\_ o n -> if o == n then Nothing else Just (Right n))
        (fmap Left)
        (fmap Right)
        old
        new

displayDecoratedWrap ::
     forall a m. Monad m
  => (forall b. a -> m (b, Utf8Builder) -> m (b, Utf8Builder))
  -> SimpleDoc a
  -> m Utf8Builder
displayDecoratedWrap f doc = do
  (mafter, result) <- go doc
  case mafter of
    Just _ -> error "Invariant violated by input to displayDecoratedWrap: no \
                    \matching SAnnotStart for SAnnotStop."
    Nothing -> return result
 where
  spaces n = display (T.replicate n " ")

  go :: SimpleDoc a -> m (Maybe (SimpleDoc a), Utf8Builder)
  go SEmpty = return (Nothing, mempty)
  go (SChar c x) = fmap (fmap (display c <>)) (go x)
  -- NOTE: Could actually use the length to guess at an initial
  -- allocation.  Better yet would be to just use Text in pprint..
  go (SText _l s x) = fmap (fmap (fromString s <>)) (go x)
  go (SLine n x) = fmap (fmap ((display '\n' <>) . (spaces n <>))) (go x)
  go (SAnnotStart ann x) = do
    (mafter, contents) <- f ann (go x)
    case mafter of
      Just after -> fmap (fmap (contents <>)) (go after)
      Nothing -> error "Invariant violated by input to displayDecoratedWrap: \
                       \no matching SAnnotStop for SAnnotStart."
  go (SAnnotStop x) = return (Just x, mempty)

{- Not used --------------------------------------------------------------------

-- Foreground color combinators

black, red, green, yellow, blue, magenta, cyan, white,
    dullblack, dullred, dullgreen, dullyellow, dullblue, dullmagenta, dullcyan, dullwhite,
    onblack, onred, ongreen, onyellow, onblue, onmagenta, oncyan, onwhite,
    ondullblack, ondullred, ondullgreen, ondullyellow, ondullblue, ondullmagenta, ondullcyan, ondullwhite
    :: Doc AnsiAnn -> Doc AnsiAnn
(black, dullblack, onblack, ondullblack) = colorFunctions Black
(red, dullred, onred, ondullred) = colorFunctions Red
(green, dullgreen, ongreen, ondullgreen) = colorFunctions Green
(yellow, dullyellow, onyellow, ondullyellow) = colorFunctions Yellow
(blue, dullblue, onblue, ondullblue) = colorFunctions Blue
(magenta, dullmagenta, onmagenta, ondullmagenta) = colorFunctions Magenta
(cyan, dullcyan, oncyan, ondullcyan) = colorFunctions Cyan
(white, dullwhite, onwhite, ondullwhite) = colorFunctions White

type EndoAnsiDoc = Doc AnsiAnn -> Doc AnsiAnn

colorFunctions :: Color -> (EndoAnsiDoc, EndoAnsiDoc, EndoAnsiDoc, EndoAnsiDoc)
colorFunctions color =
    ( ansiAnn [SetColor Foreground Vivid color]
    , ansiAnn [SetColor Foreground Dull color]
    , ansiAnn [SetColor Background Vivid color]
    , ansiAnn [SetColor Background Dull color]
    )

-}

styleAnn :: Style -> StyleDoc -> StyleDoc
styleAnn s = StyleDoc . P.annotate (StyleAnn (Just s)) . unStyleDoc

{- Not used --------------------------------------------------------------------

-- Intensity combinators

bold, faint, normal :: Doc AnsiAnn -> Doc AnsiAnn
bold = ansiAnn [SetConsoleIntensity BoldIntensity]
faint = ansiAnn [SetConsoleIntensity FaintIntensity]
normal = ansiAnn [SetConsoleIntensity NormalIntensity]

-}

-- | Tags for each field of state in SGR (Select Graphics Rendition).
--
-- It's a bit of a hack that 'TagReset' is included.
data SGRTag
  = TagReset
  | TagConsoleIntensity
  | TagItalicized
  | TagUnderlining
  | TagBlinkSpeed
  | TagVisible
  | TagSwapForegroundBackground
  | TagColorForeground
  | TagColorBackground
  | TagRGBColor
  | TagPaletteColor
  deriving (Eq, Ord)

getSGRTag :: SGR -> SGRTag
getSGRTag Reset{}                       = TagReset
getSGRTag SetConsoleIntensity{}         = TagConsoleIntensity
getSGRTag SetItalicized{}               = TagItalicized
getSGRTag SetUnderlining{}              = TagUnderlining
getSGRTag SetBlinkSpeed{}               = TagBlinkSpeed
getSGRTag SetVisible{}                  = TagVisible
getSGRTag SetSwapForegroundBackground{} = TagSwapForegroundBackground
getSGRTag (SetColor Foreground _ _)     = TagColorForeground
getSGRTag (SetColor Background _ _)     = TagColorBackground
getSGRTag SetRGBColor{}                 = TagRGBColor
getSGRTag SetPaletteColor{}             = TagPaletteColor

-- | The document @(x \<+\> y)@ concatenates document @x@ and @y@ with a
-- @(fromString \"\ \")@ in between. (infixr 6)
(<+>) :: StyleDoc -> StyleDoc -> StyleDoc
StyleDoc x <+> StyleDoc y = StyleDoc (x P.<+> y)

-- | The document @(align x)@ renders document @x@ with the nesting level set to
-- the current column. It is used for example to implement 'hang'.
--
-- As an example, we will put a document right above another one, regardless of
-- the current nesting level:
--
-- > x $$ y = align (x <> line <> y)
--
-- > test = fromString "hi" <+> (fromString "nice" $$ fromString "world")
--
-- which will be layed out as:
--
-- @
-- hi nice
--    world
-- @
align :: StyleDoc -> StyleDoc
align = StyleDoc . P.align . unStyleDoc

-- | Strip annotations from a document. This is useful for re-using the textual
-- formatting of some sub-document, but applying a different high-level
-- annotation.
noAnnotate :: StyleDoc -> StyleDoc
noAnnotate = StyleDoc . P.noAnnotate . unStyleDoc

-- | Document @(braces x)@ encloses document @x@ in braces, \"{\" and \"}\".
braces :: StyleDoc -> StyleDoc
braces = StyleDoc . P.braces . unStyleDoc

-- | Document @(angles x)@ encloses document @x@ in angles, \"\<\" and \"\>\".
angles :: StyleDoc -> StyleDoc
angles = StyleDoc . P.angles . unStyleDoc

-- | Document @(parens x)@ encloses document @x@ in parenthesis, \"(\" and
-- \")\".
parens :: StyleDoc -> StyleDoc
parens = StyleDoc . P.parens . unStyleDoc

-- | Document @(dquotes x)@ encloses document @x@ with double quotes '\"'.
dquotes :: StyleDoc -> StyleDoc
dquotes = StyleDoc . P.dquotes . unStyleDoc

-- | Document @(squotes x)@ encloses document @x@ with single quotes \"'\".
squotes :: StyleDoc -> StyleDoc
squotes = StyleDoc . P.squotes . unStyleDoc

-- | Document @(brackets x)@ encloses document @x@ in square brackets, \"[\" and
-- \"]\".
brackets :: StyleDoc -> StyleDoc
brackets = StyleDoc . P.brackets . unStyleDoc

-- | The document @string s@ concatenates all characters in @s@ using @line@ for
-- newline characters and @fromString@ for all other characters. It is used
-- whenever the text contains newline characters.
--
-- @since 0.1.4.0
string :: String -> StyleDoc
string "" = mempty
string ('\n':s) = line <> string s
string s        = let (xs, ys) = span (/='\n') s
                  in  fromString xs <> string ys

annotate :: StyleAnn -> StyleDoc -> StyleDoc
annotate a = StyleDoc . P.annotate a . unStyleDoc

-- | The document @(nest i x)@ renders document @x@ with the current indentation
-- level increased by i (See also 'hang', 'align' and 'indent').
--
-- >    nest 2 (fromString "hello" <> line <> fromString "world")
-- > <> line
-- > <> fromString "!"
--
-- outputs as:
--
-- @
-- hello
--   world
-- !
-- @
nest :: Int -> StyleDoc -> StyleDoc
nest a = StyleDoc . P.nest a . unStyleDoc

-- | The @line@ document advances to the next line and indents to the current
-- nesting level. Document @line@ behaves like @(fromString \" \")@ if the line
-- break is undone by 'group'.
line :: StyleDoc
line = StyleDoc P.line

-- | The @linebreak@ document advances to the next line and indents to the
-- current nesting level. Document @linebreak@ behaves like 'mempty' if the line
-- break is undone by 'group'.
linebreak :: StyleDoc
linebreak = StyleDoc P.linebreak

-- | The document @(fill i x)@ renders document @x@. It than appends
-- @(fromString \"\ \")@s until the width is equal to @i@. If the width of @x@
-- is already larger, nothing is appended. This combinator is quite useful in
-- practice to output a list of bindings. The following example demonstrates
-- this.
--
-- > types = [ ("empty", "Doc a")
-- >         , ("nest", "Int -> Doc a -> Doc a")
-- >         , ("linebreak", "Doc a")
-- >         ]
-- >
-- > ptype (name, tp) =
-- >   fill 6 (fromString name) <+> fromString "::" <+> fromString tp
-- >
-- > test = fromString "let" <+> align (vcat (map ptype types))
--
-- Which is layed out as:
--
-- @
-- let empty  :: Doc a
--     nest   :: Int -> Doc a -> Doc a
--     linebreak :: Doc a
-- @
fill :: Int -> StyleDoc -> StyleDoc
fill a = StyleDoc . P.fill a . unStyleDoc

-- | The document @(fillBreak i x)@ first renders document @x@. It then appends
-- @(fromString \"\ \")@s until the width is equal to @i@. If the width of @x@
-- is already larger than @i@, the nesting level is increased by @i@ and a
-- @line@ is appended. When we redefine @ptype@ in the previous example to use
-- @fillBreak@, we get a useful variation of the previous output:
--
-- > ptype (name, tp) =
-- >   fillBreak 6 (fromString name) <+> fromString "::" <+> fromString tp
--
-- The output will now be:
--
-- @
-- let empty  :: Doc a
--     nest   :: Int -> Doc a -> Doc a
--     linebreak
--            :: Doc a
-- @
fillBreak :: Int -> StyleDoc -> StyleDoc
fillBreak a = StyleDoc . P.fillBreak a . unStyleDoc

-- | The document @(enclose l r x)@ encloses document @x@ between documents @l@
-- and @r@ using @(\<\>)@.
--
-- > enclose l r x   = l <> x <> r
enclose :: StyleDoc -> StyleDoc -> StyleDoc -> StyleDoc
enclose l r x = l <> x <> r

-- | The document @(cat xs)@ concatenates all documents @xs@ either
-- horizontally with @(\<\>)@, if it fits the page, or vertically with
-- @(\<\> linebreak \<\>)@.
--
-- > cat xs = group (vcat xs)
cat :: [StyleDoc] -> StyleDoc
cat = StyleDoc . P.cat . map unStyleDoc

-- | @(punctuate p xs)@ concatenates all documents in @xs@ with document @p@
-- except for the last document.
--
-- > someText = map fromString ["words", "in", "a", "tuple"]
-- > test = parens (align (cat (punctuate comma someText)))
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
-- (If you want put the commas in front of their elements instead of at the end,
-- you should use 'encloseSep'.)
punctuate :: StyleDoc -> [StyleDoc] -> [StyleDoc]
punctuate (StyleDoc x) = map StyleDoc . P.punctuate x . map unStyleDoc

-- | The document @(fillCat xs)@ concatenates documents @xs@ horizontally with
-- @(\<\>)@ as long as its fits the page, than inserts a @linebreak@ and
-- continues doing that for all documents in @xs@.
--
-- > fillCat xs = foldr (<> softbreak <>) mempty xs
fillCat :: [StyleDoc] -> StyleDoc
fillCat = StyleDoc . P.fillCat . map unStyleDoc

-- | The document @(hcat xs)@ concatenates all documents @xs@ horizontally with
-- @(\<\>)@.
hcat :: [StyleDoc] -> StyleDoc
hcat = StyleDoc . P.hcat . map unStyleDoc

-- | The document @(vcat xs)@ concatenates all documents @xs@ vertically with
-- @(\<\> linebreak \<\>)@. If a 'group' undoes the line breaks inserted by
-- 'vcat', all documents are directly concatenated.
vcat :: [StyleDoc] -> StyleDoc
vcat = StyleDoc . P.vcat . map unStyleDoc

-- | The document @(sep xs)@ concatenates all documents @xs@ either horizontally
-- with @(\<+\>)@, if it fits the page, or vertically with @(\<\> line \<\>)@.
--
-- > sep xs = group (vsep xs)
sep :: [StyleDoc] -> StyleDoc
sep = StyleDoc . P.sep . map unStyleDoc

-- | The document @(vsep xs)@ concatenates all documents @xs@ vertically with
-- @(\<\> line \<\>)@. If a 'group' undoes the line breaks inserted by 'vsep',
-- all documents are separated with a space.
--
-- > someText = map fromString (words ("text to lay out"))
-- >
-- > test = fromString "some" <+> vsep someText
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
-- The 'align' combinator can be used to align the documents under their first
-- element
--
-- > test = fromString "some" <+> align (vsep someText)
--
-- Which is printed as:
--
-- @
-- some text
--      to
--      lay
--      out
-- @
vsep :: [StyleDoc] -> StyleDoc
vsep = StyleDoc . P.vsep . map unStyleDoc

-- | The document @(hsep xs)@ concatenates all documents @xs@ horizontally with
-- @('<+>')@.
hsep :: [StyleDoc] -> StyleDoc
hsep = StyleDoc . P.hsep . map unStyleDoc

-- | The document @(fillSep xs)@ concatenates documents @xs@ horizontally with
-- @('<+>')@ as long as its fits the page, than inserts a 'line' and continues
-- doing that for all documents in @xs@.
--
-- > fillSep xs = foldr (<> softline <>) mempty xs
fillSep :: [StyleDoc] -> StyleDoc
fillSep = StyleDoc . P.fillSep . map unStyleDoc

-- | The document @(encloseSep l r sep xs)@ concatenates the documents @xs@
-- separated by @sep@ and encloses the resulting document by @l@ and @r@. The
-- documents are rendered horizontally if that fits the page. Otherwise they are
-- aligned vertically. All separators are put in front of the elements. For
-- example, the combinator 'list' can be defined with 'encloseSep':
--
-- > list xs = encloseSep lbracket rbracket comma xs
-- > test = fromString "list" <+> (list (map int [10, 200, 3000]))
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
encloseSep :: StyleDoc -> StyleDoc -> StyleDoc -> [StyleDoc] -> StyleDoc
encloseSep (StyleDoc x) (StyleDoc y) (StyleDoc z) =
  StyleDoc . P.encloseSep x y z . map unStyleDoc

-- | The document @(indent i x)@ indents document @x@ with @i@ spaces.
--
-- > test = indent 4 (fillSep (map fromString
-- >        (words "the indent combinator indents these words !")))
--
-- Which lays out with a page width of 20 as:
--
-- @
--     the indent
--     combinator
--     indents these
--     words !
-- @
indent :: Int -> StyleDoc -> StyleDoc
indent a = StyleDoc . P.indent a . unStyleDoc

-- | The hang combinator implements hanging indentation. The document
-- @(hang i x)@ renders document @x@ with a nesting level set to the current
-- column plus @i@. The following example uses hanging indentation for some
-- text:
--
-- > test = hang 4 (fillSep (map fromString
-- >        (words "the hang combinator indents these words !")))
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
-- > hang i x = align (nest i x)
hang :: Int -> StyleDoc -> StyleDoc
hang a = StyleDoc . P.hang a . unStyleDoc

-- | The document @softbreak@ behaves like 'mempty' if the resulting output fits
-- the page, otherwise it behaves like 'line'.
--
-- > softbreak = group linebreak
softbreak :: StyleDoc
softbreak = StyleDoc P.softbreak

-- | The document @softline@ behaves like @(fromString \"\ \")@ if the resulting
-- output fits the page, otherwise it behaves like 'line'.
--
-- > softline = group line
softline :: StyleDoc
softline = StyleDoc P.softline

-- | The @group@ combinator is used to specify alternative layouts. The document
-- @(group x)@ undoes all line breaks in document @x@. The resulting line is
-- added to the current line if that fits the page. Otherwise, the document @x@
-- is rendered without any changes.
group :: StyleDoc -> StyleDoc
group = StyleDoc . P.group . unStyleDoc
