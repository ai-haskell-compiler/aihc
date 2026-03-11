{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Monoids with a homomorphism from 'String' to themselves.
module Text.Printer
  (
  -- * The class
    Printer(..)
  -- * Builders
  , StringBuilder(..)
  , buildString
  , buildText
  , buildLazyText
  , AsciiBuilder(..)
  , buildAscii
  , buildLazyAscii
  , Utf8Builder(..)
  , buildUtf8
  , buildLazyUtf8
  , PrettyPrinter(..)
  , renderPretty
  -- * Combinators
  , (<>)
  , hcat
  , fcat
  , separate
  , (<+>)
  , hsep
  , fsep
  , list
  , parens
  , brackets
  , braces
  , angles
  , squotes
  , dquotes
  , punctuateL
  , punctuateR
  -- * Multiline printers
  , MultilinePrinter(..)
  , lines
  , newLine
  , crlf
  , LinePrinter(..)
  , lfPrinter
  , crlfPrinter
  ) where

import Prelude hiding (foldr, foldr1, print, lines)
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic)
#endif
import Data.Typeable (Typeable)
import Data.String (IsString(..))
import Data.Semigroup (Semigroup)
import qualified Data.Semigroup as S
import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#endif
import Data.Foldable (Foldable(..), toList)
import Data.Traversable (Traversable, mapAccumL, mapAccumR)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
#if MIN_VERSION_bytestring(0,10,2)
import qualified Data.ByteString.Builder as BB
#else
import qualified Data.ByteString.Lazy.Builder as BB
#endif
import qualified Text.PrettyPrint as PP

-- | Text monoid. 'string' must be equivalent to 'fromString' and be a monoid
--   homomorphism, i.e. @'string' 'mempty' = 'mempty'@ and
--   @'mappend' ('string' /x/) ('string' /y/) = 'string' ('mappend' /x/ /y/)@.
--   Other operations must be monoid homomorphisms that are eqiuvalent (but
--   possibly faster) to the composition of 'string' and the corresponding
--   embedding, e.g. @'text' = 'string' . 'TS.unpack'@.
class (IsString p, Semigroup p, Monoid p) ⇒ Printer p where
  -- | Print a character. @'char' /c/@ must be equivalent to
  --   @'string' [/c/]@, but hopefully is faster.
  char ∷ Char → p
  char c = string [c]
  {-# INLINE char #-}
  -- | Print an ASCII character, can be faster than 'char'.
  char7 ∷ Char → p
  char7 = char
  {-# INLINE char7 #-}
  -- | Print a string.
  string ∷ String → p
  string = fromString
  {-# INLINE string #-}
  -- | Print an ASCII string, can be faster than 'string'.
  string7 ∷ String → p
  string7 = string
  {-# INLINE string7 #-}
  -- | Print a 'TS.Text'.
  text ∷ TS.Text → p
  text = string . TS.unpack
  {-# INLINE text #-}
  -- | Print a lazy 'TL.Text'.
  lazyText ∷ TL.Text → p
  lazyText = string . TL.unpack
  {-# INLINE lazyText #-}
  -- | Print an ASCII 'BS.ByteString'.
  ascii ∷ BS.ByteString → p
  ascii = string . BS8.unpack
  {-# INLINE ascii #-}
  -- | Print a lazy ASCII 'BL.ByteString'.
  lazyAscii ∷ BL.ByteString → p
  lazyAscii = string . BL8.unpack
  {-# INLINE lazyAscii #-}
  -- | Print a UTF-8 'BS.ByteString'.
  utf8 ∷ BS.ByteString → p
  utf8 = text . TS.decodeUtf8
  {-# INLINE utf8 #-}
  -- | Print a lazy UTF-8 'BL.ByteString'
  lazyUtf8 ∷ BL.ByteString → p
  lazyUtf8 = lazyText . TL.decodeUtf8
  {-# INLINE lazyUtf8 #-}

instance Printer String where

-- | A simple string builder as used by 'Show'.
newtype StringBuilder = StringBuilder { stringBuilder ∷ String → String }
                        deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                                 , Generic
#endif
                                 , Semigroup
                                 , Monoid)

instance IsString StringBuilder where
  fromString s = StringBuilder (s ++)
  {-# INLINE fromString #-}

instance Printer StringBuilder where
  char c = StringBuilder (c :)
  {-# INLINE char #-}

buildString ∷ StringBuilder → String
buildString b = stringBuilder b ""
{-# INLINE buildString #-}

instance Printer TB.Builder where
  char = TB.singleton
  {-# INLINE char #-}
  text = TB.fromText
  {-# INLINE text #-}
  lazyText = TB.fromLazyText
  {-# INLINE lazyText #-}

buildText ∷ TB.Builder → TS.Text
buildText = fold . TL.toChunks . buildLazyText
{-# INLINE buildText #-}

buildLazyText ∷ TB.Builder → TL.Text
buildLazyText = TB.toLazyText
{-# INLINE buildLazyText #-}

-- | Use this builder when you are sure that only ASCII characters
--   will get printed to it.
newtype AsciiBuilder = AsciiBuilder { asciiBuilder ∷ BB.Builder }
                       deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                                , Generic
#endif
                                , Monoid)

instance IsString AsciiBuilder where
  fromString = AsciiBuilder . BB.string7
  {-# INLINE fromString #-}

instance Semigroup AsciiBuilder where
  b₁ <> b₂ = AsciiBuilder $ asciiBuilder b₁ <> asciiBuilder b₂
  {-# INLINE (<>) #-}
  stimes = S.stimesMonoid
  {-# INLINE stimes #-}

instance Printer AsciiBuilder where
  char = AsciiBuilder . BB.char7
  {-# INLINE char #-}
  ascii = AsciiBuilder . BB.byteString
  {-# INLINE ascii #-}
  lazyAscii = AsciiBuilder . BB.lazyByteString
  {-# INLINE lazyAscii #-}
  utf8 = AsciiBuilder . BB.byteString
  {-# INLINE utf8 #-}
  lazyUtf8 = AsciiBuilder . BB.lazyByteString
  {-# INLINE lazyUtf8 #-}

buildAscii ∷ AsciiBuilder → BS.ByteString
buildAscii = fold . BL.toChunks . buildLazyAscii
{-# INLINE buildAscii #-}

buildLazyAscii ∷ AsciiBuilder → BL.ByteString
buildLazyAscii = BB.toLazyByteString . asciiBuilder
{-# INLINE buildLazyAscii #-}

-- | UTF-8 lazy 'BL.ByteString' builder.
newtype Utf8Builder = Utf8Builder { utf8Builder ∷ BB.Builder }
                      deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                               , Generic
#endif
                               , Monoid)

instance IsString Utf8Builder where
  fromString = Utf8Builder . BB.stringUtf8
  {-# INLINE fromString #-}

instance Semigroup Utf8Builder where
  b₁ <> b₂ = Utf8Builder $ utf8Builder b₁ <> utf8Builder b₂
  {-# INLINE (<>) #-}
  stimes = S.stimesMonoid
  {-# INLINE stimes #-}

instance Printer Utf8Builder where
  char = Utf8Builder . BB.charUtf8
  {-# INLINE char #-}
  char7 = Utf8Builder . BB.char7
  {-# INLINE char7 #-}
  string7 = Utf8Builder . BB.string7
  {-# INLINE string7 #-}
  text = Utf8Builder . BB.byteString . TS.encodeUtf8
  {-# INLINE text #-}
  lazyText = Utf8Builder . BB.lazyByteString . TL.encodeUtf8
  {-# INLINE lazyText #-}
  ascii = Utf8Builder . BB.byteString
  {-# INLINE ascii #-}
  lazyAscii = Utf8Builder . BB.lazyByteString
  {-# INLINE lazyAscii #-}
  utf8 = Utf8Builder . BB.byteString
  {-# INLINE utf8 #-}
  lazyUtf8 = Utf8Builder . BB.lazyByteString
  {-# INLINE lazyUtf8 #-}

buildUtf8 ∷ Utf8Builder → BS.ByteString
buildUtf8 = fold . BL.toChunks . buildLazyUtf8
{-# INLINE buildUtf8 #-}

buildLazyUtf8 ∷ Utf8Builder → BL.ByteString
buildLazyUtf8 = BB.toLazyByteString . utf8Builder
{-# INLINE buildLazyUtf8 #-}

newtype PrettyPrinter = PrettyPrinter { prettyPrinter ∷ PP.Doc }
                        deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                                 , Generic
#endif
#if MIN_VERSION_pretty(1,1,0)
                                 , IsString
# if MIN_VERSION_base(4,9,0)
                                 , Semigroup
# endif
                                 , Monoid
#endif
                                 )

#if !MIN_VERSION_pretty(1,1,0)
instance IsString PrettyPrinter where
  fromString = PrettyPrinter . PP.text
  {-# INLINE fromString #-}
#endif

#if !MIN_VERSION_base(4,9,0) || !MIN_VERSION_pretty(1,1,0)
instance Semigroup PrettyPrinter where
  p₁ <> p₂ = PrettyPrinter
           $ (PP.<>) (prettyPrinter p₁) (prettyPrinter p₂)
  {-# INLINE (<>) #-}
  stimes = S.stimesMonoid
  {-# INLINE stimes #-}
#endif

#if !MIN_VERSION_pretty(1,1,0)
instance Monoid PrettyPrinter where
  mempty = PP.empty
  {-# INLINE mempty #-}
  mappend = (S.<>)
  {-# INLINE mappend #-}
#endif

instance Printer PrettyPrinter where
  char = PrettyPrinter . PP.char
  {-# INLINE char #-}

-- | An alias for @'PP.render' . 'prettyPrinter'@
renderPretty ∷ PrettyPrinter → String
renderPretty = PP.render . prettyPrinter

#if !MIN_VERSION_base(4,5,0)
-- | An infix synonym for 'mappend'.
(<>) ∷ Monoid m ⇒ m → m → m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- | 'mconcat' for 'Foldable' data structures.
hcat ∷ (Printer p, Foldable f) ⇒ f p → p
hcat = fold
{-# INLINE hcat #-}

-- | Combine the items of a 'Foldable' data structure using the provided
--   function. If the data structure is empty, 'mempty' is returned.
fcat ∷ (Foldable f, Printer p) ⇒ (p → p → p) → f p → p
fcat c f = case toList f of
  [] → mempty
  ps → foldr1 c ps
{-# INLINABLE fcat #-}

-- | Concatenate two 'Printer's with a separator between them.
separate ∷ Printer p
         ⇒ p -- ^ The separator
         → p → p → p
separate s x y = x <> s <> y
{-# INLINE separate #-}

infixr 6 <+>

-- | Concatenate two 'Printer's with a space between them.
(<+>) ∷ Printer p ⇒ p → p → p
(<+>) = separate (char7 ' ')
{-# INLINE (<+>) #-}

-- | Concatenate the items of a 'Foldable' data structure
--   with spaces between them.
hsep ∷ (Printer p, Foldable f) ⇒ f p → p
hsep = fcat (<+>)
{-# INLINE hsep #-}

-- | A shorthand for @'fcat' . 'separate'@.
fsep ∷ (Foldable f, Printer p) ⇒ p → f p → p
fsep = fcat . separate
{-# INLINE fsep #-}

-- | Concatenate the items of a 'Foldable' data structure with commas
--   between them.
--
-- @
--   'list' = 'fsep' ('char7' ',')
-- @
list ∷ (Foldable f, Printer p) ⇒ f p → p
list = fsep (char7 ',')
{-# INLINE list #-}

-- | Enclose a 'Printer' with parentheses.
parens ∷ Printer p ⇒ p → p
parens p = char7 '(' <> p <> char7 ')'
{-# INLINE parens #-}

-- | Enclose a 'Printer' with square brackets.
brackets ∷ Printer p ⇒ p → p
brackets p = char7 '[' <> p <> char7 ']'
{-# INLINE brackets #-}

-- | Enclose a 'Printer' with curly braces.
braces ∷ Printer p ⇒ p → p
braces p = char7 '{' <> p <> char7 '}'
{-# INLINE braces #-}

-- | Enclose a 'Printer' with angle brackets.
angles ∷ Printer p ⇒ p → p
angles p = char7 '<' <> p <> char7 '>'
{-# INLINE angles #-}

-- | Enclose a 'Printer' with single quotes.
squotes ∷ Printer p ⇒ p → p
squotes p = char7 '\'' <> p <> char7 '\''
{-# INLINE squotes #-}

-- | Enclose a 'Printer' with double quotes.
dquotes ∷ Printer p ⇒ p → p
dquotes p  = char7 '\"' <> p <> char7 '\"'
{-# INLINE dquotes #-}

-- | Prepend all but the first element of a 'Traversable' with the
--   provided value, e.g.
--   @'punctuateL' /p/ [/x1/, /x2/, ..., /xN/] =
--      [/x1/, /p/ '<>' /x2/, ..., /p/ '<>' /xN/]@
punctuateL ∷ (Traversable t, Printer p) ⇒ p → t p → t p
punctuateL p =
  snd . mapAccumL (\f a → if f then (False, a) else (False, p <> a)) True
{-# INLINE punctuateL #-}

-- | Append the provided value to all but the last element of a 'Traversable',
--   e.g. @'punctuateR' /p/ [/x1/, ..., /xN-1/, /xN/] =
--           [/x1/ '<>' /p/, ..., /xN-1/ '<>' /p/, /xN/]@
punctuateR ∷ (Traversable t, Printer p) ⇒ p → t p → t p
punctuateR p =
  snd . mapAccumR (\l a → if l then (False, a) else (False, a <> p)) True
{-# INLINE punctuateR #-}

infixr 5 <->

-- | Printers that can produce multiple lines of text.
class Printer p ⇒ MultilinePrinter p where
  -- | Combine two lines. Must be associative, i.e.
  --   /x/ '<->' (/y/ '<->' /z/) = (/x/ '<->' /y/) '<->' /z/.
  (<->) ∷ p → p → p

instance MultilinePrinter PrettyPrinter where
  p₁ <-> p₂ = PrettyPrinter 
            $ (PP.$+$) (prettyPrinter p₁) (prettyPrinter p₂)
  {-# INLINE (<->) #-}

-- | Combine the items of a 'Foldable' data structure with '<->'.
lines ∷ (MultilinePrinter p, Foldable f) ⇒ f p → p
lines = fcat (<->)
{-# INLINE lines #-}

-- | Print the LF character (/'\n'/).
newLine ∷ Printer p ⇒ p
newLine = char '\n'
{-# INLINE newLine #-}

-- | Print CR (/'\r'/) followed by LF (/'\n'/).
crlf ∷ Printer p ⇒ p
crlf = char '\r' <> char '\n'
{-# INLINE crlf #-}

-- | A multiline printer that combines lines with the provided function.
newtype LinePrinter p = LinePrinter { linePrinter ∷ (p → p → p) → p }
                        deriving ( Typeable
#if __GLASGOW_HASKELL__ >= 706
                                 , Generic
#endif
                                 )

instance IsString p ⇒ IsString (LinePrinter p) where
  fromString = LinePrinter . const . fromString
  {-# INLINE fromString #-}

instance Semigroup p ⇒ Semigroup (LinePrinter p) where
  x <> y = LinePrinter $ \l → linePrinter x l S.<> linePrinter y l
  {-# INLINE (<>) #-}
  stimes n x = LinePrinter $ S.stimes n . linePrinter x
  {-# INLINE stimes #-}

instance Monoid p ⇒ Monoid (LinePrinter p) where
  mempty = LinePrinter $ const mempty
  {-# INLINE mempty #-}
  mappend x y = LinePrinter $ \l → mappend (linePrinter x l) (linePrinter y l)
  {-# INLINE mappend #-}
  mconcat xs = LinePrinter $ \l → mconcat (map (\x → linePrinter x l) xs)
  {-# INLINE mconcat #-}

instance Printer p ⇒ Printer (LinePrinter p) where
  char = LinePrinter . const . char
  {-# INLINE char #-}
  char7 = LinePrinter . const . char7
  {-# INLINE char7 #-}
  string = LinePrinter . const . string
  {-# INLINE string #-}
  string7 = LinePrinter . const . string7
  {-# INLINE string7 #-}
  text = LinePrinter . const . text
  {-# INLINE text #-}
  lazyText = LinePrinter . const . lazyText
  {-# INLINE lazyText #-}
  ascii = LinePrinter . const . ascii
  {-# INLINE ascii #-}
  lazyAscii = LinePrinter . const . lazyAscii
  {-# INLINE lazyAscii #-}
  utf8 = LinePrinter . const . utf8
  {-# INLINE utf8 #-}
  lazyUtf8 = LinePrinter . const . lazyUtf8
  {-# INLINE lazyUtf8 #-}
  
instance Printer p ⇒ MultilinePrinter (LinePrinter p) where
  x <-> y = LinePrinter $ \l → l (linePrinter x l) (linePrinter y l)
  {-# INLINE (<->) #-}

-- | Separate lines with 'newLine'.
lfPrinter ∷ Printer p ⇒ LinePrinter p → p
lfPrinter p = linePrinter p (separate newLine)
{-# INLINE lfPrinter #-}

-- | Separate lines with 'crlf'.
crlfPrinter ∷ Printer p ⇒ LinePrinter p → p
crlfPrinter p = linePrinter p (separate crlf)
{-# INLINE crlfPrinter #-}
