-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  stable
--   Portability :  portable
--
--   This module provides bencode values serialization. Normally, you
--   don't need to import this module, use 'Data.BEncode' instead.
--
{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}
module Data.BEncode.Internal
       ( -- * Parsing
         parser
       , parse

         -- * Rendering
       , builder
       , build
       , ppBEncode
       ) where

import Control.Applicative
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString as B
import           Data.ByteString.Internal as B (c2w, w2c)
import qualified Data.ByteString.Lazy as Lazy
import Data.List as L
import Text.PrettyPrint hiding ((<>))

#if MIN_VERSION_bytestring(0, 10, 12)
import qualified Data.ByteString.Builder as B
#else
import qualified Data.ByteString.Lazy.Builder as B
#endif

#if __GLASGOW_HASKELL__ < 710
import Data.Foldable
import Data.Monoid (Monoid (mappend))
#endif

import Data.BEncode.Types
import Data.BEncode.BDict as BD

import GHC.Types

#if MIN_VERSION_integer_gmp(1, 1, 0)
import GHC.Num.Integer
#else
import GHC.Integer.GMP.Internals
#endif

{--------------------------------------------------------------------
-- Serialization
--------------------------------------------------------------------}

integerDecimal :: Integer -> B.Builder
#if MIN_VERSION_integer_gmp(1, 1, 0)
integerDecimal (IS i#) = B.intDec (I# i#)
#else
integerDecimal (S# i#) = B.intDec (I# i#)
#endif
integerDecimal  i      = B.string7 (show i) -- TODO more efficient

-- | BEncode format encoder according to specification.
builder :: BValue -> B.Builder
builder = go
    where
      go (BInteger i) = B.word8 (c2w 'i') `mappend`
                          integerDecimal i `mappend`
                        B.word8 (c2w 'e')
      go (BString  s) = buildString s
      go (BList    l) = B.word8 (c2w 'l') `mappend`
                        foldMap go l `mappend`
                        B.word8 (c2w 'e')
      go (BDict    d) = B.word8 (c2w 'd') `mappend`
                        foldMapWithKey mkKV d `mappend`
                        B.word8 (c2w 'e')
          where
            mkKV k v = buildString k `mappend` go v

      buildString s = B.intDec (B.length s) `mappend`
                      B.word8 (c2w ':') `mappend`
                      B.byteString s
      {-# INLINE buildString #-}

-- | Convert bencoded value to raw bytestring according to the
-- specification.
build :: BValue -> Lazy.ByteString
build = B.toLazyByteString . builder

{--------------------------------------------------------------------
-- Deserialization
--------------------------------------------------------------------}

-- TODO try to replace peekChar with something else
-- | BEncode format parser according to specification.
parser :: Parser BValue
parser = valueP
  where
    valueP = do
      mc <- P.peekChar
      case mc of
        Nothing -> fail "end of input"
        Just c  ->
            case c of
              -- if we have digit it always should be string length
              di | di <= '9' -> BString <$> stringP
              'i' -> P.anyChar *> ((BInteger <$> integerP)  <* P.anyChar)
              'l' -> P.anyChar *> ((BList    <$> listBodyP) <* P.anyChar)
              'd' -> P.anyChar *>  (BDict    <$> dictBodyP) <* P.anyChar
              t   -> fail ("bencode unknown tag: " ++ [t])

    dictBodyP :: Parser BDict
    dictBodyP = Cons <$> stringP <*> valueP <*> dictBodyP
            <|> pure Nil

    listBodyP = do
      c <- P.peekChar
      case c of
        Just 'e' -> return []
        _        -> (:) <$> valueP <*> listBodyP

    stringP :: Parser ByteString
    stringP = do
      n <- P.decimal :: Parser Int
      _ <- P.char ':'
      P.take n
    {-# INLINE stringP #-}

    integerP :: Parser Integer
    integerP = do
      c <- P.peekChar
      case c of
        Just '-' -> do
          _ <- P.anyChar
          negate <$> P.decimal
        _        ->  P.decimal
    {-# INLINE integerP #-}

-- | Try to convert raw bytestring to bencoded value according to
-- specification.
parse :: ByteString -> Either String BValue
parse = P.parseOnly parser

{--------------------------------------------------------------------
  Pretty Printing
--------------------------------------------------------------------}

ppBS :: ByteString -> Doc
ppBS = text . L.map w2c . B.unpack

-- | Convert to easily readable JSON-like document. Typically used for
-- debugging purposes.
ppBEncode :: BValue -> Doc
ppBEncode (BInteger i) = int $ fromIntegral i
ppBEncode (BString  s) = ppBS s
ppBEncode (BList    l)
    = brackets $ hsep $ punctuate comma $ L.map ppBEncode l
ppBEncode (BDict    d)
    = braces $ vcat $ punctuate comma $ L.map ppKV $ BD.toAscList d
  where
    ppKV (k, v) = ppBS k <+> colon <+> ppBEncode v
