----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.MetaDoc
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

module Prettyprinter.MetaDoc
  ( DocKind(..)
  , MetaDoc
  , mdPayload
  , mdKind
  , compositeMetaDoc
  , atomicMetaDoc

  , metaDocInt
  , metaDocFloat
  , metaDocDouble
  , metaDocInteger
  , metaDocNatural
  , metaDocWord
  , metaDocWord8
  , metaDocWord16
  , metaDocWord32
  , metaDocWord64
  , metaDocInt8
  , metaDocInt16
  , metaDocInt32
  , metaDocInt64
  , metaDocUnit
  , metaDocBool
  , metaDocChar

  , stringMetaDoc
  , strictTextMetaDoc
  , lazyTextMetaDoc
  , strictByteStringMetaDoc
  , lazyByteStringMetaDoc
  , shortByteStringMetaDoc

  , constructorAppMetaDoc
  ) where

import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as CL8
import Data.ByteString.Short qualified as ShortBS
import Data.Int
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Word
import Numeric.Natural
import Prettyprinter
import Prettyprinter qualified as PP
import Prettyprinter.Combinators.Basic

data DocKind = Atomic | Composite
  deriving (Eq, Ord, Enum, Bounded)

instance Semigroup DocKind where
  (<>) = max

instance Monoid DocKind where
  mempty = minBound

data MetaDoc ann = MetaDoc
  { mdPayload :: Doc ann
  , mdKind    :: DocKind
  }

compositeMetaDoc :: Doc ann -> MetaDoc ann
compositeMetaDoc x = MetaDoc
  { mdPayload = x
  , mdKind    = Composite
  }

atomicMetaDoc :: Doc ann -> MetaDoc ann
atomicMetaDoc x = MetaDoc
  { mdPayload = x
  , mdKind    = Atomic
  }

instance Semigroup (MetaDoc ann) where
  (<>) (MetaDoc p1 kind1) (MetaDoc p2 kind2) = MetaDoc
    { mdPayload = p1 <> p2
    , mdKind    = kind1 <> kind2
    }

instance Monoid (MetaDoc ann) where
  mempty = MetaDoc
    { mdPayload = mempty
    , mdKind    = mempty
    }
  mappend = (<>)

metaDocInt :: Int -> MetaDoc ann
metaDocInt = atomicMetaDoc . pretty

metaDocFloat :: Float -> MetaDoc ann
metaDocFloat = atomicMetaDoc . pretty

metaDocDouble :: Double -> MetaDoc ann
metaDocDouble = atomicMetaDoc . pretty

metaDocInteger :: Integer -> MetaDoc ann
metaDocInteger = atomicMetaDoc . pretty

metaDocNatural :: Natural -> MetaDoc ann
metaDocNatural = atomicMetaDoc . pretty

metaDocWord :: Word -> MetaDoc ann
metaDocWord = atomicMetaDoc . pretty

metaDocWord8 :: Word8 -> MetaDoc ann
metaDocWord8 = atomicMetaDoc . pretty

metaDocWord16 :: Word16 -> MetaDoc ann
metaDocWord16 = atomicMetaDoc . pretty

metaDocWord32 :: Word32 -> MetaDoc ann
metaDocWord32 = atomicMetaDoc . pretty

metaDocWord64 :: Word64 -> MetaDoc ann
metaDocWord64 = atomicMetaDoc . pretty

metaDocInt8 :: Int8 -> MetaDoc ann
metaDocInt8 = atomicMetaDoc . pretty

metaDocInt16 :: Int16 -> MetaDoc ann
metaDocInt16 = atomicMetaDoc . pretty

metaDocInt32 :: Int32 -> MetaDoc ann
metaDocInt32 = atomicMetaDoc . pretty

metaDocInt64 :: Int64 -> MetaDoc ann
metaDocInt64 = atomicMetaDoc . pretty

metaDocUnit :: () -> MetaDoc ann
metaDocUnit = atomicMetaDoc . pretty

metaDocBool :: Bool -> MetaDoc ann
metaDocBool = atomicMetaDoc . pretty

metaDocChar :: Char -> MetaDoc ann
metaDocChar = atomicMetaDoc . pretty


stringMetaDoc :: String -> MetaDoc ann
stringMetaDoc str = f $ pretty str
  where
    f | any (== ' ') str = compositeMetaDoc
      | otherwise        = atomicMetaDoc

strictTextMetaDoc :: T.Text -> MetaDoc ann
strictTextMetaDoc str = f $ pretty str
  where
    f | T.any (== ' ') str = compositeMetaDoc
      | otherwise          = atomicMetaDoc

lazyTextMetaDoc :: TL.Text -> MetaDoc ann
lazyTextMetaDoc str = f $ pretty str
  where
    f | TL.any (== ' ') str = compositeMetaDoc
      | otherwise           = atomicMetaDoc

strictByteStringMetaDoc :: C8.ByteString -> MetaDoc ann
strictByteStringMetaDoc str = f $ pretty $ C8.unpack str
  where
    f | C8.any (== ' ') str = compositeMetaDoc
      | otherwise           = atomicMetaDoc

lazyByteStringMetaDoc :: CL8.ByteString -> MetaDoc ann
lazyByteStringMetaDoc str = f $ pretty $ CL8.unpack str
  where
    f | CL8.any (== ' ') str = compositeMetaDoc
      | otherwise            = atomicMetaDoc

shortByteStringMetaDoc :: ShortBS.ShortByteString -> MetaDoc ann
shortByteStringMetaDoc str = f $ pretty $ C8.unpack str'
  where
    str' = ShortBS.fromShort str
    f | C8.any (== ' ') str' = compositeMetaDoc
      | otherwise            = atomicMetaDoc

constructorAppMetaDoc :: MetaDoc ann -> [MetaDoc ann] -> MetaDoc ann
constructorAppMetaDoc constructor args =
  case map field args of
    []  -> constructor
    [f] -> compositeMetaDoc $ mdPayload constructor <+> group (mdPayload f)
    fs  -> compositeMetaDoc $ PP.align $ mdPayload constructor ## PP.vsep (map mdPayload fs)
  where
    field :: MetaDoc ann -> MetaDoc ann
    field md =
      case mdKind md of
        Atomic    -> md
        Composite -> compositeMetaDoc $ PP.flatAlt payload (PP.parens payload)
      where
        payload = mdPayload md
