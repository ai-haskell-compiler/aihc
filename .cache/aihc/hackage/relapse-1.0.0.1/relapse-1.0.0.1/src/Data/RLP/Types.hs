{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -fno-warn-orphans     #-}
module Data.RLP.Types where

import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8

import           Data.Bits
import           Data.Char             (ord)
import           Data.Foldable
import           Data.Int
import           Data.List             (foldl')
import           Data.Word

import           GHC.Generics

data RLPObject = String S.ByteString | Array [RLPObject] deriving (Eq, Ord, Read, Show)

rlp0 :: RLPObject
rlp0 = String S.empty

class RLPEncodable a where
    rlpEncode :: a -> RLPObject
    rlpDecode :: RLPObject -> Either String a

    default rlpEncode :: (Generic a, GRLPEncodable (Rep a)) => a -> RLPObject
    rlpEncode = gRLPEncode . from

    default rlpDecode :: (Generic a, GRLPEncodable (Rep a)) => RLPObject -> Either String a
    rlpDecode = fmap to . gRLPDecode

rlpEncodeFinite :: (FiniteBits n, Integral n) => n -> RLPObject
rlpEncodeFinite = rlpEncode . S.pack . packFiniteBE
{-# INLINE rlpEncodeFinite #-}

rlpDecodeIntegralBE :: (Bits n, Integral n) => RLPObject -> Either String n
rlpDecodeIntegralBE = \case
    String s -> Right . unpackBE $ S.unpack s
    x        -> rlpDecodeFail "String" x
{-# INLINE rlpDecodeIntegralBE #-}

rlpDecodeFail :: String -> RLPObject -> Either String a
rlpDecodeFail myType instead =
    Left $ "Expected an RLPObject that's isomorphic to " ++ myType ++ ", instead got " ++ show instead
{-# INLINE rlpDecodeFail #-}

-- todo: more efficient?
unpackBE :: (Bits n, Integral n) => [Word8] -> n
unpackBE words = foldl' (.|.) 0 shifted
    where shifts  = [((wc - 1) * 8), ((wc - 2) * 8)..0]
          wc      = length words
          doShift word shift = fromIntegral word `shiftL` shift
          shifted = zipWith doShift words shifts
{-# INLINE unpackBE #-}

-- todo: ditto
packFiniteBE :: (FiniteBits n, Integral n) => n -> [Word8]
packFiniteBE n = packWithByteCount byteCount n
    where byteCount = (finiteBitSize n + 7) `quot` 8
{-# INLINE packFiniteBE #-}

packIntegerBE :: Integer -> [Word8]
packIntegerBE n = packWithByteCount byteCount n
    where byteCount = (bitCount + 7) `quot` 8
          bitCount  = floor (logBase 2 $ fromIntegral n) + 1
{-# INLINE packIntegerBE #-}

packWithByteCount :: (Bits n, Integral n) => Int -> n -> [Word8]
packWithByteCount byteCount n = dropWhile (== 0) $ zipWith f rep shifts
    where rep    = replicate byteCount n
          shifts = [((byteCount - 1) * 8), ((byteCount - 2) * 8)..0]
          f r s  = fromIntegral (r `shiftR` s)
{-# INLINE packWithByteCount #-}

instance RLPEncodable S.ByteString where
    rlpEncode = String
    rlpDecode = \case
        String s -> Right s
        x        -> rlpDecodeFail "String" x

instance {-# OVERLAPPABLE #-} (Integral n, FiniteBits n) => RLPEncodable n where
    rlpEncode = rlpEncodeFinite
    rlpDecode = rlpDecodeIntegralBE

instance RLPEncodable Integer where
    rlpEncode = rlpEncode . S.pack . packIntegerBE
    rlpDecode = rlpDecodeIntegralBE

instance {-# OVERLAPPABLE #-} (RLPEncodable a) => RLPEncodable [a] where
    rlpEncode = Array . toList . fmap rlpEncode
    rlpDecode = \case
        Array xs -> sequence $ rlpDecode <$> xs
        x        -> rlpDecodeFail "Array" x

instance {-# OVERLAPPING #-} RLPEncodable String where
    rlpEncode = String . S8.pack
    rlpDecode = \case
        String s -> Right (S8.unpack s)
        x        -> rlpDecodeFail "String" x

instance RLPEncodable () where
    rlpEncode _ = rlp0
    rlpDecode x = if x == rlp0 then return () else rlpDecodeFail "()" x

instance (RLPEncodable a, RLPEncodable b) => RLPEncodable (a,b) where
    rlpEncode (a,b) = Array [rlpEncode a,rlpEncode b]
    rlpDecode = \case
      Array [a,b] -> (,) <$> rlpDecode a <*> rlpDecode b
      x           -> rlpDecodeFail "Pair" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  ) => RLPEncodable (a,b,c) where
  rlpEncode (a,b,c) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    ]
  rlpDecode = \case
    Array [a,b,c] -> (,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
    x -> rlpDecodeFail "Triple" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  , RLPEncodable d
  ) => RLPEncodable (a,b,c,d) where
  rlpEncode (a,b,c,d) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    , rlpEncode d
    ]
  rlpDecode = \case
    Array [a,b,c,d] -> (,,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
      <*> rlpDecode d
    x -> rlpDecodeFail "Quadruple" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  , RLPEncodable d
  , RLPEncodable e
  ) => RLPEncodable (a,b,c,d,e) where
  rlpEncode (a,b,c,d,e) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    , rlpEncode d
    , rlpEncode e
    ]
  rlpDecode = \case
    Array [a,b,c,d,e] -> (,,,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
      <*> rlpDecode d
      <*> rlpDecode e
    x -> rlpDecodeFail "Quintuple" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  , RLPEncodable d
  , RLPEncodable e
  , RLPEncodable f
  ) => RLPEncodable (a,b,c,d,e,f) where
  rlpEncode (a,b,c,d,e,f) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    , rlpEncode d
    , rlpEncode e
    , rlpEncode f
    ]
  rlpDecode = \case
    Array [a,b,c,d,e,f] -> (,,,,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
      <*> rlpDecode d
      <*> rlpDecode e
      <*> rlpDecode f
    x -> rlpDecodeFail "Sextuple" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  , RLPEncodable d
  , RLPEncodable e
  , RLPEncodable f
  , RLPEncodable g
  ) => RLPEncodable (a,b,c,d,e,f,g) where
  rlpEncode (a,b,c,d,e,f,g) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    , rlpEncode d
    , rlpEncode e
    , rlpEncode f
    , rlpEncode g
    ]
  rlpDecode = \case
    Array [a,b,c,d,e,f,g] -> (,,,,,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
      <*> rlpDecode d
      <*> rlpDecode e
      <*> rlpDecode f
      <*> rlpDecode g
    x -> rlpDecodeFail "Septuple" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  , RLPEncodable d
  , RLPEncodable e
  , RLPEncodable f
  , RLPEncodable g
  , RLPEncodable h
  ) => RLPEncodable (a,b,c,d,e,f,g,h) where
  rlpEncode (a,b,c,d,e,f,g,h) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    , rlpEncode d
    , rlpEncode e
    , rlpEncode f
    , rlpEncode g
    , rlpEncode h
    ]
  rlpDecode = \case
    Array [a,b,c,d,e,f,g,h] -> (,,,,,,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
      <*> rlpDecode d
      <*> rlpDecode e
      <*> rlpDecode f
      <*> rlpDecode g
      <*> rlpDecode h
    x -> rlpDecodeFail "Octuple" x

instance
  ( RLPEncodable a
  , RLPEncodable b
  , RLPEncodable c
  , RLPEncodable d
  , RLPEncodable e
  , RLPEncodable f
  , RLPEncodable g
  , RLPEncodable h
  , RLPEncodable i
  ) => RLPEncodable (a,b,c,d,e,f,g,h,i) where
  rlpEncode (a,b,c,d,e,f,g,h,i) = Array
    [ rlpEncode a
    , rlpEncode b
    , rlpEncode c
    , rlpEncode d
    , rlpEncode e
    , rlpEncode f
    , rlpEncode g
    , rlpEncode h
    , rlpEncode i
    ]
  rlpDecode = \case
    Array [a,b,c,d,e,f,g,h,i] -> (,,,,,,,,)
      <$> rlpDecode a
      <*> rlpDecode b
      <*> rlpDecode c
      <*> rlpDecode d
      <*> rlpDecode e
      <*> rlpDecode f
      <*> rlpDecode g
      <*> rlpDecode h
      <*> rlpDecode i
    x -> rlpDecodeFail "Nontuple" x

instance RLPEncodable RLPObject where -- ayy lmao
    rlpEncode = id
    rlpDecode = Right

instance RLPEncodable Char where
    rlpEncode = rlpEncodeFinite . ord
    rlpDecode = \case
        String s | S.length s == 1 -> Right (S8.head s)
        x                          -> rlpDecodeFail "String of length 1" x


-- Generic instances
class GRLPEncodable g where
  gRLPEncode :: g a -> RLPObject
  gRLPDecode :: RLPObject -> Either String (g a)

-- Reuse () instance for Unit generic
instance GRLPEncodable U1 where
    gRLPEncode U1 = rlpEncode ()
    gRLPDecode = fmap (const U1) . (rlpDecode :: RLPObject -> Either String ())

-- Products are just an RLP object with two sub-objects
instance (GRLPEncodable a, GRLPEncodable b) => GRLPEncodable (a :*: b) where
  gRLPEncode (a :*: b) = rlpEncode $ Array [gRLPEncode a, gRLPEncode b]
  gRLPDecode x = rlpDecode x >>= \case
      Array [a, b] -> do
        a' <- gRLPDecode a
        b' <- gRLPDecode b
        return (a' :*: b')
      _ -> rlpDecodeFail "expected RLP list of two subobjects for generic product type decode" x

-- Sums use a [identifier_string, rlp_obj] structure to pick correct constructor
instance (GRLPEncodable a, GRLPEncodable b) => GRLPEncodable (a :+: b) where
  gRLPEncode (L1 a) = rlpEncode $ Array [String (S8.pack "L"), gRLPEncode a]
  gRLPEncode (R1 b) = rlpEncode $ Array [String (S8.pack "R"), gRLPEncode b]

  gRLPDecode x = rlpDecode x >>= \case
    Array [String "L", l] -> L1 <$> gRLPDecode l
    Array [String "R", r] -> R1 <$> gRLPDecode r
    x -> rlpDecodeFail "expected RLP list of [\"L\" | \"R\", RLPObject] for generic sum type decode" x

-- MetaInfo is just passthrough
instance (GRLPEncodable a) => GRLPEncodable (M1 i c a) where
  gRLPEncode (M1 a) = gRLPEncode a
  gRLPDecode = fmap M1 . gRLPDecode

-- ditto for constant arg application
instance (RLPEncodable a) => GRLPEncodable (K1 i a) where
  gRLPEncode (K1 a) = rlpEncode a
  gRLPDecode = fmap K1 . rlpDecode
