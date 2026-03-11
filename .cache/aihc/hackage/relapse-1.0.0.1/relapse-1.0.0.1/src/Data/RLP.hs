{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
module Data.RLP
    ( RLPObject(..)
    , RLPEncodable(..)
    , rlpParser
    , unpackRLP
    , unpackRLPFully
    , packRLP
    , rlpSerialize
    , rlpDeserialize
    , module Data.RLP.Types
    ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              ((<=<))
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.Combinator
import           Data.Bits                  (Bits, FiniteBits, finiteBitSize,
                                             shiftL, shiftR, (.|.))
import qualified Data.ByteString            as S
import qualified Data.ByteString.Char8      as S8
import           Data.List                  (foldl', intercalate)
import           Data.Word
import           Numeric                    (showHex)
import           Prelude                    hiding (take)
import qualified Prelude                    as P

import           Data.RLP.Types

singleByteParser :: Parser RLPObject
singleByteParser = String . S.singleton <$> satisfy (<= 0x7F)

shortParser :: Word8 -> (a -> RLPObject) -> (S.ByteString -> Parser a) -> a -> Parser RLPObject
shortParser base constructor postProcessor def = do
    len <- fromIntegral . subtract base <$> satisfy (\x -> x >= base && x <= (base + 55))
    if len == 0
    then return (constructor def)
    else constructor <$> (take len >>= postProcessor)

longParser :: Word8 -> (a -> RLPObject) -> (S.ByteString -> Parser a) -> Parser RLPObject
longParser base constructor postProcessor = do
    lengthLength <- fromIntegral . subtract base <$> satisfy (\x -> x > base && x <= (base + 8))
    payloadLen <- unpackBE . S.unpack <$> take lengthLength
    constructor <$> (take payloadLen >>= postProcessor)

shortStringParser :: Parser RLPObject
shortStringParser = shortParser 0x80 String return S.empty

longStringParser :: Parser RLPObject
longStringParser = longParser 0xB7 String return

shortListParser :: Parser RLPObject
shortListParser = shortParser 0xC0 Array parseListPayload []

longListParser :: Parser RLPObject
longListParser = longParser 0xF7 Array parseListPayload

parseListPayload :: S.ByteString -> Parser [RLPObject]
parseListPayload pl = case parse rlpParser pl of
    Done rem res -> if S8.null rem
        then return [res]
        else (res:) <$> parseListPayload rem
    Partial _    ->
        fail "Partial result when parsing an RLP list member, this should be impossible."
    Fail rem ctxs err ->
        fail $ "RLP list member parse failed: " ++ intercalate ", " ctxs ++ ": " ++ err
               ++ ". Remaining data: \"" ++ S8.unpack rem ++ "\""

rlpParser :: Parser RLPObject
rlpParser =  try (singleByteParser  <?> "single byte")
         <|> try (longStringParser  <?> "long string")  -- long string/list go first since we dont want the error
         <|> try (shortStringParser <?> "short string") -- message saying it failed parsing a long list cause it fell
         <|> try (longListParser    <?> "long list")    -- through after failing to correctly parse a short one
         <|> try (shortListParser   <?> "short list")


unpackRLP :: S.ByteString -> Either String RLPObject
unpackRLP input = case parseOnly rlpParser input of
    Left err -> Left $ "Parse failed: " ++ err -- to have consistent errors w/ `parseRLPFully`
    r        -> r

unpackRLPFully :: S.ByteString -> Either String RLPObject
unpackRLPFully input = handleResult $ parse rlpParser input
    where handleResult = \case
            Done rem res -> if S8.null rem
                then Right res
                else Left $ "Incomplete parse, leftover data: " ++ S8.unpack rem
            Fail rem ctxs err ->
                Left $ "Parse failed: " ++ intercalate ", " ctxs ++ ": " ++ err
                       ++ ". Remaining data: \"" ++ S8.unpack rem ++ "\""
            Partial cont -> handleResult (cont S8.empty)

packRLP :: RLPObject -> S.ByteString
packRLP o = case o of
    String s -> packString s
    Array xs -> packList xs
    where packString s | len == 0  = S.singleton 0x80
                       | len == 1  = packSingleChar (S.head s)
                       | len <= 55 = S.cons (0x80 + fromIntegral len) s
                       | otherwise = prefixLength 0xB7 s
                       where len = S.length s

          packSingleChar c | c <= 0x7F = S.singleton c
                           | otherwise = S.pack [0x81, c]

          packList xs | payloadLength <= 55 = S.cons (0xC0 + fromIntegral payloadLength) packedPayload
                      | otherwise = prefixLength 0xF7 packedPayload
                      where packedPayload = S.concat (packRLP <$> xs)
                            payloadLength = S.length packedPayload

          prefixLength base str = (prefixed `S.cons` pLen) `S.append` str
              where len      = S.length str
                    pLen     = S.pack (packFiniteBE len)
                    pLenLen  = fromIntegral (S.length pLen)
                    prefixed = base + pLenLen

rlpSerialize :: RLPEncodable a => a -> S.ByteString
rlpSerialize = packRLP . rlpEncode

rlpDeserialize :: RLPEncodable a => S.ByteString -> Either String a
rlpDeserialize = rlpDecode <=< unpackRLP
