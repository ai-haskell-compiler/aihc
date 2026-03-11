{-|
Module      : Data.Fits.MegaParser
Description : MegaParsec based parser for an HDU.
Copyright   : (c) Zac Slade, 2023
License     : BSD2
Maintainer  : krakrjak@gmail.com
Stability   : experimental

Parsing rules for an HDU in a FITS file.
-}

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Fits.MegaParser where

-- qualified imports
---- bytestring
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS ( c2w )
---- megaparsec
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Stream as M
import qualified Text.Megaparsec.Pos as MP
import qualified Text.Megaparsec.Byte as M
import qualified Text.Megaparsec.Byte.Lexer as MBL
---- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
---- local imports
import qualified Data.Fits as Fits
import qualified Data.ByteString.Char8 as C8
import qualified Data.Binary as C8


-- symbol imports
---- bytestring
import Data.ByteString ( ByteString )
---- text
import Data.Text ( Text )
---- megaparsec
import Text.Ascii ( isAscii )
import Text.Megaparsec ( Parsec, ParseErrorBundle, (<|>), (<?>))
---- microlens
import Lens.Micro ((^.))
---- base
import Control.Applicative ( (<$>) )
import Control.Exception ( Exception(displayException) )
import Control.Monad ( void, foldM, replicateM_ )
import Data.Bifunctor ( first )
import Data.Char ( ord )
import Data.Maybe ( catMaybes, fromMaybe )
import Data.Word ( Word8, Word16, Word32, Word64 )
import Data.Void ( Void )
---- local imports
import Data.Fits
  ( Axes
  , Dimensions(Dimensions)
  , Header(Header)
  , KeywordRecord(..)
  , HeaderRecord(..)
  , HeaderDataUnit(HeaderDataUnit)
  , BitPixFormat(..)
  , Extension(..)
  , LogicalConstant(..)
  , Value(..)
  , bitPixToByteSize, hduRecordLength
  )


type Parser = Parsec Void ByteString
type ParseErr = ParseErrorBundle ByteString Void

data DataUnitValues
  = FITSUInt8 Word8
  | FITSInt16 Word16
  | FITSInt32 Word32
  | FITSInt64 Word64
  | FITSFloat32 Float
  | FITSFloat64 Double 


toWord :: Char -> Word8
toWord = fromIntegral . ord

wordsText :: [Word8] -> Text
wordsText = TE.decodeUtf8 . BS.pack


-- | Consumes ALL header blocks until end, then all remaining space
parseHeader :: Parser Header
parseHeader = do
    pairs <- M.manyTill parseRecordLine (M.string' "end")
    M.space -- consume space padding all the way to the end of the next 2880 bytes header block
    return $ Header pairs

parseRecordLine :: Parser HeaderRecord
parseRecordLine = do
    M.try (Keyword <$> parseKeywordRecord)
      <|> M.try (Comment <$> parseLineComment)
      <|> BlankLine <$ parseLineBlank


parseKeywordRecord :: Parser KeywordRecord
parseKeywordRecord = do
    ((k, v), mc) <- withComments parseKeywordValue
    pure $ KeywordRecord k v mc

-- | Parses the specified keyword
parseKeywordRecord' :: ByteString -> Parser a -> Parser a
parseKeywordRecord' k pval = ignoreComments $ do
    M.string' k
    parseEquals
    pval




-- | Combinator to allow for parsing a record with inline comments
withComments :: Parser a -> Parser (a, Maybe Text)
withComments parse = do
    -- assumes we are at the beginning of the line
    lineStart <- parsePos
    a <- parse
    mc <- parseLineEnd lineStart
    return (a, mc)

ignoreComments :: Parser a -> Parser a
ignoreComments parse = do
    (a, _) <- withComments parse
    pure a


parseKeywordValue :: Parser (Text, Value)
parseKeywordValue = do
    key <- parseKeyword
    parseEquals
    val <- parseValue
    return (key, val)


parseLineEnd :: Int -> Parser (Maybe Text)
parseLineEnd lineStart = do
  M.try (Nothing <$ spacesToLineEnd lineStart) <|> (Just <$> parseInlineComment lineStart)


spacesToLineEnd :: Int -> Parser ()
spacesToLineEnd lineStart = do
  curr <- parsePos
  let used = curr - lineStart
  parseSpacesN (hduRecordLength - used)
  pure ()

parseSpacesN :: Int -> Parser ()
parseSpacesN n = replicateM_ n (M.char $ toWord ' ')

parseInlineComment :: Int -> Parser Text
parseInlineComment lineStart = do
    -- any number of spaces... the previous combinator has eaten up blank lines already
    M.space
    M.char $ toWord '/'
    M.optional charSpace
    curr <- parsePos
    let used = curr - lineStart
    c <- M.count (hduRecordLength - used) M.anySingle
    return $ T.strip $ wordsText c
  where
    charSpace = M.char $ toWord ' '


parseLineComment :: Parser Text
parseLineComment = do
    let keyword = "COMMENT " :: ByteString
    M.string' keyword
    c <- M.count (hduRecordLength - BS.length keyword) M.anySingle
    return $ wordsText c

parseLineBlank :: Parser ()
parseLineBlank = do
  M.string' (BS.replicate hduRecordLength (toWord ' '))
  pure ()


-- | Anything but a space or equals
parseKeyword :: Parser Text
parseKeyword = wordsText <$> M.some (M.noneOf $ fmap toWord [' ', '='])

parseValue :: Parser Value
parseValue =
    -- try is required here because Megaparsec doesn't automatically backtrack if the parser consumes anything
    M.try (Float <$> parseFloat)
    <|> M.try (Integer <$> parseInt)
    <|> (Logic <$> parseLogic)
    <|> (String <$> parseStringContinue)

parseInt :: Num a => Parser a
parseInt = MBL.signed M.space MBL.decimal

parseFloat :: Parser Float
parseFloat = MBL.signed M.space MBL.float

parseLogic :: Parser LogicalConstant
parseLogic = do
    T <$ M.string' "T" <|> F <$ M.string' "F"

parseStringContinue :: Parser Text
parseStringContinue = do
    t <- parseStringValue

    mc <- M.optional $ do
      M.string' "CONTINUE"
      M.space
      parseStringContinue

    case mc of
      Nothing -> return t
      Just tc -> return $ T.dropWhileEnd (=='&') t <> tc

parseStringValue :: Parser Text
parseStringValue = do
    -- The rules are weird, NULL means a NULL string, '' is an empty
    -- string, a ' followed by a bunch of spaces and a close ' is
    -- considered an empty string, and trailing whitespace is ignored
    -- within the quotes, but not leading spaces.
    ls <- M.between (M.char quote) (M.char quote) $ M.many $ M.anySingleBut quote
    consumeDead
    return (T.stripEnd $ wordsText ls)
    where quote = toWord '\''

requireKeyword :: Text -> Header -> Parser Value
requireKeyword k kvs = do
    case Fits.lookup k kvs of
      Nothing -> fail $ "Missing: " <> show k
      Just v -> return v

requireNaxis :: Header -> Parser Int
requireNaxis kvs = do
    v <- requireKeyword "NAXIS" kvs
    case v of
      Integer n -> return n
      _ -> fail "Invalid NAXIS header"

skipEmpty :: Parser ()
skipEmpty = void (M.many $ M.satisfy (toWord '\0' ==))

consumeDead :: Parser ()
consumeDead = M.space >> skipEmpty

parseEnd :: Parser ()
parseEnd = M.string' "end" >> M.space <* M.eof

parseEquals :: Parser ()
parseEquals = M.space >> M.char (toWord '=') >> M.space

parsePos :: Parser Int
parsePos = MP.unPos . MP.sourceColumn <$> M.getSourcePos


parseBitPix :: Parser BitPixFormat
parseBitPix = do
    v <- parseKeywordRecord' "BITPIX" parseValue
    toBitpix v
    where
      toBitpix (Integer 8) = return EightBitInt
      toBitpix (Integer 16) = return SixteenBitInt
      toBitpix (Integer 32) = return ThirtyTwoBitInt
      toBitpix (Integer 64) = return SixtyFourBitInt
      toBitpix (Integer (-32)) = return ThirtyTwoBitFloat
      toBitpix (Integer (-64)) = return SixtyFourBitFloat
      toBitpix _ = fail "Invalid BITPIX header"

parseNaxes :: Parser Axes
parseNaxes = do
    n <- parseKeywordRecord' "NAXIS" parseInt
    mapM parseN [1..n]
  where
    parseN :: Int -> Parser Int
    parseN n = parseKeywordRecord' (C8.pack $ "NAXIS" <> show n) parseInt

-- | We don't parse simple here, because it isn't required on all HDUs
parseDimensions :: Parser Dimensions
parseDimensions = do
    bp <- parseBitPix
    Dimensions bp <$> parseNaxes

parsePrimary :: Parser HeaderDataUnit
parsePrimary = do
    dm <- parsePrimaryKeywords
    hd <- parseHeader
    dt <- parseMainData dm
    return $ HeaderDataUnit hd dm Primary dt


parsePrimaryKeywords :: Parser Dimensions
parsePrimaryKeywords = do
    parseKeywordRecord' "SIMPLE" parseLogic
    M.lookAhead parseDimensions


parseImage :: Parser HeaderDataUnit
parseImage = do
    dm <- parseImageKeywords
    hd <- parseHeader
    dt <- parseMainData dm
    return $ HeaderDataUnit hd dm Image dt

parseImageKeywords :: Parser Dimensions
parseImageKeywords = do
    ignoreComments $ M.string' "XTENSION= 'IMAGE   '"
    M.lookAhead parseDimensions

parseBinTable :: Parser HeaderDataUnit
parseBinTable = do
    (dm, pc) <- M.lookAhead parseBinTableKeywords
    hd <- parseHeader
    dt <- parseMainData dm
    hp <- parseBinTableHeap
    let tab = BinTable pc hp
    return $ HeaderDataUnit hd dm tab dt
    where
      parseBinTableHeap = return ""

parseBinTableKeywords :: Parser (Dimensions, Int)
parseBinTableKeywords = do 
  ignoreComments $ M.string' "XTENSION= 'BINTABLE'"
  sz <- parseDimensions
  pc <- parseKeywordRecord' "PCOUNT" parseInt
  return (sz, pc)

parseMainData :: Dimensions -> Parser ByteString
parseMainData size = do
    let len = dataSize size
    M.takeP (Just ("Data Array of " <> show len <> " Bytes")) (fromIntegral len)

parseHDU :: Parser HeaderDataUnit
parseHDU =
    parsePrimary <|> parseImage <|> parseBinTable

parseHDUs :: Parser [HeaderDataUnit]
parseHDUs = do
    M.many parseHDU

dataSize :: Dimensions -> Int
dataSize (Dimensions bitpix axes) = size bitpix * count axes
  where
    count [] = 0
    count ax = fromIntegral $ product ax
    size = fromIntegral . bitPixToByteSize
