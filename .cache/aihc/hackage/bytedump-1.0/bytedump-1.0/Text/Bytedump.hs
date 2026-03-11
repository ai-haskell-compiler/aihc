
-- |
-- Module      : Text.Bytedump
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- A module containing some routines to debug data dump
--

module Text.Bytedump
    ( hexString

    -- * Formatted string configuration
    , BytedumpConfig(..)
    , defaultConfig

    -- * Dump bytes into not formatted strings
    , dumpRaw
    , dumpRawS
    , dumpRawBS
    , dumpRawLBS

    -- * Dump bytes into formatted strings using a specific config
    , dumpWith
    , dumpWithS
    , dumpWithBS
    , dumpWithLBS

    -- * Dump bytes into formatted strings using default config
    , dump
    , dumpS
    , dumpBS
    , dumpLBS

    -- * Dump 2 set of bytes into formatted side-by-side strings using default config
    , dumpDiff
    , dumpDiffS
    , dumpDiffBS
    , dumpDiffLBS
    ) where

import Data.List
import Data.Word
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

-- | Configuration structure used for formatting functions
data BytedumpConfig = BytedumpConfig
    { configRowSize      :: Int    -- ^ number of bytes per row.
    , configRowGroupSize :: Int    -- ^ number of bytes per group per row.
    , configRowGroupSep  :: String -- ^ string separating groups.
    , configRowLeft      :: String -- ^ string on the left of the row.
    , configRowRight     :: String -- ^ string on the right of the row.
    , configCellSep      :: String -- ^ string separating cells in row.
    , configPrintChar    :: Bool   -- ^ if the printable ascii table is displayed.
    } deriving (Show,Eq)

-- | Default Config using 16 bytes by row with a separation at the 8th byte, and
-- dumping printable ascii character on the right.
defaultConfig :: BytedumpConfig
defaultConfig = BytedumpConfig
    { configRowSize      = 16
    , configRowGroupSize = 8
    , configRowGroupSep  = " : "
    , configRowLeft      = " | "
    , configRowRight     = " | "
    , configCellSep      = " "
    , configPrintChar    = True
    }

hex :: Int -> Char
hex 0  = '0'
hex 1  = '1'
hex 2  = '2'
hex 3  = '3'
hex 4  = '4'
hex 5  = '5'
hex 6  = '6'
hex 7  = '7'
hex 8  = '8'
hex 9  = '9'
hex 10 = 'a'
hex 11 = 'b'
hex 12 = 'c'
hex 13 = 'd'
hex 14 = 'e'
hex 15 = 'f'
hex _  = ' '

{-# INLINE hexBytes #-}
hexBytes :: Word8 -> (Char, Char)
hexBytes w = (hex h, hex l) where (h,l) = (fromIntegral w) `divMod` 16

-- | Dump one byte into a 2 hexadecimal characters.
hexString :: Word8 -> String
hexString i = [h,l] where (h,l) = hexBytes i

-- | Dump a list of word8 into a raw string of hex value
dumpRaw :: [Word8] -> String
dumpRaw = concatMap hexString

-- | Dump a string into a raw string of hex value
dumpRawS :: String -> String
dumpRawS = dumpRaw . map (toEnum.fromEnum)

-- | Dump a bytestring into a raw string of hex value
dumpRawBS :: B.ByteString -> String
dumpRawBS = dumpRaw . B.unpack

-- | Dump a lazy bytestring into a raw string of hex value
dumpRawLBS :: L.ByteString -> String
dumpRawLBS = dumpRaw . L.unpack

disptable :: BytedumpConfig -> [Word8] -> [String]
disptable _   [] = []
disptable cfg x  =
    let (pre, post) = splitAt (configRowSize cfg) x in
    tableRow pre : disptable cfg post
    where
        tableRow row =
            let l  = splitMultiple (configRowGroupSize cfg) $ map hexString row in
            let lb = intercalate (configRowGroupSep cfg) $ map (intercalate (configCellSep cfg)) l in
            let rb = map printChar row in
            let rowLen = 2 * configRowSize cfg
                       + (configRowSize cfg - 1) * length (configCellSep cfg)
                       + ((configRowSize cfg `div` configRowGroupSize cfg) - 1) * length (configRowGroupSep cfg) in
            configRowLeft cfg ++ lb ++ replicate (rowLen - length lb) ' ' ++ configRowRight cfg ++ (if configPrintChar cfg then rb else "")

        splitMultiple _ [] = []
        splitMultiple n l  = let (pre, post) = splitAt n l in pre : splitMultiple n post

        printChar :: Word8 -> Char
        printChar w
            | w >= 0x20 && w < 0x7f = toEnum $ fromIntegral w
            | otherwise             = '.'

dispDiffTable :: BytedumpConfig -> [Word8] -> [Word8] -> [String]
dispDiffTable _   [] [] = []
dispDiffTable cfg x1 x2 =
    let (pre1, post1) = splitAt (configRowSize cfg) x1 in
    let (pre2, post2) = splitAt (configRowSize cfg) x2 in
    tableRow pre1 pre2 : dispDiffTable cfg post1 post2

    where
        tableRow row1 row2 =
            let l1 = splitMultiple (configRowGroupSize cfg) $ map hexString row1 in
            let l2 = splitMultiple (configRowGroupSize cfg) $ map hexString row2 in
            let lb1 = intercalate (configRowGroupSep cfg) $ map (intercalate (configCellSep cfg)) l1 in
            let lb2 = intercalate (configRowGroupSep cfg) $ map (intercalate (configCellSep cfg)) l2 in
            let rowLen = 2 * configRowSize cfg
                       + (configRowSize cfg - 1) * length (configCellSep cfg)
                       + ((configRowSize cfg `div` configRowGroupSize cfg) - 1) * length (configRowGroupSep cfg) in
            configRowLeft cfg ++ lb1 ++ replicate (rowLen - length lb1) ' ' ++ configRowRight cfg
                              ++ lb2 ++ replicate (rowLen - length lb2) ' ' ++ configRowRight cfg

        splitMultiple _ [] = []
        splitMultiple n l  = let (pre, post) = splitAt n l in pre : splitMultiple n post

-- | Dump a list of bytes into formatted strings using a specific config
dumpWith :: BytedumpConfig -> [Word8] -> String
dumpWith cfg l = intercalate "\n" rows
    where rows = disptable cfg l

-- | Dump a string into formatted strings using a specific config
dumpWithS :: BytedumpConfig -> String -> String
dumpWithS cfg = dumpWith cfg . map (toEnum.fromEnum)

-- | Dump a bytestring into formatted strings using a specific config
dumpWithBS :: BytedumpConfig -> B.ByteString -> String
dumpWithBS cfg = dumpWith cfg . B.unpack

-- | Dump a lazy bytestring into formatted strings using a specific config
dumpWithLBS :: BytedumpConfig -> L.ByteString -> String
dumpWithLBS cfg = dumpWith cfg . L.unpack

-- | Dump a list of word8 into a formatted string of hex value
dump :: [Word8] -> String
dump l = intercalate "\n" rows
    where rows = disptable defaultConfig l

-- | Dump a string into a formatted string of hex value
dumpS :: String -> String
dumpS = dump . map (toEnum.fromEnum)

-- | Dump a bytestring into a formatted string of hex value
dumpBS :: B.ByteString -> String
dumpBS = dump . B.unpack

-- | Dump a lazy bytestring into a formatted string of hex value
dumpLBS :: L.ByteString -> String
dumpLBS = dump . L.unpack

-- | Dump two list of word8 into a formatted string of hex value side by side
dumpDiff :: [Word8] -> [Word8] -> String
dumpDiff l1 l2 = intercalate "\n" rows
    where rows = dispDiffTable defaultConfig l1 l2

-- | Dump a string into a formatted string of hex value
dumpDiffS :: String -> String -> String
dumpDiffS s1 s2 = dumpDiff (map (toEnum.fromEnum) s1) (map (toEnum.fromEnum) s2)

-- | Dump a bytestring into a formatted string of hex value
dumpDiffBS :: B.ByteString -> B.ByteString -> String
dumpDiffBS b1 b2 = dumpDiff (B.unpack b1) (B.unpack b2)

-- | Dump a lazy bytestring into a formatted string of hex value
dumpDiffLBS :: L.ByteString -> L.ByteString -> String
dumpDiffLBS l1 l2 = dumpDiff (L.unpack l1) (L.unpack l2)
