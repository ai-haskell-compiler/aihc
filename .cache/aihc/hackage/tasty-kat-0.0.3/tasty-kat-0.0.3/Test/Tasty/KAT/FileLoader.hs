{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Test.Tasty.KAT.FileLoader
-- License     : MIT
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- extra loaders helpers
--
module Test.Tasty.KAT.FileLoader
    ( katLoader
    , katLoaderSimple
    -- * generic helpers on TestResource
    , mapTestUnitValues
    , mapTestUnits
    -- * common helpers on TestResource
    , mapTestUnitValuesBase64
    , mapTestUnitValuesBase16
    -- * common value decoding helpers
    , valueUnbase16
    , valueUnbase64
    , valueInteger
    , valueHexInteger
    -- * associated hierarchy of KAT types 
    , TestResource
    , TestGroup
    , TestUnit
    ) where

import Control.Arrow (second)
import Data.Bits
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B
import Data.ByteString (ByteString)
import Data.ByteString.Char8 () -- for Bytestring OverloadedString instance on old ghc
import Data.List
import Data.Word
import Foreign.Storable
import Foreign.Ptr

import Test.Tasty.KAT.Internal

type TestResource a = [(String, TestGroup a)]
type TestGroup a = [TestUnit a]
type TestUnit a = [a]

-- | From a simple KAT file, extract 
--
-- * lines starting by #, are assumed to be comment
--
-- format should be the following:
-- 
-- > skipped ..
-- > skipped ..
-- > [group1]
-- >
-- > f1= v1
-- > f2= v2
-- > ...
-- > 
-- > f1= v3
-- > f2= v4
-- > ...
-- >
-- > [group2]
-- > ...
katLoaderSimple :: [String] -> TestResource (String, String)
katLoaderSimple = katLoader '=' "#"

katLoader :: Char     -- ^ key value separator, e.g. '='
          -> String   -- ^ line comment, e.g. "--" "#"
          -> [String] -- ^ input lines
          -> TestResource (String, String)
katLoader kvSep lineComment =
      map (second (map (map kv)))
    . removeEmpty
    . map (second (splitWhen null)) -- split a group of lines into a group of tests
    . groupify "" []
    . map noTrailing
    . filter (not . isComment)
  where isComment = isPrefixOf lineComment

        removeEmpty = filter ((/= []) . snd)

        groupify :: String -> [String] -> [String] -> [(String, [String])]
        groupify gname acc []     = [(gname, reverse acc)]
        groupify gname acc (x:xs) =
            case getGroupHeader x of
                Just hdr -> (gname, reverse acc) : groupify hdr [] xs
                Nothing  -> groupify gname (x:acc) xs

        kv :: String -> (String, String)
        kv s = case break (== kvSep) s of
                    (k, c:v)
                        | c == kvSep -> (stripSpaces k, stripSpaces v)
                        | otherwise  -> (stripSpaces k, stripSpaces v)
                    (_, _)     -> (s, "") -- no error handling ..

        getGroupHeader :: String -> Maybe String
        getGroupHeader s
            | isPrefixOf "[" s && isSuffixOf "]" s = Just . drop 1 . reverse . drop 1 . reverse $ s
            | otherwise                            = Nothing

        noTrailing = reverse . dropWhile (== ' ') . reverse

        splitWhen :: (a -> Bool) -> [a] -> [[a]]
        splitWhen p s = case dropWhile p s of
                             [] -> []
                             s' -> w : splitWhen p s''
                                   where (w, s'') = break p s'
        stripSpaces = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

mapTestUnitValues :: (String -> a) -> TestResource (String, String) -> TestResource (String,a)
mapTestUnitValues f = map (second (map (map (\(k,v) -> (k, f v)))))

mapTestUnits :: (TestUnit (String,a) -> TestUnit b)
             -> TestResource (String,a)
             -> TestResource b
mapTestUnits f = map (second (map f))

mapTestUnitValuesBase64 :: TestResource (String, String) -> TestResource (String, ByteString)
mapTestUnitValuesBase64 =  mapTestUnitValues valueUnbase64

mapTestUnitValuesBase16 :: TestResource (String, String) -> TestResource (String, ByteString)
mapTestUnitValuesBase16 = mapTestUnitValues valueUnbase16

-- expect an ascii string.
valueUnbase64 :: String -> ByteString
valueUnbase64 s
    | (length s `mod` 4) /= 0 = error ("decodiong base64 not proper length: " ++ s)
    | otherwise               = unsafeCreateUptoN maxSz $ \ptr -> do
                                    szRemove <- loop s ptr
                                    return (maxSz - szRemove)
  where maxSz = (length s `div` 4) * 3
        loop []               _   = return 0
        loop (w:x:'=':'=':[]) ptr = do
            let w' = rset w
                x' = rset x
            poke ptr ((w' `shiftL` 2) .|. (x' `shiftR` 4))
            return 2
        loop (w:x:y:'=':[])   ptr = do
            let w' = rset w
                x' = rset x
                y' = rset y
            poke ptr               ((w' `shiftL` 2) .|. (x' `shiftR` 4))
            poke (ptr `plusPtr` 1) ((x' `shiftL` 4) .|. (y' `shiftR` 2))
            return 1
        loop (w:x:y:z:r)      ptr = do
            let w' = rset w
                x' = rset x
                y' = rset y
                z' = rset z
            poke ptr               ((w' `shiftL` 2) .|. (x' `shiftR` 4))
            poke (ptr `plusPtr` 1) ((x' `shiftL` 4) .|. (y' `shiftR` 2))
            poke (ptr `plusPtr` 2) ((y' `shiftL` 6) .|. z')
            loop r (ptr `plusPtr` 3)
        loop _                _   = error ("internal error in base64 decoding")
        
        rset :: Char -> Word8
        rset c
            | cval <= 0xff = B.unsafeIndex rsetTable cval
            | otherwise    = 0xff
          where cval = fromEnum c
        -- dict = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        rsetTable = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x3e\xff\xff\xff\x3f\
                    \\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\xff\xff\xff\xff\xff\xff\
                    \\xff\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\
                    \\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\xff\xff\xff\xff\xff\
                    \\xff\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\
                    \\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\
                    \\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"

-- expect an ascii string.
valueUnbase16 :: String -> ByteString
valueUnbase16 s
    | odd (length s) = error ("decoding base16 not proper length: " ++ s)
    | otherwise = B.unsafeCreate (length s `div` 2) (loop s)
  where loop []         _   = return ()
        loop (x1:x2:xs) ptr = do
            poke ptr ((unhex x1 `shiftL` 4) .|. unhex x2)
            loop xs (ptr `plusPtr` 2)
        loop _          _   = error "internal error in base16 decoding"
        unhex :: Char -> Word8
        unhex c
            | c >= '0' && c <= '9' = fromIntegral (fromEnum c - fromEnum '0')
            | c >= 'a' && c <= 'f' = 10 + fromIntegral (fromEnum c - fromEnum 'a')
            | c >= 'A' && c <= 'F' = 10 + fromIntegral (fromEnum c - fromEnum 'A')
            | otherwise            = error ("invalid base16 character " ++ show c ++ " in " ++ show s)

valueInteger :: String -> Integer
valueInteger s = read s

valueHexInteger :: String -> Integer
valueHexInteger s = read ("0x" ++ s)
