-- |
-- Module      :  DobutokO.Sound.Keyboard
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Keyboard (
  -- * Working with input and files
  qwerty2dvorak
  , dvorak2qwerty
  , input2BL
  , input2BLN
  , input2BLMN
  , readFile2BL
  , readFile2BLN
  , readFile2BLMN
  , readFile2BLGen  
  , readFile2BLGenN
  , readFile2BLGenMN
  -- * Conversions
  , readFileDoubles
  , readFileDoublesN
  , readFileDoublesMN
  , readFileDoublesGen
  , readFileDoublesGenN
  , readFileDoublesGenMN
  , takeDoubles
  , hashStr2
  , convH
) where

import GHC.Arr
import CaseBi.Arr (getBFst')
import Data.Char (isAsciiLower)
import qualified Data.Vector as V
import GHC.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as BL (ByteString,map,zipWith,tail,filter,getContents,readFile,take,drop)

-- | Converts a lazy 'BL.ByteString' into a 'V.Vector' of 'Int' using 'hashStr2'. 
takeDoubles :: BL.ByteString -> V.Vector Int
takeDoubles xs = V.fromList . BL.zipWith hashStr2 xs $ BL.tail xs

-- | Converts a string of lowercase ASCII letters being typed on the QWERTY keyboard layout into corresponding Dvorak keyboard layout.
qwerty2dvorak :: BL.ByteString -> BL.ByteString
qwerty2dvorak = BL.map (getBFst' (' ',listArray (0,29) . zip ("/;<>" ++ ['a'..'z']) $ "wvszaxje.uidchtnmbrl'poygk,qf;"))

-- | Vice versa to 'qwerty2dvorak'.
dvorak2qwerty :: BL.ByteString -> BL.ByteString
dvorak2qwerty = BL.map (getBFst' (' ',listArray (0,29) . zip ("',.;" ++ ['a'..'z']) $ "qwezanihdyujgcvpmlsrxo;kf.,bt/"))

-- | Hashes two lower case ascii characters. Is used for controlling frequencies and operators.
hashStr2 :: Char -> Char -> Int
hashStr2 x y = getBFst' (57, listArray (0,25) . zip ['a'..'z'] . map (\t -> getBFst' (26 * t + 18, listArray (0,25) . zip ['a'..'z'] $ [(26 * t + 4)..]) y) . 
  concat $ [[0],[6..8],[1],[9..11],[4],[12..16],[2],[17..21],[3],[22..24],[5,25]]) x -- 679 is the greatest value ~ \"zz\"; there are 572 effectful val.

-- | Get contents into lazy 'BL.ByteString' with filtering of all characters that are not a lower case ascii letters.
input2BL :: IO (BL.ByteString)
input2BL = fmap (convH [] (BL.filter isAsciiLower)) BL.getContents

-- | Like 'input2BL', but takes only first @n@ symbols specified with the first 'Int64' argument.
input2BLN :: Int64 -> IO (BL.ByteString)
input2BLN n = fmap (convH [] (BL.take n . BL.filter isAsciiLower)) BL.getContents

-- | Like 'input2BL', but takes only first @n@ symbols specified with the second 'Int64' argument dropping before this the first @m@ symbols specified 
-- with the first 'Int64' argument.
input2BLMN :: Int64 -> Int64 -> IO (BL.ByteString)
input2BLMN m n = fmap (convH [] (BL.take n . BL.drop m . BL.filter isAsciiLower)) BL.getContents

-- | Reads a given file into a lazy 'BL.ByteString' with filtering of all characters that are not a lower case ascii letters. It has additional 
-- first command line argument to control the way of treating letters: as being typed (entered) properly (null 'String'), or needed to be converted 
-- from qwerty to dvorak layout (\"q\" 'String'), or vice versa (otherwise).
readFile2BLGen :: String -> FilePath -> IO (BL.ByteString)
readFile2BLGen ys = fmap (convH ys (BL.filter isAsciiLower)) . BL.readFile

-- | Like 'readFile2BLGen', but reads only first @n@ symbols specified with the first 'Int64' argument.
readFile2BLGenN :: Int64 -> String -> FilePath -> IO (BL.ByteString)
readFile2BLGenN n ys = fmap (convH ys (BL.take n . BL.filter isAsciiLower)) . BL.readFile

-- | Like 'readFile2BLGen', but reads only first @n@ symbols specified with the second 'Int64' argument dropping before this the first @m@ symbols specified 
-- with the first 'Int64' argument.
readFile2BLGenMN :: Int64 -> Int64 -> String -> FilePath -> IO (BL.ByteString)
readFile2BLGenMN m n ys = fmap (convH ys (BL.take n . BL.drop m . BL.filter isAsciiLower)) . BL.readFile

-- | Auxiliary function to define how is a 'BL.ByteString' treated, see 'readFile2BLGen'.
convH :: String -> (BL.ByteString -> BL.ByteString) -> (BL.ByteString -> BL.ByteString)
convH ys f 
 | null ys = f
 | ys == "q" = qwerty2dvorak . f
 | otherwise = dvorak2qwerty . f
  
-- | Usual way the function 'readFile2BLGen' is used. The text in a file being read is treated as a properly typed (entered) one. So there is no 
-- keyboard layout conversion at all.
readFile2BL :: FilePath -> IO (BL.ByteString)
readFile2BL = readFile2BLGen []

-- | Like 'readFile2BL', but reads only first @n@ symbols specified with the first 'Int64' argument.
readFile2BLN :: Int64 -> FilePath -> IO (BL.ByteString)
readFile2BLN n = readFile2BLGenN n []

-- | Like 'readFile2BL', but reads only first @n@ symbols specified with the second 'Int64' argument dropping before this the first @m@ symbols specified 
-- with the first 'Int64' argument.
readFile2BLMN :: Int64 -> Int64 -> FilePath -> IO (BL.ByteString)
readFile2BLMN m n = readFile2BLGenMN m n []

-- | After reading a file into a filtered lazy 'BL.ByteString' (see, 'readFile2BLGen') converts the resulting 'BL.ByteString' into a 'V.Vector' 
-- of 'Int'. The arguments have the same meaning as for 'readFile2BLGen'.
readFileDoublesGen :: String -> FilePath -> IO (V.Vector Int)
readFileDoublesGen ys = fmap (takeDoubles . convH ys (BL.filter isAsciiLower)) . BL.readFile

-- | Like 'readFileDoublesGen', but returns only first @n@ elements of the 'V.Vector' specified with the first 'Int64' argument.
readFileDoublesGenN :: Int64 -> String -> FilePath -> IO (V.Vector Int)
readFileDoublesGenN n ys = fmap (V.unsafeSlice 0 (fromIntegral n) . takeDoubles . convH ys (BL.filter isAsciiLower)) . BL.readFile

-- | Like 'readFileDoublesGen', but returns only first @n@ symbols specified with the second 'Int64' argument dropping before this the first @m@ symbols specified 
-- with the first 'Int64' argument.
readFileDoublesGenMN :: Int64 -> Int64 -> String -> FilePath -> IO (V.Vector Int)
readFileDoublesGenMN m n ys = fmap (V.unsafeSlice (fromIntegral m) (fromIntegral n) . takeDoubles . convH ys (BL.filter isAsciiLower)) . BL.readFile

-- | Usual way the function 'readFileDoublesGen' is used. The text in a file being read is treated as a properly typed (entered) one. So there is no 
-- keyboard layout conversion at all.
readFileDoubles :: FilePath -> IO (V.Vector Int)
readFileDoubles = readFileDoublesGen []

-- | Like 'readFileDoubles', but returns only first @n@ elements of the 'V.Vector' specified with the first 'Int64' argument.
readFileDoublesN :: Int64 -> FilePath -> IO (V.Vector Int)
readFileDoublesN n = readFileDoublesGenN n []

-- | Like 'readFileDoubles', but returns only first @n@ elements of the 'V.Vector' specified with the second 'Int64' argument 
-- dropping before this the first @m@ elements specified with the first 'Int64' argument.
readFileDoublesMN :: Int64 -> Int64 -> FilePath -> IO (V.Vector Int)
readFileDoublesMN m n = readFileDoublesGenMN m n []
