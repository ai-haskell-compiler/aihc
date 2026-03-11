-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Signal.EEG.BDF
-- Copyright   :  (c) Alexander Vivian Hugh McPhail 2010, 2014, 2015
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  
--
-- EEG BDF data file functions
--
-----------------------------------------------------------------------------

module Numeric.Signal.EEG.BDF (
                               BDF(..)
                              , loadBDFRaw
                              , loadBDF
                          ) where

import qualified Data.ByteString as BS

--import Data.Char
--import Data.List
import Data.Word
import Data.Bits
--import Data.Array.Storable
import Numeric.LinearAlgebra.Data

import qualified Numeric.Signal.Multichannel as M

import Control.Monad hiding(join)
import Control.Monad.State hiding(join)

--import System.IO

--import Debug.Trace

{- BDF Header format: http://www.biosemi.com/faq/file_format.htm
Length in bytes  	BDF Header:  	EDF Header:  	Description
8 bytes 	Byte 1: "255" (non ascii) 	Byte 1: "0" (ASCII) 	Identification code
Bytes 2-8 : "BIOSEMI" (ASCII) 	Bytes 2-8 : " "(ASCII)
80 bytes 	
User text input (ASCII)
	Local subject identification
80 bytes 	
User text input (ASCII)
	Local recording identification
8 bytes 	
dd.mm.yy (ASCII)
	Startdate of recording
8 bytes 	
hh.mm.ss (ASCII)
	Starttime of recording
8 bytes 	
(ASCII)
	Number of bytes in header record
44 bytes 	
"24BIT" (ASCII)
	
"BIOSEMI" (ASCII)
	Version of data format.
8 bytes 	
(ASCII)
	Number of data records "-1" if unknown
8 bytes 	
e.g.: "1" (ASCII)
	Duration of a data record, in seconds
4 bytes 	
e.g.: "257" or "128" (ASCII)
	Number of channels (N) in data record
N x 16 bytes 	
e.g.: "Fp1", "Fpz", "Fp2", etc (ASCII)
	Labels of the channels
N x 80 bytes 	
e.g.: "active electrode", "respiration belt" (ASCII)
	Transducer type
N x 8 bytes 	
e.g.: "uV", "Ohm" (ASCII)
	Physical dimension of channels
N x 8 bytes 	e.g.: "-262144" (ASCII) 	e.g.: "-32768" (ASCII) 	Physical minimum in units of physical dimension
N x 8 bytes 	e.g.: "262143" (ASCII) 	e.g.: "32767" (ASCII) 	Physical maximum in units of physical dimension
N x 8 bytes 	e.g.: "-8388608" (ASCII) 	e.g.: "-32768" (ASCII) 	Digital minimum
N x 8 bytes 	e.g.: "8388607" (ASCII) 	e.g.: "32767" (ASCII) 	Digital maximum
N x 80 bytes 	e.g.: "HP:DC; LP:410" 	e.g.: "HP:0,16; LP:500" 	Prefiltering
N x 8 bytes 	
For example: "2048" (ASCII)
	
Number of samples in each data record
(Sample-rate if Duration of data record = "1")
N x 32 bytes 	
(ASCII)
	Reserved
-}

type BSM a = StateT BS.ByteString IO a

getN :: Int -> BSM BS.ByteString
getN n = do
         bs <- get
         let (v,bs') = BS.splitAt n bs
         put bs'
         return v

data Date = Date { day :: Int, month :: Int, year :: Int }
          deriving(Eq,Ord,Show)

data Time = Time { hour :: Int, minute :: Int, second :: Int }
          deriving(Eq,Ord,Show)

-- | BDF datatype.  NOTE: data has already been converted to Float
data BDF = BDF {
                id_ :: !Word8
                , type_ :: !String
                , subject :: !String
                , recording :: !String
                , date :: !Date
                , time :: !Time
                , head_bytes :: !Int
                , data_version :: !String
                , num_records :: !Int
                , duration :: !Int
                , channels :: !Int
                , chan_labels :: ![String]
                , tran_type :: ![String]
                , dimensions :: ![String]
                , phys_min :: ![Int]
                , phys_max :: ![Int]
                , dig_min :: ![Int]
                , dig_max :: ![Int]
                , prefilter :: ![String]
                , samples :: ![Int]
                , reserved :: ![String]
                , data_ :: ![Vector Float]
               } --deriving(Show)

getString :: Int -> BSM String
getString n = do
              bs <- getN n
              return $ (reverse . dropWhile (== ' ') . tail . reverse . tail . show) bs

getInt :: Int -> BSM Int
getInt n = do
           s <- getString n
           return $ read s

getDate = do
          s <- getString 8
          return $ strToDate' s

strToDate' :: String -> Date
strToDate' (d1:d2:'.':m1:m2:'.':y1:y2:[]) = Date (read [d1,d2]) (read [m1,m2]) (read [y1,y2])
strToDate' _                              = error "strToDate"

getTime = do
          s <- getString 8
          return $ strToTime' s
 
strToTime' :: String -> Time
strToTime' (h1:h2:'.':m1:m2:'.':s1:s2:[]) = Time (read [h1,h2]) (read [m1,m2]) (read [s1,s2])
strToTime' _                              = error "strToTime"

reverseBits :: Word8 -> Word8
--reverseBits w = foldl setBit 0 $ snd . unzip . filter ((== False) . fst) $ zip (map (testBit w) [1..8]) $ reverse [1..8]
--reverseBits = foldl setBit 0 . snd . unzip . filter ((False ==) . fst) . ($ reverse [0..7]) . zip . flip map [0..7] . testBit
--reverseBits w = foldl setBit 0 . foldl ((++) . \b -> if testBit w b then [9-b] else []) [] [1..8]
reverseBits w = foldr (\b r -> if not $ testBit w b then setBit r (7-b) else r) 0 [0..7]

get24Bit :: (Int -> Float) -> BSM Float
get24Bit f = do
             b1 <- getN 1
             b2 <- getN 1
             b3 <- getN 1
--           return $ (fromIntegral $ BS.head b1) `shiftL` 16 .|. (fromIntegral $ BS.head b2) `shiftL` 8 .|. (fromIntegral $ BS.head b3)
             return $ f $ (to32 b3) `shiftL` 16 .|. (to32 b2) `shiftL` 8 .|. (to32 b1)
    where to32 = fromIntegral {- . reverseBits -} . BS.head

readRecord :: (Int -> Float) -> Int -> BSM (Vector Float)
readRecord f s = do
                 m <- replicateM s $ get24Bit f
                 return $! fromList m

readRecordBlock :: (Int -> Int -> Float) -> [(Int,Int)] -> BSM [Vector Float]
readRecordBlock f ss = mapM (\(i,s) -> readRecord (f i) s) ss

convert :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Float
convert p_min p_max d_min d_max i = \x -> (fromIntegral x) - (fromIntegral (d_min !! i))*(fromIntegral ((p_max !! i) - (p_min !! i)))/(fromIntegral ((d_max !! i) - (d_min !! i)))

readData :: (Int -> Int -> Float) -> Int -> [(Int,Int)] -> BSM [Vector Float]
readData f rs ss = do
                   lift $ putStrLn $ "channels: " ++ (show $ length ss) ++ ", records: " ++ (show rs) ++ ", samples per record: " ++ (show $ snd $ head ss)
                   d <- mapM (\x -> do
                                    lift $ putStrLn $ "Record: " ++ show x 
                                    readRecordBlock f ss) [1..rs]
                   -- let v = rotate d
                   -- lift $ putStrLn $ "vectors: " ++ (show $ length v)
                   -- lift $ putStrLn $ "slices: " ++ (show $ length $ head v)
                   return $! map vjoin $! rotate_ d
    where rotate_ []            = []
          rotate_ xs@((_:[]):_) = [concat xs]
          rotate_ ((x:xs):xss)  = (x : (map head xss)) : (rotate_ (xs : (map tail xss)))
 
{-
readData _ 0     _  = error "readData, zeroth record requested"
readData f 1     ss = do
                      lift $ putStrLn $ "data record: 1"
                      readRecordBlock f ss
readData f (n+1) ss = do
                      lift $ putStrLn $ "data record: " ++ show (n+1)
                      bs <- readRecordBlock f ss
                      ds <- readData f n ss
                      let result = zipWith (\x y -> join [x,y]) bs ds
                      return $ result
-}

readBDF :: BSM (Maybe BDF)
readBDF = do
          id_' <- getN 1
          type_' <- getString 7
          if (not $ BS.head id_' == 255 && type_' == "BIOSEMI")
               then do  
                    lift $ putStrLn "Error: File not BDF Format"                     
                    return Nothing
               else do
                    subject' <- getString 80
                    recording' <- getString 80
                    date' <- getDate
                    time' <- getTime
                    head_bytes' <- getInt 8
                    data_version' <- getString 44
                    num_records' <- getInt 8
                    if (num_records' == -1)
                       then do
                            lift $ putStrLn "This file is probably a valid BDF file..."
                            lift $ putStrLn "   but this program cannot grok an unspecified"
                            lift $ putStrLn "   number of records"
                            lift $ putStrLn "So complain to the software author"
                            return Nothing
                       else do
                            duration' <- getInt 8 
                            channels' <- getInt 4
                            chan_labels' <- replicateM channels' $ getString 16
                            tran_type' <- replicateM channels' $ getString 80
                            dimensions' <- replicateM channels' $ getString 8
                            phys_min' <- replicateM channels' $ getInt 8
                            phys_max' <- replicateM channels' $ getInt 8
                            dig_min' <- replicateM channels' $ getInt 8
                            dig_max' <- replicateM channels' $ getInt 8
                            prefilter' <- replicateM channels' $ getString 80
                            samples' <- replicateM channels' $ getInt 8
                            reserved' <- replicateM channels' $ getString 32
                            data_' <- readData (convert phys_min' phys_max' dig_min' dig_max') num_records' (zip [0..] samples')
                            return $ Just $ BDF {
                                                 id_ = BS.head id_'
                                                 , type_ = type_'
                                                 , subject = subject'
                                                 , recording = recording'
                                                 , date = date'
                                                 , time = time'
                                                 , head_bytes = head_bytes'
                                                 , data_version = data_version'
                                                 , num_records = num_records'
                                                 , duration = duration'
                                                 , channels = channels'
                                                 , chan_labels = chan_labels'
                                                 , tran_type = tran_type'
                                                 , dimensions = dimensions'
                                                 , phys_min = phys_min'
                                                 , phys_max = phys_max'
                                                 , dig_min = dig_min'
                                                 , dig_max = dig_max'
                                                 , prefilter = prefilter'
                                                 , samples = samples'
                                                 , reserved = reserved'
                                                 , data_ = data_'
                                                }

loadBDF :: FilePath -> IO (Maybe (M.Multichannel Float))
loadBDF fn = do
             bs <- BS.readFile fn
             (bdf,bs') <- runStateT readBDF bs
             m <- case bdf of
                           (Just b) -> do
                                       return $ Just $ M.createMultichannel (head $ samples b) 24 (data_ b)
                           _        -> do
                                       putStrLn "File not read"
                                       return Nothing
             when (not (BS.null bs')) $ do
                  putStrLn "data remaining..."
             return m

-- | Load a BDF data structure
--   NOTE: data has already been converted to Float
loadBDFRaw :: FilePath -> IO (Maybe BDF)
loadBDFRaw fn = do
  bs <- BS.readFile fn
  (bdf,bs') <- runStateT readBDF bs
  m <- case bdf of
        Just b -> return (Just b)
        _      -> do
                  putStrLn "File not read"
                  return Nothing
  when (not (BS.null bs')) $ do
    putStrLn "data remaining..."
  return m


{-
getDPConversion :: BDF -> [Word32 -> Float]
getDPConversion b = let chan = channels b
                        tup4 = zip4 (phys_min b) (phys_max b) (dig_min b) (dig_max b) 
                    in map (\(a,b,c,d) -> \x -> ((fromIntegral x) - (fromIntegral c))*(fromIntegral (b-a))/(fromIntegral (d-c))) tup4


writeLine :: [Float] -> String
writeLine = ((++ "\n") . concat . intersperse " " . map show) 

main = do
       bs <- BS.readFile "VivianMeditation.bdf"
       (bdf,bs') <- runStateT readBDF bs
       case bdf of
                (Just b) -> do 
                            let conv = getDPConversion b
                            let b' = map (zipWith ($) conv) (data_ b)
                            putStrLn $ show $ length $ head b'
                            writeFile "VivianMeditation.txt" $ concat $ map writeLine b'
                _        -> putStrLn "File not read"
       when (not (BS.null bs')) $
            do
            putStrLn "data remaining..."
            return ()
       return ()
-}

