-- Copyright (c) 2013, Finn Espen Gundersen
-- All rights reserved
-- Licensed under the 2-clause Simplified BSD (FreeBSD) License

-- | Pure module for encoding and decoding Google Polyline format as specified in
-- https://developers.google.com/maps/documentation/utilities/polylinealgorithm
module GPolyline (encodeline,encodeunsigned,decodeline,decodeunsigned) where

import Data.Word
import Data.Bits
import Data.Char
import Data.List.Split

type Point = (Double,Double)

example_decoded = [(38.5, -120.2), (40.7, -120.95), (43.252, -126.453)]
example_encoded = "_p~iF~ps|U_ulLnnqC_mqNvxq`@"

example_encoded2 = "ctteJe{{b@EESCKWAWCMAEGSQQ]Yo@"
example_decoded2 = [(58.765620000000006,5.88227),(58.76565000000001,5.8823),(58.76575000000001,5.88232),(58.76581000000001,5.88244),(58.76582000000001,5.88256),(58.76584000000001,5.88263),(58.765850000000015,5.88266),(58.76589000000001,5.882759999999999),(58.76598000000001,5.8828499999999995),(58.76613000000001,5.88298),(58.76637000000001,5.88298)]

example_encoded3 = "ctteJe{{b@E?E?SCK?WAWCMAEGSQQ]Yo@"


encodeline :: [Point] -> String
encodeline points = concatMap encodepoint rels
  where rels = transform points calcoffsets -- step1 turn into offsets from first point
        encodepoint (latoff,lngoff) = encodefloat latoff ++ encodefloat lngoff

decodeline :: String -> [Point]
decodeline str = transform points calcoffsets'
  where chunks = chunkinput $ prepareinput str
        floats = map (decodefloat) chunks
        points = pairup floats

decodeunsigned :: String -> Int -- convenience function when we know that a string has only one unsigned
decodeunsigned str = fromIntegral $ createvalue 5 (clrthem (prepareinput str))

encodeunsigned :: Int -> String -- convenience function when we have just an unsigned
encodeunsigned off =
  map (\b -> chr (fromIntegral(b+63))) w32l
  where w32l = shorten $ thedrop (chunkvalue 5 (fromIntegral off))
        shorten wrd
          | null wrd = [0]
          | otherwise = orthem $ reverse wrd
        thedrop wrd = dropWhile (==0) (reverse wrd) -- remove unnecessary blocks (part of step 6)

-- turns list of values into list of pairs
-- map (\[a,b] -> (a.b)) (chunksOf 2 <list>) is more succinct, but fails on odd-length
pairup :: [a] -> [(a,a)]
pairup [] = []
pairup (x:[]) = []  -- throw away odd element if any (should not appear in well-formed string)
pairup (x:y:xs) = (x,y) : pairup xs

-- Converts a list of relative vectors to list of absolute points and vice versa
transform :: [Point] -> (Point -> [Point] -> [Point]) -> [Point]
transform [] _ = []
transform (x:xs) transformer
  | null xs = [x]
  | otherwise = x : transformer x xs

-- Used to convert a list of absolute points to list of relative vectors
calcoffsets :: Point -> [Point] -> [Point]
calcoffsets _ [] = []
calcoffsets (xprev,yprev) lst =
  (x-xprev,y-yprev) : calcoffsets (x,y) (tail lst)
  where (x,y) = head lst

-- Used to convert a list of relative vectors to list of absolute points
calcoffsets' :: Point -> [Point] -> [Point]
calcoffsets' _ [] = []
calcoffsets' (xprev,yprev) lst =
  (x+xprev,y+yprev) : calcoffsets' (x+xprev,y+yprev) (tail lst)
  where (x,y) = head lst

encodefloat :: Double -> String -- steps 9,10,11: add 63 and convert to ascii
encodefloat off =
  map (\b -> chr (fromIntegral(b+63))) w32l
  where w32l = shorten $ thedrop (chunkvalue 5 (preparefloat off))
        shorten wrd
          | null wrd = [0]
          | otherwise = orthem $ reverse wrd
        thedrop wrd = dropWhile (==0) (reverse wrd) -- remove unnecessary blocks (part of step 6)

decodefloat :: [Word32] -> Double
decodefloat lst = 0.00001 * res
  where val = createvalue 5 (clrthem lst)
        num = shiftR val 1
        res
          | testBit val 0 = -fromIntegral (num+1)
          | otherwise = fromIntegral num

orthem :: [Word32] -> [Word32]  -- step8 bitwise or all blocks except last with 0x20
orthem [] = []
orthem (x:[]) = [x]
orthem (x:xs) = (x .|. 32) : orthem xs

clrthem :: [Word32] -> [Word32] -- reverse of step8
clrthem [] = []
clrthem (x:[]) = [x]
clrthem (x:xs) = (clearBit x 5) : clrthem xs

chunkvalue :: Int -> Word32 -> [Word32] -- step6+7 break into 5bit chunks and reverse
chunkvalue bitspersegment wrd =
  [(shiftR wrd b) .&. mask | b <- [0,bitspersegment..maxbits]]
  where mask = (bit bitspersegment) - 1
        maxbits = 25  -- should be 31 in general, but always max 25 for GPolyline

createvalue :: Int -> [Word32] -> Word32 -- reverse of step6+7, put reverse list of chunks together to one value
createvalue bitspersegment chunks =
  sum $ zipWith (*) chunks [mul^e | e <- [0..]]
  where mul = bit bitspersegment :: Word32

-- First steps, turning double into word32 (with max 25 bits + 1bit pos/neg content)
preparefloat :: Double -> Word32
preparefloat val = bin3
  where int = round (val * 100000)  -- step2 multiply by 1e5 and round
        bin = fromIntegral int :: Word32 -- step3 convert to binary (2's complement for negs)
        bin2 = shiftL bin 1 -- step4 left shift
        bin3  -- step5 complement if negative
          | val < 0 = complement bin2
          | otherwise = bin2

chunkinput :: [Word32] -> [[Word32]]
chunkinput vals = splt (\v -> not $ testBit v 5) vals
  where splt = split . keepDelimsR . whenElt

prepareinput :: String -> [Word32]
prepareinput str = map fromIntegral vals
  where vals = map (\c -> (-63) + ord c) str
