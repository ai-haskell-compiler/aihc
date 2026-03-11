{-# LANGUAGE DeriveAnyClass, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}


-------------------------------------------------------------------------------
-- |
-- Module      :  Graphics.OpenEXR
-- Copyright   :  (c) 2018 Pavol Klacansky
-- License     :  PublicDomain
--
-- Maintainer  :  pavol@klacansky.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Library for writting OpenEXR images which support high dynamic range. These
-- images are common in computer graphics, especially ray tracing, and can be
-- used to delay quantization to the post-processing stage.
--
--
-- An example of writting 1x1 ZIP compressed image consisting of a red pixel.
--
-- > module Main where
-- >
-- > import qualified Data.Vector      as V
-- > import qualified Graphics.OpenEXR as EXR
-- >
-- >
-- > main :: IO ()
-- > main = do
-- >         let image = EXR.ImageRGBF 1 1 (V.fromList [EXR.PixelRGBF 1.0 0.0 0.0])
-- >         EXR.writeFile "image.exr" image EXR.ZipCompression
-------------------------------------------------------------------------------




module Graphics.OpenEXR
        ( Image(..)
        , PixelRGBF(..)
        , CompressionType(..)
        , writeFile
        ) where 




import           Prelude                hiding (writeFile)

import           Codec.Compression.Zlib (compress)
import           Control.DeepSeq        (NFData)
import           Data.Binary            (Binary, get, put)
import           Data.Binary.IEEE754    (putFloat32le)
import           Data.Binary.Put        (Put, putLazyByteString, putWord8, putWord32le, putWord64le, runPut)
import qualified Data.ByteString.Lazy   as BL
import           Data.List.Split        (chunksOf)
import qualified Data.Vector            as V
import qualified Data.Vector.Split      as V -- TODO: implement internally (trivial) to reduce dependency if we want this module as separate package
import           Data.Word              (Word8, Word32)
import           GHC.Generics           (Generic)




data Image = ImageRGBF
        { imageWidth  :: !Int
        , imageHeight :: !Int
        , imageData   :: V.Vector PixelRGBF  -- ^ Stored in row-major layout
        } deriving (Generic, NFData, Show)


data PixelRGBF = PixelRGBF !Float !Float !Float
        deriving (Generic, NFData, Show)


data CompressionType = NoCompression    -- ^ No compression applied
                     | ZipsCompression  -- ^ DEFLATE lossless compression applied per scanline
                     | ZipCompression   -- ^ DEFLATE lossless compression applied per 16 scanlines


data Attribute = Attribute BL.ByteString AttributeType


data AttributeType = Box2i (Word32, Word32, Word32, Word32)
                   | Chlist [Channel]
                   | Compression CompressionType
                   | Float Float
                   | LineOrder LineOrderType
                   | V2f (Float, Float)


data Channel = Channel
        { name      :: BL.ByteString
        , pixelType :: PixelType
        , pLinear   :: Word8
        , reserved  :: (Word8, Word8, Word8)
        , xSampling :: Word32
        , ySampling :: Word32
        }


data PixelType = PixelUint | PixelHalf | PixelFloat
        deriving Enum


data LineOrderType = IncreasingY | DecreasingY | RandomY
        deriving Enum




instance Binary Attribute where
        get = undefined
        put (Attribute n t) = putLazyByteString (BL.snoc n 0) >> put t


instance Binary AttributeType where
        get = undefined
        put a@(Box2i (x, y, z, w)) = putLazyByteString "box2i\0" >> putWord32le (attrSize a) >> putWord32le x >> putWord32le y >> putWord32le z >> putWord32le w
        put a@(Chlist xs) = putLazyByteString "chlist\0" >> putWord32le (attrSize a) >> mapM_ put xs >> put '\0'
        put a@(Compression x) = putLazyByteString "compression\0" >> putWord32le (attrSize a) >> put x
        put a@(Float x) = putLazyByteString "float\0" >> putWord32le (attrSize a) >> putFloat32le x
        put a@(LineOrder x) = putLazyByteString "lineOrder\0" >> putWord32le (attrSize a) >> put x
        put a@(V2f (x, y)) = putLazyByteString "v2f\0" >> putWord32le (attrSize a) >> putFloat32le x >> putFloat32le y


instance Binary Channel where
        get = undefined
        put x = do
                putLazyByteString (BL.snoc (name x) 0)
                put (pixelType x)
                put (pLinear x)
                put (reserved x)
                putWord32le (xSampling x)
                putWord32le (ySampling x)


instance Binary PixelType where
        get = undefined
        put = putWord32le . fromIntegral . fromEnum


instance Enum CompressionType where
        fromEnum NoCompression   = 0
        fromEnum ZipsCompression = 2
        fromEnum ZipCompression  = 3
        toEnum _ = undefined


instance Binary CompressionType where
        get = undefined
        put = putWord8 . fromIntegral . fromEnum


instance Binary LineOrderType where
        get = undefined
        put = putWord8 . fromIntegral . fromEnum




-- | Write an 'Image' using a 'CompressionType' to an OpenEXR formatted file
writeFile :: FilePath -> Image -> CompressionType -> IO ()
writeFile filepath img compression = BL.writeFile filepath (header `BL.append` offTable `BL.append` (BL.concat cs))
        where header = runPut (magicNumber >> versionField >> mapM_ put attributes >> put '\0')
              offTable = runPut (offsetTable (fromIntegral $ BL.length header) ((fromIntegral . BL.length) <$> cs))
              cs = runPut <$> chunks img compression
              attributes = [ Attribute "channels" channels
                           , Attribute "compression" (Compression compression)
                           , Attribute "dataWindow" (Box2i (0, 0, w, h))
                           , Attribute "displayWindow" (Box2i (0, 0, w, h))
                           , Attribute "lineOrder" (LineOrder IncreasingY)
                           , Attribute "pixelAspectRatio" (Float 1)
                           , Attribute "screenWindowCenter" (V2f (0, 0))
                           , Attribute "screenWindowWidth" (Float 1)]
              channels = Chlist [ Channel "B" PixelFloat 0 (0, 0, 0) 1 1
                                , Channel "G" PixelFloat 0 (0, 0, 0) 1 1
                                , Channel "R" PixelFloat 0 (0, 0, 0) 1 1
                                ]
              w = fromIntegral (imageWidth img) - 1
              h = fromIntegral (imageHeight img) - 1




attrSize :: AttributeType -> Word32
attrSize (Box2i _) = 16
attrSize (Chlist xs) = fromIntegral ((sum . map (\x -> (BL.length . name $ x) + 1 + 4 + 1 + 3 + 4 + 4) $ xs) + 1)
attrSize (Compression _) = 1
attrSize (Float _) = 4
attrSize (LineOrder _) = 1
attrSize (V2f _) = 8




magicNumber :: Put
magicNumber = mapM_ putWord8 [0x76, 0x2F, 0x31, 0x01]




versionField :: Put
versionField = mapM_ putWord8 [0x02, 0x00, 0x00, 0x00]




-- TODO: partial function (init)
offsetTable :: Int -> [Int] -> Put
offsetTable offset chunksLengths = mapM_ (\x -> putWord64le (fromIntegral (offset + 8*nChunks + x))) . scanl (+) 0 . init $ chunksLengths
        where nChunks = length chunksLengths




scanlinesPerChunk :: Num a => CompressionType -> a
scanlinesPerChunk ZipCompression = 16
scanlinesPerChunk _ = 1




transform :: CompressionType -> (BL.ByteString -> BL.ByteString)
transform ZipsCompression = compress . preZip
transform ZipCompression = compress . preZip
transform _ = id




chunks :: Image -> CompressionType -> [Put]
chunks (ImageRGBF width _ px) c = uncurry f <$> zip [0,yStride..] (chunksOf yStride cs)
        where cs = V.chunksOf width px
              yStride = scanlinesPerChunk c
              f y xs = do
                let d = runPut . mapM_ scanline $ xs
                    transformed = transform c d
                    result = if BL.length d <= BL.length transformed then d else transformed
                putWord32le (fromIntegral y)
                putWord32le (fromIntegral (BL.length result))
                putLazyByteString result




scanline :: V.Vector PixelRGBF -> Put
scanline xs = do
        mapM_ (\(PixelRGBF _ _ b) -> putFloat32le b) xs
        mapM_ (\(PixelRGBF _ g _) -> putFloat32le g) xs
        mapM_ (\(PixelRGBF r _ _) -> putFloat32le r) xs




-- TODO: potentially optimize by not creating the temporary copy and instead use strided access
-- TODO: converting to lists may be inefficient, but lazyness should take care of it
preZip :: BL.ByteString -> BL.ByteString
preZip d = BL.pack (x:zipWith predictor (x:xs ++ ys) (xs ++ ys))
        where (x:xs, ys) = deinterleave (BL.unpack d)
              predictor p value = fromIntegral value - fromIntegral p + (128 + 256)




deinterleave :: [a] -> ([a], [a])
deinterleave []       = ([], [])
deinterleave [x]      = ([x], [])
deinterleave [x,y]    = ([x], [y])
deinterleave (x:y:xs) = let (as, bs) = deinterleave xs in (x:as, y:bs)
