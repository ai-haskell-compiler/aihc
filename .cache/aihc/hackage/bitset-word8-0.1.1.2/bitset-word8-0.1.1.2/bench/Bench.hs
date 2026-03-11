{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

import           Control.DeepSeq     (NFData, force)
import           Criterion.Main
import           Data.Bits           (testBit)
import           Data.ByteString     (ByteString, index, pack)
import           Data.Vector.Unboxed (Vector, fromList, (!))
import           Data.Word           (Word64, Word8)
import           GHC.Generics        (Generic)

{-
    Unboxed Word64 backend
-}
data BitSetWord8Word64 = BitSetWord8Word64 {-# UNPACK #-} !Word64
                                           {-# UNPACK #-} !Word64
                                           {-# UNPACK #-} !Word64
                                           {-# UNPACK #-} !Word64
    deriving (Eq, Generic, NFData, Show)

memberWord64 :: BitSetWord8Word64 -> Word8 -> Bool
memberWord64 bitSet val = doMember bitSet (val `div` 64) (val `mod` 64)
  where
    doMember :: BitSetWord8Word64 -> Word8 -> Word8 -> Bool
    doMember (BitSetWord8Word64 w _ _ _) 0 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word64 _ w _ _) 1 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word64 _ _ w _) 2 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word64 _ _ _ w) 3 ind = testBit w (fromIntegral ind)

allZeroWord64 :: BitSetWord8Word64
allZeroWord64 = force $ BitSetWord8Word64 0 0 0 0

allOneWord64 :: BitSetWord8Word64
allOneWord64 = force $ BitSetWord8Word64 maxBound maxBound maxBound maxBound

{-
    ByteString backend
-}
newtype BitSetWord8ByteString = BitSetWord8ByteString ByteString deriving (Eq, Generic, NFData, Show)

memberByteString :: BitSetWord8ByteString -> Word8 -> Bool
memberByteString (BitSetWord8ByteString bs) w = testBit (index bs (fromIntegral (w `div` 8))) (fromIntegral (w `mod` 8))

allZeroByteString :: BitSetWord8ByteString
allZeroByteString = force $ BitSetWord8ByteString $ pack [ 0,0,0,0,0,0,0,0
                                                         , 0,0,0,0,0,0,0,0
                                                         , 0,0,0,0,0,0,0,0
                                                         , 0,0,0,0,0,0,0,0
                                                         ]

allOneByteString :: BitSetWord8ByteString
allOneByteString = force $ BitSetWord8ByteString $ pack [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                        , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                        , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                        , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                        ]

{-
    Vector of Word64 backend
-}
newtype BitSetWord8Vector64 = BitSetWord8Vector64 (Vector Word64) deriving (Eq, Generic, NFData, Show)

memberVector64 :: BitSetWord8Vector64 -> Word8 -> Bool
memberVector64 (BitSetWord8Vector64 v) w = testBit (v ! fromIntegral (w `div` 64)) (fromIntegral (w `mod` 64))

allZeroVector64 :: BitSetWord8Vector64
allZeroVector64 = force $ BitSetWord8Vector64 $ fromList [ 0,0,0,0 ]

allOneVector64 :: BitSetWord8Vector64
allOneVector64 = force $ BitSetWord8Vector64 $ fromList [ maxBound, maxBound, maxBound, maxBound ]

{-
    Vector of Word8 backend
-}
newtype BitSetWord8Vector8 = BitSetWord8Vector8 (Vector Word8) deriving (Eq, Generic, NFData, Show)

memberVector8 :: BitSetWord8Vector8 -> Word8 -> Bool
memberVector8 (BitSetWord8Vector8 v) w = testBit (v ! fromIntegral (w `div` 8)) (fromIntegral (w `mod` 8))

allZeroVector8 :: BitSetWord8Vector8
allZeroVector8 = force $ BitSetWord8Vector8 $ fromList [ 0,0,0,0,0,0,0,0
                                                       , 0,0,0,0,0,0,0,0
                                                       , 0,0,0,0,0,0,0,0
                                                       , 0,0,0,0,0,0,0,0
                                                       ]

allOneVector8 :: BitSetWord8Vector8
allOneVector8 = force $ BitSetWord8Vector8 $ fromList [ 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                      , 0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                                                      ]

{-
    Unboxed Word8 backend
-}
data BitSetWord8Word8 = BitSetWord8Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                         {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    deriving (Eq, Generic, NFData, Show)

memberWord8 :: BitSetWord8Word8 -> Word8 -> Bool
memberWord8 bitSet val = doMember bitSet (val `div` 8) (val `mod` 8)
  where
    doMember :: BitSetWord8Word8 -> Word8 -> Word8 -> Bool
    doMember (BitSetWord8Word8 w _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  0 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ w _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  1 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ w _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  2 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ w _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  3 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ w _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  4 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ w _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  5 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ w _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  6 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ w  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  7 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  w _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  8 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ w _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _)  9 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ w _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 10 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ w _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 11 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ w _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 12 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ w _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 13 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ w _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 14 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ w  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 15 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  w _ _ _ _ _ _ _  _ _ _ _ _ _ _ _) 16 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ w _ _ _ _ _ _  _ _ _ _ _ _ _ _) 17 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ w _ _ _ _ _  _ _ _ _ _ _ _ _) 18 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ w _ _ _ _  _ _ _ _ _ _ _ _) 19 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ w _ _ _  _ _ _ _ _ _ _ _) 20 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ w _ _  _ _ _ _ _ _ _ _) 21 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ w _  _ _ _ _ _ _ _ _) 22 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ w  _ _ _ _ _ _ _ _) 23 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  w _ _ _ _ _ _ _) 24 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ w _ _ _ _ _ _) 25 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ w _ _ _ _ _) 26 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ w _ _ _ _) 27 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ w _ _ _) 28 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ w _ _) 29 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ w _) 30 ind = testBit w (fromIntegral ind)
    doMember (BitSetWord8Word8 _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ _  _ _ _ _ _ _ _ w) 31 ind = testBit w (fromIntegral ind)

allZeroWord8 :: BitSetWord8Word8
allZeroWord8 = force $ BitSetWord8Word8 0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0  0 0 0 0 0 0 0 0

allOneWord8 :: BitSetWord8Word8
allOneWord8 = force $ BitSetWord8Word8 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
                                       0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
                                       0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff
                                       0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff

main :: IO ()
main = defaultMain
    [ bgroup "Word64" [ bench "All zero, LSB" $ whnf (memberWord64 allZeroWord64) 0
                      , bench "All zero, 64" $ whnf (memberWord64 allZeroWord64) 64
                      , bench "All zero, 128" $ whnf (memberWord64 allZeroWord64) 128
                      , bench "All zero, MSB" $ whnf (memberWord64 allZeroWord64) 255
                      , bench "All one, LSB" $ whnf (memberWord64 allOneWord64) 0
                      , bench "All one, 64" $ whnf (memberWord64 allOneWord64) 64
                      , bench "All one, 128" $ whnf (memberWord64 allOneWord64) 128
                      , bench "All one, MSB" $ whnf (memberWord64 allOneWord64) 255
                      ]
    , bgroup "ByteString" [ bench "All zero, LSB" $ whnf (memberByteString allZeroByteString) 0
                          , bench "All zero, 64" $ whnf (memberByteString allZeroByteString) 64
                          , bench "All zero, 128" $ whnf (memberByteString allZeroByteString) 128
                          , bench "All zero, MSB" $ whnf (memberByteString allZeroByteString) 255
                          , bench "All one, LSB" $ whnf (memberByteString allOneByteString) 0
                          , bench "All one, 64" $ whnf (memberByteString allOneByteString) 64
                          , bench "All one, 128" $ whnf (memberByteString allOneByteString) 128
                          , bench "All one, MSB" $ whnf (memberByteString allOneByteString) 255
                          ]
    , bgroup "Vector64" [ bench "All zero, LSB" $ whnf (memberVector64 allZeroVector64) 0
                        , bench "All zero, 64" $ whnf (memberVector64 allZeroVector64) 64
                        , bench "All zero, 128" $ whnf (memberVector64 allZeroVector64) 128
                        , bench "All zero, MSB" $ whnf (memberVector64 allZeroVector64) 255
                        , bench "All one, LSB" $ whnf (memberVector64 allOneVector64) 0
                        , bench "All one, 64" $ whnf (memberVector64 allOneVector64) 64
                        , bench "All one, 128" $ whnf (memberVector64 allOneVector64) 128
                        , bench "All one, MSB" $ whnf (memberVector64 allOneVector64) 255
                        ]
    , bgroup "Vector8" [ bench "All zero, LSB" $ whnf (memberVector8 allZeroVector8) 0
                       , bench "All zero, 64" $ whnf (memberVector8 allZeroVector8) 64
                       , bench "All zero, 128" $ whnf (memberVector8 allZeroVector8) 128
                       , bench "All zero, MSB" $ whnf (memberVector8 allZeroVector8) 255
                       , bench "All one, LSB" $ whnf (memberVector8 allOneVector8) 0
                       , bench "All one, 64" $ whnf (memberVector8 allOneVector8) 64
                       , bench "All one, 128" $ whnf (memberVector8 allOneVector8) 128
                       , bench "All one, MSB" $ whnf (memberVector8 allOneVector8) 255
                       ]
    , bgroup "Word8" [ bench "All zero, LSB" $ whnf (memberWord8 allZeroWord8) 0
                     , bench "All zero, 64" $ whnf (memberWord8 allZeroWord8) 64
                     , bench "All zero, 128" $ whnf (memberWord8 allZeroWord8) 128
                     , bench "All zero, MSB" $ whnf (memberWord8 allZeroWord8) 255
                     , bench "All one, LSB" $ whnf (memberWord8 allOneWord8) 0
                     , bench "All one, 64" $ whnf (memberWord8 allOneWord8) 64
                     , bench "All one, 128" $ whnf (memberWord8 allOneWord8) 128
                     , bench "All one, MSB" $ whnf (memberWord8 allOneWord8) 255
                     ]
    ]
