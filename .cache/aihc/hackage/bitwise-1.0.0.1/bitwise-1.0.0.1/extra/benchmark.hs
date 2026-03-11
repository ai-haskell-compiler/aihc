{-# LANGUAGE BangPatterns #-}
module Main(main) where

import Control.Exception (evaluate)
import Control.Monad.ST (runST, ST)
import Data.Bits (shiftR)
import Data.ByteString (ByteString, pack, unpack)
import Data.Ix (range)
import Data.List (foldl1')
import Data.Word (Word8)
import System.Environment (getArgs)

import Data.Bits.Bitwise (packWord8LE, unpackWord8LE)
import qualified Data.Array.Unboxed  as A
import qualified Data.Array.BitArray as B
import qualified Data.Array.ST as STA (STUArray, readArray)
import qualified Data.Array.Unsafe as STA (unsafeThaw)
import qualified Data.Array.BitArray.ST as STB

import qualified Data.Array.BitArray.ByteString as BSB

import Criterion.Main

type I = (Int, Int, Int, Int)
type A = A.UArray I Bool
type B = B.BitArray I

bs :: (I, I)
bs = ((0, 0, 0, 0), (na - 1, nb - 1, nc - 1, nd - 1))

na, nb, nc, nd, n :: Int
na = 13
nb = 17
nc = 19
nd = 11
n = na * nb * nc * nd

next :: I -> I
next !(!a, !b, !c, !d) = ((a + 1) `mod` na, (b + 1) `mod` nb, (c + 1) `mod` nc, (d + 1) `mod` nd)

aToB :: A -> B
aToB a = {-# SCC "aToB" #-} B.listArray (A.bounds a) (A.elems a)

bToA :: B -> A
bToA b = {-# SCC "bToA" #-} A.listArray (B.bounds b) (B.elems b)

aToBS :: A -> ByteString
aToBS a = {-# SCC "aToBS" #-} pack . toWord8sLE . A.elems $ a

bToBS :: B -> ByteString
bToBS b = {-# SCC "bToBS" #-} BSB.toByteString b

aFromBS :: ((I, I), ByteString) -> A
aFromBS (i, a) = {-# SCC "aFromBS" #-} A.listArray i . fromWord8sLE . unpack $ a

bFromBS :: ((I, I), ByteString) -> B
bFromBS (i, b) = {-# SCC "bFromBS" #-} BSB.fromByteString i b

aListArray :: [Bool] -> A
aListArray a = {-# SCC "aListArray" #-} A.listArray bs a

bListArray :: [Bool] -> B
bListArray b = {-# SCC "bListArray" #-} B.listArray bs b

aIndex :: A -> ()
aIndex !a = {-# SCC "aIndex" #-} loop (1, 1, 1, 1) (n `div` 3)
  where
    loop  _  0 = ()
    loop !i !m = (a A.! i) `seq` loop (next i) (m - 1)

bIndex :: B -> ()
bIndex !b = {-# SCC "bIndex" #-} loop (1, 1, 1, 1) (n `div` 3)
  where
    loop  _  0 = ()
    loop !i !m = (b B.! i) `seq` loop (next i) (m - 1)

bIndex' :: B -> ()
bIndex' !b = {-# SCC "bIndex'" #-} loop (1, 1, 1, 1) (n `div` 3)
  where
    loop  _  0 = ()
    loop !i !m = (b B.!!! i) `seq` loop (next i) (m - 1)

aIndexST :: A -> ()
aIndexST !a = {-# SCC "aIndexST" #-} runST $ do
  !a' <- STA.unsafeThaw a :: ST s (STA.STUArray s I Bool)
  loop a' (1, 1, 1, 1) (n `div` 3)
  where
    loop _ _  0 = return ()
    loop !a' !i !m = do
      !_ <- STA.readArray a' i
      loop a' (next i) (m - 1)

bIndexST :: B -> ()
bIndexST !b = {-# SCC "bIndexST" #-} runST $ do
  !b' <- STB.unsafeThaw b
  loop b' (1, 1, 1, 1) (n `div` 3)
  where
    loop _ _  0 = return ()
    loop !b' !i !m = do
      !_ <- STB.readArray b' i
      loop b' (next i) (m - 1)

bIndexST' :: B -> ()
bIndexST' !b = {-# SCC "bIndexST'" #-} runST $ do
  !b' <- STB.unsafeThaw b
  loop b' (1, 1, 1, 1) (n `div` 3)
  where
    loop _ _  0 = return ()
    loop !b' !i !m = do
      !_ <- STB.unsafeReadArray b' i
      loop b' (next i) (m - 1)

aUpdate :: A -> A
aUpdate !a = {-# SCC "aUpdate" #-} a A.// take (n `div` 3) (loop (1, 1, 1, 1) False)
  where
    loop (0, 0, 0, 0) _ = []
    loop !i !c = (i, c) : loop (next i) (not c)

bUpdate :: B -> B
bUpdate !b = {-# SCC "bUpdate" #-} b B.// take (n `div` 3) (loop (1, 1, 1, 1) False)
  where
    loop (0, 0, 0, 0) _ = []
    loop !i !c = (i, c) : loop (next i) (not c)

aElems :: A -> ()
aElems !a = {-# SCC "aElems" #-} seqList (A.elems a) ()

seqList :: [a] -> b -> b
seqList [] b = b
seqList (x:xs) b = x `seq` seqList xs b

bElems :: B -> ()
bElems b = {-# SCC "bElems" #-} seqList (B.elems b) ()

aMap :: A -> A
aMap !a = {-# SCC "aMap" #-} A.amap not a

bMap :: B -> B
bMap !b = {-# SCC "bMap" #-} B.amap not b

aZipWith :: (A, A) -> A
aZipWith (!a, !a') = {-# SCC "aZipWith" #-} A.listArray bs (map (\ !i -> a A.! i == a' A.! i) (range bs)) :: A

bZipWith :: (B, B) -> B
bZipWith (!b, !b') = {-# SCC "bZipWith" #-} B.zipWith (==) b b' :: B

aFold :: A -> Bool
aFold !a = {-# SCC "aFold" #-} foldl1' (/=) (A.elems a)

bFold :: B -> Bool
bFold !b = {-# SCC "bFold" #-} case B.fold (/=) b of
  Just !c -> c
  Nothing -> error "fold"

main :: IO ()
main = do
  let a  = A.listArray bs (take n         lorem ) :: A
      a' = A.listArray bs (take n (drop n lorem)) :: A
      b  = B.listArray bs (take n         lorem ) :: B
      b' = B.listArray bs (take n (drop n lorem)) :: B
      l  = take n lorem
  evaluate (l `seqList` a `seq` a' `seq` b `seq` b' `seq` ())
  defaultMain
    [ bgroup "listArray"
        [ bench "UArray"   $ whnf aListArray l
        , bench "BitArray" $ whnf bListArray l
        ]
    , bgroup "elems"
        [ bench "UArray"   $ whnf aElems a
        , bench "BitArray" $ whnf bElems b
        ]
    , bgroup "index I"
        [ bench "UArray"   $ whnf aIndex a
        , bench "BitArray" $ whnf bIndex b
        , bench "BitArray'"$ whnf bIndex' b
        ]
    , bgroup "index ST"
        [ bench "UArray"   $ whnf aIndexST a
        , bench "BitArray" $ whnf bIndexST b
        , bench "BitArray'"$ whnf bIndexST' b
        ]
    , bgroup "update"
        [ bench "UArray"   $ whnf aUpdate a
        , bench "BitArray" $ whnf bUpdate b
        ]
    , bgroup "map"
        [ bench "UArray"   $ whnf aMap a
        , bench "BitArray" $ whnf bMap b
        ]
    , bgroup "zipWith"
        [ bench "UArray"   $ whnf aZipWith (a, a')
        , bench "BitArray" $ whnf bZipWith (b, b')
        ]
    , bgroup "fold"
        [ bench "UArray"   $ whnf aFold a
        , bench "BitArray" $ whnf bFold b
        ]
    , bgroup "conversion"
        [ bench "U to Bit" $ whnf aToB a
        , bench "Bit to U" $ whnf bToA b
        ]
    , bgroup "serialize"
        [ bench "UArray"   $ whnf aToBS loremA
        , bench "BitArray" $ whnf bToBS loremB
        ]
    , bgroup "deserialize"
        [ bench "UArray"   $ whnf aFromBS (bs, loremBS)
        , bench "BitArray" $ whnf bFromBS (bs, loremBS)
        ]
    ]

loremA :: A
loremA = A.listArray bs (take n lorem)

loremB :: B
loremB = B.listArray bs (take n lorem)

lorem :: [Bool]
lorem = cycle . fromWord8sLE $ loremWS

loremBS :: ByteString
loremBS = pack . take ((n + 7) `shiftR` 3) . cycle $ loremWS

loremWS :: [Word8]
loremWS = map (toEnum . fromEnum) . unlines $
  [ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, "
  , "sed do eiusmod tempor incididunt ut labore et dolore magna "
  , "aliqua. Ut enim ad minim veniam, quis nostrud exercitation "
  , "ullamco laboris nisi ut aliquip ex ea commodo consequat. "
  , "Duis aute irure dolor in reprehenderit in voluptate velit "
  , "esse cillum dolore eu fugiat nulla pariatur. Excepteur sint "
  , "occaecat cupidatat non proident, sunt in culpa qui officia "
  , "deserunt mollit anim id est laborum."
  ]

un8 :: (a, a, a, a, a, a, a, a) -> [a]
un8 (a, b, c, d, e, f, g, h) = [a, b, c, d, e, f, g, h]

fromWord8sLE :: [Word8] -> [Bool]
fromWord8sLE = concatMap (un8 . unpackWord8LE)

toWord8sLE :: [Bool] -> [Word8]
toWord8sLE [] = []
toWord8sLE (a:b:c:d:e:f:g:h:rest) = packWord8LE a b c d e f g h : toWord8sLE rest
toWord8sLE rest = toWord8sLE (take 8 (rest ++ repeat False))
