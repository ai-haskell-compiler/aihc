module Main where

import qualified Streamly.External.ByteString as Strict
import qualified Streamly.External.ByteString.Lazy as Lazy
import Control.Monad.IO.Class (MonadIO)

import qualified Streamly.Data.Stream as S
import qualified Streamly.Data.Fold as Fold
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Control.DeepSeq (NFData(..))
import System.Random (randomRIO)

import Gauge

numElements :: Int
numElements = 100000

numChunks :: Int
numChunks = 100000

{-# INLINE benchIO #-}
benchIO :: NFData b => String -> (Int -> IO a) -> (a -> b) -> Benchmark
benchIO name src f = bench name $ nfIO $ randomRIO (1, 1) >>= src >>= return . f

{-# INLINE benchIO' #-}
benchIO' :: NFData b => String -> (Int -> IO a) -> (a -> IO b) -> Benchmark
benchIO' name src f = bench name $ nfIO $ randomRIO (1, 1) >>= src >>= f

{-# INLINE fromChunks #-}
fromChunks :: Monad m => Int -> m BSL.ByteString
fromChunks n =
    Lazy.fromChunks $
    fmap Strict.toArray $
    fmap BS.singleton $
    fmap fromIntegral $
    fmap (\x -> x `mod` 256) $ S.enumerateFromTo n (n + numChunks)

{-# INLINE fromChunksIO #-}
fromChunksIO :: Int -> IO BSL.ByteString
fromChunksIO n =
    Lazy.fromChunksIO $
    fmap Strict.toArray $
    fmap BS.singleton $
    fmap fromIntegral $
    fmap (\x -> x `mod` 256) $ S.enumerateFromTo n (n + numChunks)

{-# INLINE toChunks #-}
toChunks :: Monad m => BSL.ByteString -> m ()
toChunks = S.fold Fold.drain . Lazy.toChunks

{-# INLINE strictWrite #-}
strictWrite :: MonadIO m => Int -> m BS.ByteString
strictWrite n =
    S.fold Strict.write $
    fmap fromIntegral $
    fmap (\x -> x `mod` 256) $ S.enumerateFromTo n (n + numElements)

{-# INLINE strictWriteN #-}
strictWriteN :: MonadIO m => Int -> m BS.ByteString
strictWriteN n =
    S.fold (Strict.writeN numElements) $
    fmap fromIntegral $
    fmap (\x -> x `mod` 256) $ S.enumerateFromTo n (n + numElements)

{-# INLINE strictRead #-}
strictRead :: MonadIO m => BS.ByteString -> m ()
strictRead = S.fold Fold.drain . S.unfold Strict.reader

{-# INLINE lazyRead #-}
lazyRead :: MonadIO m => BSL.ByteString -> m ()
lazyRead = S.fold Fold.drain . S.unfold Lazy.reader

main :: IO ()
main =
    defaultMain
        [ benchIO "Strict Write" strictWrite id
        , benchIO "Strict WriteN" strictWriteN id
        , benchIO' "Strict Read" strictWrite strictRead
        , benchIO' "Lazy Read" fromChunks lazyRead
        , benchIO "fromChunks" fromChunks id
        , benchIO "fromChunksIO" fromChunksIO id
        , benchIO' "toChunks" fromChunks toChunks
        ]
