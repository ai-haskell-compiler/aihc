{-# LANGUAGE UndecidableInstances,
             FlexibleInstances,
             FlexibleContexts,
             TypeFamilies,
             ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Signal.Multichannel
-- Copyright   :  (c) Alexander Vivian Hugh McPhail 2010, 2014, 2015, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses Concurrency
--
-- Signal processing functions, multichannel datatype
--
-- link with '-threaded' and run with +RTS -Nn, where n is the number of CPUs
--
-----------------------------------------------------------------------------

--             IncoherentInstances,


module Numeric.Signal.Multichannel (
                       Multichannel,readMultichannel,writeMultichannel,
                       createMultichannel,
                       sampling_rate,precision,channels,samples,
                       detrended,filtered,
                       getChannel,getChannels,
                       toMatrix,
                       mapConcurrently,
                       detrend,filter,
                       slice,
                       histograms,
                       entropy_delta_phase,mi_phase
                ) where

-----------------------------------------------------------------------------

import qualified Numeric.Signal as S

--import Complex

import qualified Data.Array.IArray as I
import Data.Ix

--import Data.Word

import Control.Concurrent
--import Control.Concurrent.MVar

import System.IO.Unsafe(unsafePerformIO)

--import qualified Data.List as L

import Data.Binary
import Data.Maybe

import Foreign.Storable

import Numeric.LinearAlgebra hiding(range)

import qualified Data.Vector.Generic as GV

--import qualified Numeric.GSL.Fourier as F

import qualified Numeric.GSL.Histogram as H
import qualified Numeric.GSL.Histogram2D as H2

import qualified Numeric.Statistics.Information as SI

import Prelude hiding(filter)

import Control.Monad(replicateM)


{-
-------------------------------------------------------------------

instance (Binary a, Storable a) => Binary (Vector a) where
    put v = do
            let d = GV.length v
            put d
            mapM_ (\i -> put $ v @> i) [0..(d-1)]
    get = do
          d <- get
          xs <- replicateM d get
          return $ fromList xs

-------------------------------------------------------------------
-}

-----------------------------------------------------------------------------

-- | data type with multiple channels
data Multichannel a = MC {
                          _sampling_rate :: Int             -- ^ sampling rate
                          , _precision   :: Int             -- ^ bits of precision
                          , _channels    :: Int             -- ^ number of channels
                          , _length      :: Int             -- ^ length in samples
                          , _detrended   :: Bool            -- ^ was the data detrended?
                          , _filtered    :: Maybe (Int,Int) -- ^ if filtered the passband
                          , _data        :: I.Array Int (Vector a) -- ^ data
                         }

-----------------------------------------------------------------------------

instance Binary (Multichannel Double) where
    put (MC s p c l de f d) = do
                              put s
                              put p
                              put c
                              put l
                              put de
                              put f
                              put $! fmap convert d
        where convert v = let (mi,ma) = (minElement v,maxElement v)
                              v' = GV.map (\x -> round $ (x - mi)/(ma - mi) * (fromIntegral (maxBound :: Word64))) v
                          in (mi,ma,v' :: Vector Word64) 

    get = do
          s <- get
          p <- get
          c <- get
          l <- get
          de <- get
          f <- get
          (d :: I.Array Int (Double,Double,Vector Word64)) <- get
          return $! (MC s p c l de f (seq d (fmap convert) d))
              where convert (mi,ma,v) = GV.map (\x -> ((fromIntegral x)) / (fromIntegral (maxBound :: Word64)) * (ma - mi) + mi) v

instance Binary (Multichannel Float) where
    put (MC s p c l de f d) = do
                              put s
                              put p
                              put c
                              put l
                              put de
                              put f
                              put $! fmap convert d
        where convert v = let (mi,ma) = (minElement v,maxElement v)
                              v' = GV.map (\x -> round $ (x - mi)/(ma - mi) * (fromIntegral (maxBound :: Word64))) v
                          in (mi,ma,v' :: Vector Word64) 

    get = do
          s <- get
          p <- get
          c <- get
          l <- get
          de <- get
          f <- get
          (d :: I.Array Int (Float,Float,Vector Word32)) <- get
          return $! (MC s p c l de f (seq d (fmap convert) d))
              where convert (mi,ma,v) = GV.map (\x -> ((fromIntegral x)) / (fromIntegral (maxBound :: Word32)) * (ma - mi) + mi) v

instance Binary (Multichannel (Complex Double)) where
    put (MC s p c l de f d) = do
                              put s
                              put p
                              put c
                              put l
                              put de
                              put f
                              put $! fmap ((\(r,j) -> (convert r, convert j)) . fromComplex) d
        where convert v = let (mi,ma) = (minElement v,maxElement v)
                              v' = GV.map (\x -> round $ (x - mi)/(ma - mi) * (fromIntegral (maxBound :: Word64))) v
                          in (mi,ma,v' :: Vector Word64) 

    get = do
          s <- get
          p <- get
          c <- get
          l <- get
          de <- get
          f <- get
          (d :: I.Array Int ((Double,Double,Vector Word64),(Double,Double,Vector Word64))) <- get
          return $! (MC s p c l de f (seq d (fmap (\(r,j) -> toComplex (convert r,convert j)) d)))
              where convert (mi,ma,v) = GV.map (\x -> ((fromIntegral x)) / (fromIntegral (maxBound :: Word64)) * (ma - mi) + mi) v



instance Binary (Multichannel (Complex Float)) where
    put (MC s p c l de f d) = do
                              put s
                              put p
                              put c
                              put l
                              put de
                              put f
                              put $! fmap ((\(r,j) -> (convert r, convert j)) . fromComplex) d
        where convert v = let (mi,ma) = (minElement v,maxElement v)
                              v' = GV.map (\x -> round $ (x - mi)/(ma - mi) * (fromIntegral (maxBound :: Word32))) v
                          in (mi,ma,v' :: Vector Word32) 

    get = do
          s <- get
          p <- get
          c <- get
          l <- get
          de <- get
          f <- get
          (d :: I.Array Int ((Float,Float,Vector Word32),(Float,Float,Vector Word32))) <- get
          return $! (MC s p c l de f (seq d (fmap (\(r,j) -> toComplex (convert r,convert j)) d)))
              where convert (mi,ma,v) = GV.map (\x -> ((fromIntegral x)) / (fromIntegral (maxBound :: Word32)) * (ma - mi) + mi) v



-----------------------------------------------------------------------------

readMultichannel :: (Binary (Multichannel a)) => FilePath -> IO (Multichannel a)
readMultichannel = decodeFile

writeMultichannel :: (Binary (Multichannel a)) => FilePath -> Multichannel a -> IO ()
writeMultichannel = encodeFile

-----------------------------------------------------------------------------

-- | create a multichannel data type
createMultichannel :: Storable a 
                   => Int               -- ^ sampling rate
                   -> Int               -- ^ bits of precision
                   -> [Vector a]        -- ^ data
                   -> Multichannel a    -- ^ datatype
createMultichannel s p d = let c = length d
                 in MC s p c (GV.length $ head d) False Nothing (I.listArray (1,c) d)

-- | the sampling rate
sampling_rate :: Multichannel a -> Int
sampling_rate = _sampling_rate

-- | the bits of precision
precision :: Multichannel a -> Int
precision = _precision

-- | the number of channels
channels :: Multichannel a -> Int
channels = _channels

-- | the length, in samples
samples :: Multichannel a -> Int
samples = _length

-- | extract one channel
getChannel :: Int -> Multichannel a -> Vector a
getChannel c d = (_data d) I.! c

-- | extract all channels
getChannels :: Multichannel a -> I.Array Int (Vector a)
getChannels d = _data d

-- | convert the data to a matrix with channels as rows
toMatrix :: Element a => Multichannel a -> Matrix a
toMatrix = fromRows . I.elems . _data

-- | was the data detrended?
detrended :: Multichannel a -> Bool
detrended = _detrended

-- | was the data filtered?
filtered :: Multichannel a -> Maybe (Int,Int)
filtered = _filtered

-----------------------------------------------------------------------------

-- | map a function executed concurrently
mapArrayConcurrently :: Ix i => (a -> b)    -- ^ function to map
                     -> I.Array i a         -- ^ input
                     -> (I.Array i b)     -- ^ output
mapArrayConcurrently f d = unsafePerformIO $ do
  let b = I.bounds d
  results <- replicateM (rangeSize b) newEmptyMVar
  mapM_ (forkIO . applyFunction f) $ zip results (I.assocs d)
  vectors <- mapM takeMVar results
  return $ I.array b vectors
    where applyFunction f' (m,(j,e)) = putMVar m (j,f' e)

{-
-- | map a function executed concurrently
mapListConcurrently :: (a -> b)             -- ^ function to map
                    -> [a]                  -- ^ input
                    -> [b]                  -- ^ output
mapListConcurrently f d = unsafePerformIO $ do
                                            results <- replicateM (length d) newEmptyMVar
                                            mapM_ (forkIO . applyFunction f) zip results d
                                            mapM takeMVar results
    where applyFunction f' (m,e) = putMVar m (f' e)
-}

-- | map a function executed concurrently
mapConcurrently :: Storable b 
                => (Vector a -> Vector b)      -- ^ the function to be mapped 
                -> Multichannel a              -- ^ input data
                -> Multichannel b              -- ^ output data
mapConcurrently f (MC sr p c _ de fi d) = let d' = mapArrayConcurrently f d
                                          in MC sr p c (GV.length $ d' I.! 1) de fi d'

-- | map a function
mapMC :: Storable b 
      => (Vector a -> Vector b)                -- ^ the function to be mapped 
      -> Multichannel a                        -- ^ input data
      -> Multichannel b                        -- ^ output data
mapMC f (MC sr p c _ de fi d) = let d' = fmap f d
                                in MC sr p c (GV.length $ d' I.! 1) de fi d'
                                    
-----------------------------------------------------------------------------

-- | detrend the data with a specified window size
detrend :: Int -> Multichannel Double -> Multichannel Double
detrend w m = let m' = mapConcurrently (S.detrend w) m
              in m' { _detrended = True }


-- | filter the data with the given passband
filter :: (S.Filterable a, Double ~ DoubleOf a) => 
         (Int,Int) -> Multichannel a -> Multichannel a
filter pb m = let m' = mapConcurrently (S.broadband_filter (_sampling_rate m) pb) m
              in m' { _filtered = Just pb }

-----------------------------------------------------------------------------

-- | extract a slice of the data
slice :: Storable a 
      => Int                 -- ^ starting sample number
      -> Int                 -- ^ length
      -> Multichannel a 
      -> Multichannel a
slice j w m = let m' = mapConcurrently (subVector j w) m
              in m' { _length = w }

-----------------------------------------------------------------------------

-- | calculate histograms
histograms :: (S.Filterable a, Double ~ DoubleOf a) =>
            I.Array Int (Vector a)
          -> Int -> (Double,Double) 
          -> Int -> Int -> (Double,Double) -> (Double,Double) -- ^ bins and ranges
          -> (I.Array Int H.Histogram,I.Array (Int,Int) H2.Histogram2D)
histograms d' b (l,u) bx by (lx,ux) (ly,uy) 
  = let d = fmap double d'
        (bl,bu) = I.bounds d
        br = ((bl,bl),(bu,bu))
        histarray = mapArrayConcurrently (H.fromLimits b (l,u)) d
        pairs = I.array br $ map (\(m,n) -> ((m,n),(d I.! m,d I.! n))) (range br)
        hist2array = mapArrayConcurrently (\(x,y) -> (H2.addVector (H2.emptyLimits bx by (lx,ux) (ly,uy)) x y)) pairs
    in (histarray,hist2array)

-----------------------------------------------------------------------------

-- | calculate the entropy of the phase difference between pairs of channels (fills upper half of matrix)
entropy_delta_phase :: (S.Filterable a, Double ~ DoubleOf a) =>
                Multichannel a      -- ^ input data
              -> Matrix Double
entropy_delta_phase m = let d = _data m
                            c = _channels m
                            b = ((1,1),(c,c))
                            r = I.range b
                            diff = I.listArray b (map (\j@(x,y) -> (j,if x <= y then Just (double $ (d I.! y)-(d I.! x)) else Nothing)) r) :: I.Array (Int,Int) ((Int,Int),Maybe (Vector Double))
                            h = mapArrayConcurrently (maybe Nothing (\di -> Just $ H.fromLimits 128 ((-2)*pi,2*pi) di)) (fmap snd diff)
                            ent = mapArrayConcurrently (\(j,difvec) -> case difvec of 
                                                        Nothing -> 0 :: Double
                                                        Just da -> SI.entropy (fromJust (h I.! j)) da) diff
                        in fromArray2D ent

-----------------------------------------------------------------------------

-- | calculate the mutual information of the phase between pairs of channels (fills upper half of matrix)
mi_phase :: (S.Filterable a, Double ~ DoubleOf a) =>
           Multichannel a      -- ^ input data
         -> Matrix Double
mi_phase m = let d = _data m
                 (histarray,hist2array) = histograms d 128 (-pi,pi) 128 128 (-pi,pi) (-pi,pi)
                 indhist = I.listArray (I.bounds hist2array) (I.assocs hist2array)
                 mi = mapArrayConcurrently (doMI histarray (fmap double d)) indhist
             in fromArray2D mi
    where doMI histarray d ((x,y),h2) 
              | x <= y     = SI.mutual_information h2 (histarray I.! x) (histarray I.! y) (d I.! x,d I.! y)
              | otherwise = 0

-----------------------------------------------------------------------------
