{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Primitive
import Control.Monad.State

import Data.HashSet (HashSet)
import Data.Word
import qualified Data.HashSet as HS

import qualified Data.IntSet.Bounded.Imperative as BIS

import System.Random

main :: IO ()
main = evalStateT testOnce (mkStdGen 0x28761967)

testOnce :: StateT StdGen IO ()
testOnce = do
    let minB = 0
    let maxB = 1*1000

    let newRand :: StateT StdGen IO Word64
        newRand = do
            f <- getRandom
            if f then getRandom else getRandomR (minB, maxB)

    doAll 1000 HS.empty newRand =<< liftIO (BIS.empty minB maxB)

    liftIO $ putStrLn "Success"
  where
    doAll :: Int -> HashSet Word64 -> StateT StdGen IO Word64 -> BIS.IntSet (PrimState IO) -> StateT StdGen IO ()
    doAll !k !s' newRand s
        | k <= 0 = return ()
        | otherwise = do
            when (k `rem` 100 == 0) $
                liftIO $ print k

            forM_ (HS.toList s') $ \n -> do
                f <- liftIO $ BIS.member s n
                unless f $
                    liftIO . throwIO $ TestError "added number did not return true"

            replicateM_ 100 $ do
                n <- getRandom
                f <- liftIO $ BIS.member s n
                let g = HS.member n s'
                when (g /= f) $
                    liftIO . throwIO $ TestError "random number did not match"

            n <- newRand
            liftIO $ BIS.insert s n

            m <- newRand
            liftIO $ BIS.delete s m

            doAll (k-1) (HS.delete m $ HS.insert n s') newRand s


getRandom :: (MonadState g m, RandomGen g, Random a) => m a
getRandom = do
    g <- get
    let (!x, !h) = random g
    put h
    return x

getRandomR :: (MonadState g m, RandomGen g, Random a) => (a, a) -> m a
getRandomR b = do
    g <- get
    let (!x, !h) = randomR b g
    put h
    return x


data TestError = TestError String deriving (Show)

instance Exception TestError
