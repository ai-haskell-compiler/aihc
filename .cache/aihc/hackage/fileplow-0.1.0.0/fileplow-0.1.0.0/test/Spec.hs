module Main where

import Data.FilePlow
import Data.FilePlow.Ordered

import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Monoid
import System.IO (hClose)
import System.IO.Temp
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL

makeRangeBS :: ([Int] -> [Int]) -> Int -> Int -> BS.ByteString
makeRangeBS f lb ub =
    BSL.toStrict . BSB.toLazyByteString $
    foldl' (\bldr i -> bldr <> BSB.int64Dec (fromIntegral i) <> BSB.char8 '\n') mempty $ f [lb .. ub]

withDummyFileNum :: ([Int] -> [Int]) -> (Int, Int) -> (FilePath -> IO a) -> IO a
withDummyFileNum f (lb, ub) go =
    withSystemTempFile "dummyFilePlowTest" $ \fp hdl ->
    do BS.hPut hdl (makeRangeBS f lb ub)
       hClose hdl
       go fp

withDummyFile :: Char -> Int -> (FilePath -> IO a) -> IO a
withDummyFile c size go =
    withSystemTempFile "dummyFilePlowTest" $ \fp hdl ->
    do BS.hPut hdl (BSC.replicate size c)
       hClose hdl
       go fp

readRest :: MultiHandle Handle -> IO BS.ByteString
readRest hdl =
    do let getLoop builder =
               do eof <- pIsEOF hdl
                  if eof
                      then pure $ BSL.toStrict (BSB.toLazyByteString builder)
                      else do x <- pGetChar hdl
                              getLoop (builder <> BSB.char8 x)
       getLoop mempty

main :: IO ()
main =
    hspec $
    do describe "multi handle" $
           do it "simple get char works" $
                  withDummyFile 'a' 100 $ \f1 ->
                  withDummyFile 'b' 100 $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do bs <- readRest hdl
                     bs `shouldBe` (BSC.replicate 100 'a' <> BSC.replicate 100 'b')
              it "two full reads with going to the begining work" $
                  withDummyFile 'a' 100 $ \f1 ->
                  withDummyFile 'b' 100 $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do _ <- readRest hdl
                     pSeek hdl AbsoluteSeek 0
                     bs <- readRest hdl
                     bs `shouldBe` (BSC.replicate 100 'a' <> BSC.replicate 100 'b')
              it "seek tell works" $
                  forAll (choose (0, 199)) $ \p ->
                  monadicIO $
                  do r <-
                         liftIO $
                         withDummyFile 'a' 100 $ \f1 ->
                         withDummyFile 'b' 100 $ \f2 ->
                         withMultiHandle [f1, f2] $ \hdl ->
                         do pSeek hdl AbsoluteSeek p
                            x <- pTell hdl
                            pure x
                     assert (r == p)
              it "seek until works" $
                  withDummyFile 'a' 100 $ \f1 ->
                  withDummyFile 'b' 100 $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do ok <- seekUntil hdl (== 'b')
                     ok `shouldBe` True
                     bs <- readRest hdl
                     bs `shouldBe` BSC.replicate 99 'b'
              it "seek untilRev works" $
                  withDummyFile 'a' 100 $ \f1 ->
                  withDummyFile 'b' 100 $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do pSeek hdl AbsoluteSeek 199
                     ok <- seekUntilRev hdl (== 'a')
                     ok `shouldBe` True
                     bs <- readRest hdl
                     bs `shouldBe` BSC.replicate 100 'b'
              it "seeked reads work" $
                  withDummyFile 'a' 100 $ \f1 ->
                  withDummyFile 'b' 100 $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do pSeek hdl AbsoluteSeek 100
                     bs <- readRest hdl
                     bs `shouldBe` BSC.replicate 100 'b'

                     pSeek hdl AbsoluteSeek 50
                     bs2 <- readRest hdl
                     bs2 `shouldBe` (BSC.replicate 50 'a' <> BSC.replicate 100 'b')

                     pSeek hdl SeekFromEnd (-50)
                     bs3 <- readRest hdl
                     bs3 `shouldBe` BSC.replicate 50 'b'

                     pSeek hdl SeekFromEnd (-150)
                     bs4 <- readRest hdl
                     bs4 `shouldBe` (BSC.replicate 50 'a' <> BSC.replicate 100 'b')

                     pSeek hdl AbsoluteSeek 0
                     pSeek hdl RelativeSeek 100
                     bs5 <- readRest hdl
                     bs5 `shouldBe` BSC.replicate 100 'b'

                     pSeek hdl AbsoluteSeek 50
                     pSeek hdl RelativeSeek 25
                     bs6 <- readRest hdl
                     bs6 `shouldBe` (BSC.replicate 25 'a' <> BSC.replicate 100 'b')
       describe "ordered" $
           do it "find a lower bound" $
                  withDummyFileNum id (100, 199) $ \f1 ->
                  withDummyFileNum id (200, 299) $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do ok <-
                         seekTo hdl (StLargerThan (150 :: Int)) $ \h ->
                         do void $ seekUntilRev hdl (== '\n')
                            eof <- pIsEOF hdl
                            if eof
                               then pure 999
                               else do ln <- pGetLine h
                                       pure $ read (BSC.unpack ln)
                     ok `shouldBe` True
                     void $ seekUntilRev hdl (== '\n')
                     bs <- readRest hdl
                     bs `shouldBe` (makeRangeBS id 150 299)
              it "finds upper bound" $
                  withDummyFileNum reverse (200, 299) $ \f1 ->
                  withDummyFileNum reverse (100, 199) $ \f2 ->
                  withMultiHandle [f1, f2] $ \hdl ->
                  do ok <-
                         seekTo hdl (StSmallerThan (150 :: Int)) $ \h ->
                         do void $ seekUntilRev hdl (== '\n')
                            eof <- pIsEOF hdl
                            if eof
                               then pure 0
                               else do ln <- pGetLine h
                                       pure $ read (BSC.unpack ln)
                     ok `shouldBe` True
                     void $ seekUntilRev hdl (== '\n')
                     bs <- readRest hdl
                     bs `shouldBe` (makeRangeBS reverse 100 150)
