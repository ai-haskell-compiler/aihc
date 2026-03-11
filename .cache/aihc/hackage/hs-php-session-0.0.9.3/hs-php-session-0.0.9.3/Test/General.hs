-- |
-- Module : Data.PHPSession
-- License: BSD3
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: unknown
--
-- Testing Data.PHPSession
-- 
{-# LANGUAGE OverloadedStrings #-}

module Test.General (testcases, testcasesWith) where

import Data.PHPSession

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.List as L
import Data.List (foldl')
import System.IO

test1 = "v1|s:94:\" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}\";v2|a:3:{i:0;i:10;i:1;i:9;i:2;s:5:\"eight\";}v3|C:9:\"ClassName\":94:{ !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}}v4|b:1;v5|i:10;v6|d:5.5;v7|N;"
test2 = "v1|a:3:{i:0;i:10;i:1;i:9;i:2;a:3:{i:0;i:8;i:1;i:7;i:2;a:3:{i:0;i:6;i:1;s:4:\"five\";i:2;d:4;}}}v2|O:9:\"ClassName\":2:{s:4:\"blah\";i:1;s:3:\"str\";s:94:\" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}\";}"
test3 = "v1|s:256:\"00000000000000000000000000000000 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\";"
test4 = "teststr1|s:0:\"\";test_array|a:1:{i:0;a:2:{s:4:\"ABCD\";s:9:\"987654321\";s:0:\"\";b:1;}}test_int|i:1;teststr2|s:4:\"1234\";"
test5 = "testfloat1|d:1.00001121;testfloat2|d:NAN;testfloat3|d:INF;testfloat4|d:-INF;"

testcases :: IO Bool
testcases = do
  testcasesWith (\message -> hPutStrLn stderr message)

testcasesWith :: (String -> IO ()) -> IO Bool
testcasesWith issue = do
  let tresults = map testDecodeAndReencode [test1,test2,test3,test4,test5]
      fresults = map testDecodeOnly []
  tresults' <- mapM issueIfDidntDecodedAndReencoded tresults
  fresults' <- mapM issueIfDecodedAndReencodedWhenItShouldnt fresults
  return $ and tresults' && ((or fresults') /= True) 
  where
    issueIfDidntDecodedAndReencoded a =
      case a of
        Left message -> issue message >> return False
        Right b -> return b
    issueIfDecodedAndReencodedWhenItShouldnt a =
      case a of
        Right decoded -> (issue $ "Decoded when it shouldnt " ++ (show decoded)) >> return False
        Left b -> return True
    testDecodeAndReencode test = 
      case decodePHPSessionEither test of
        Left err -> Left (show err)
        Right dec -> let result = encodePHPSession dec in
           if result == test
             then Right True
             else Left $ "Not same: " ++ LBS.unpack test ++ "  and  " ++ LBS.unpack result
    testDecodeOnly test = decodePHPSessionEither test
