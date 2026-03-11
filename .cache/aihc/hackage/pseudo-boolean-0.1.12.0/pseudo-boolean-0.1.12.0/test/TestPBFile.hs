{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.Either
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import System.IO
import System.IO.Temp
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH
import Data.PseudoBoolean
import qualified Data.PseudoBoolean.Attoparsec as A
import qualified Data.PseudoBoolean.Megaparsec as M
import Data.PseudoBoolean.Internal.TextUtil

case_exampleLIN  = checkOPBString "exampleLIN"  exampleLIN
case_exampleNLC1 = checkOPBString "exampleNLC1" exampleNLC1
case_exampleNLC2 = checkOPBString "exampleNLC2" exampleNLC2
case_exampleWBO1 = checkWBOString "exampleWBO1" exampleWBO1
case_exampleWBO2 = checkWBOString "exampleWBO2" exampleWBO2
case_exampleWBO3 = checkWBOString "exampleWBO3" exampleWBO3

case_exampleLIN_file  = checkOPBFile "test/samples/example-lin.opb"
case_exampleNLC1_file = checkOPBFile "test/samples/example-nlc-1.opb"
case_exampleNLC2_file = checkOPBFile "test/samples/example-nlc-2.opb"
case_exampleWBO1_file = checkWBOFile "test/samples/example1.wbo"
case_exampleWBO2_file = checkWBOFile "test/samples/example2.wbo"
case_exampleWBO3_file = checkWBOFile "test/samples/example3.wbo"

case_exampleLIN_PBS_file = checkOPBFile "test/samples/example-lin-pbs.opb"
case_exampleLIN_nohint_file = checkOPBFile "test/samples/example-lin-nohint.opb"
case_exampleLIN_nospaces_file = checkOPBFile "test/samples/example-lin-nospaces.opb"
case_exampleWBO1_nohint_file = checkWBOFile "test/samples/example1-nohint.wbo"
case_exampleWBO1_notop_file = checkWBOFile "test/samples/example1-notop.wbo"
case_exampleWBO1_nospaces_file = checkWBOFile "test/samples/example1-nospaces.wbo"

case_normalized_1096_cudf_paranoid  = checkOPBFile "test/samples/normalized-1096.cudf.paranoid.opb"
case_normalized_mds_50_10_4 = checkOPBFile "test/samples/normalized-mds_50_10_4.opb"
case_normalized_opt_market_split_4_30_2 = checkOPBFile "test/samples/normalized-opt-market-split_4_30_2.opb"
case_pigeonhole_5_4 = checkOPBFile "test/samples/pigeonhole_5_4.opb"

case_invalid_obj_empty_sum = checkOPBFile "test/samples/invalid-obj-empty-sum.opb"
case_invalid_lhs_empty_sum = checkOPBFile "test/samples/invalid-lhs-empty-sum.opb"
case_invalid_lhs_empty_sum_wbo = checkWBOFile "test/samples/invalid-lhs-empty-sum.wbo"

case_trailing_junk = do
  isError (parseOPBString "" trailingJunk) @?= True
  isError (M.parseOPBString "" trailingJunk) @?= True
  isError (parseOPBByteString "" (BSChar8.pack trailingJunk)) @?= True
  isError (M.parseOPBByteString "" (BSChar8.pack trailingJunk)) @?= True
  isError (A.parseOPBByteString (BSChar8.pack trailingJunk)) @?= True
  where
    -- isLeft is available only on base >=4.7.0.0.
    isError :: Either a b -> Bool
    isError (Left _) = True
    isError (Right _) = False
    
    trailingJunk = unlines
      [ "* #variable= 5 #constraint= 4"
      , "*"
      , "* this is a dummy instance"
      , "*"
      , "min: 1 x2 -1 x3 ;"
      , "1 x1 +4 x2 -2 x5 >= 2;"
      , "-1 x1 +4 x2 -2 x5 >= +3;"
      , "12345678901234567890 x4 +4 x3 >= 10;"
      , "* an equality constraint"
      , "2 x2 +3 x4 +2 x1 +3 x5 = 5;"
      , "foo"
      ]

case_min_eol = do
  isOk (parseOPBString "" opb) @?= True
  isOk (M.parseOPBString "" opb) @?= True
  isOk (parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (M.parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (A.parseOPBByteString (BSChar8.pack opb)) @?= True
  where
    -- isLeft is available only on base >=4.7.0.0.
    isOk :: Either a b -> Bool
    isOk (Left _) = False
    isOk (Right _) = True
    
    opb = unlines
      [ "* #variable= 5 #constraint= 4"
      , "*"
      , "* this is a dummy instance"
      , "*"
      , "min: "
      , "1 x2 -1 x3 ;"
      , "1 x1 +4 x2 -2 x5 >= 2;"
      , "-1 x1 +4 x2 -2 x5 >= +3;"
      , "12345678901234567890 x4 +4 x3 >= 10;"
      , "* an equality constraint"
      , "2 x2 +3 x4 +2 x1 +3 x5 = 5;"
      ]

case_empty_constraints = do
  isOk (parseOPBString "" opb) @?= True
  isOk (M.parseOPBString "" opb) @?= True
  isOk (parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (M.parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (A.parseOPBByteString (BSChar8.pack opb)) @?= True
  where
    -- isRight is available only on base >=4.7.0.0.
    isOk :: Either a b -> Bool
    isOk (Left _) = False
    isOk (Right _) = True
    
    opb = unlines
      [ "* #variable= 3 #constraint= 0"
      , "min: 1 x2 -1 x3 ;"
      ]

case_wbo_empty_constraints = do
  isOk (parseWBOString "" wbo) @?= True
  isOk (M.parseWBOString "" wbo) @?= True
  isOk (parseWBOByteString "" (BSChar8.pack wbo)) @?= True
  isOk (M.parseWBOByteString "" (BSChar8.pack wbo)) @?= True
  isOk (A.parseWBOByteString (BSChar8.pack wbo)) @?= True
  where
    -- isRight is available only on base >=4.7.0.0.
    isOk :: Either a b -> Bool
    isOk (Left _) = False
    isOk (Right _) = True
    
    wbo = unlines
      [ "* #variable= 0 #constraint= 0"
      , "soft: 1 ;"
      ]

case_trailing_spaces_before_eol = do
  isOk (parseOPBString "" opb) @?= True
  isOk (M.parseOPBString "" opb) @?= True
  isOk (parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (M.parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (A.parseOPBByteString (BSChar8.pack opb)) @?= True
  where
    -- isRight is available only on base >=4.7.0.0.
    isOk :: Either a b -> Bool
    isOk (Left _) = False
    isOk (Right _) = True
    
    opb = unlines
      [ "* #variable= 5 #constraint= 4"
      , "*"
      , "* this is a dummy instance"
      , "*"
      , "min: 1 x2 -1 x3 ;"
      , "1 x1 +4 x2 -2 x5 >= 2; "
      , "-1 x1 +4 x2 -2 x5 >= +3; "
      , "12345678901234567890 x4 +4 x3 >= 10; "
      , "* an equality constraint "
      , "2 x2 +3 x4 +2 x1 +3 x5 = 5; "
      ]

case_trailing_spaces_before_eof = do
  isOk (parseOPBString "" opb) @?= True
  isOk (M.parseOPBString "" opb) @?= True
  isOk (parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (M.parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (A.parseOPBByteString (BSChar8.pack opb)) @?= True
  where
    -- isRight is available only on base >=4.7.0.0.
    isOk :: Either a b -> Bool
    isOk (Left _) = False
    isOk (Right _) = True
    
    opb = unlines
      [ "* #variable= 5 #constraint= 4"
      , "*"
      , "* this is a dummy instance"
      , "*"
      , "min: 1 x2 -1 x3 ;"
      , "1 x1 +4 x2 -2 x5 >= 2;"
      , "-1 x1 +4 x2 -2 x5 >= +3;"
      , "12345678901234567890 x4 +4 x3 >= 10;"
      , "* an equality constraint"
      , "2 x2 +3 x4 +2 x1 +3 x5 = 5;"
      , "        "
      ]

case_leading_spaces = do
  isOk (parseOPBString "" opb) @?= True
  isOk (M.parseOPBString "" opb) @?= True
  isOk (parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (M.parseOPBByteString "" (BSChar8.pack opb)) @?= True
  isOk (A.parseOPBByteString (BSChar8.pack opb)) @?= True
  where
    -- isRight is available only on base >=4.7.0.0.
    isOk :: Either a b -> Bool
    isOk (Left _) = False
    isOk (Right _) = True
    
    opb = unlines
      [ "* #variable= 5 #constraint= 4"
      , "*"
      , "* this is a dummy instance"
      , "*"
      , "min: 1 x2 -1 x3 ;"
      , "   1 x1 +4 x2 -2 x5 >= 2;"
      , "   -1 x1 +4 x2 -2 x5 >= +3;"
      , "   12345678901234567890 x4 +4 x3 >= 10;"
      , "* an equality constraint"
      , "   2 x2 +3 x4 +2 x1 +3 x5 = 5;"
      ]

case_readUnsignedInteger_maxBound_bug :: IO ()
case_readUnsignedInteger_maxBound_bug =
  readUnsignedInteger "006666666666666667" @?= 6666666666666667

prop_readUnsignedInteger = 
  forAll (choose (0, 2^(128::Int))) $ \i -> 
    readUnsignedInteger (show i) == i

------------------------------------------------------------------------
-- Sample data

exampleLIN :: String
exampleLIN = unlines
  [ "* #variable= 5 #constraint= 4"
  , "*"
  , "* this is a dummy instance"
  , "*"
  , "min: 1 x2 -1 x3 ;"
  , "1 x1 +4 x2 -2 x5 >= 2;"
  , "-1 x1 +4 x2 -2 x5 >= +3;"
  , "12345678901234567890 x4 +4 x3 >= 10;"
  , "* an equality constraint"
  , "2 x2 +3 x4 +2 x1 +3 x5 = 5;"
  ]

exampleNLC1 :: String
exampleNLC1 = unlines
  [ "* #variable= 5 #constraint= 4 #product= 5 sizeproduct= 13"
  , "*"
  , "* this is a dummy instance"
  , "*"
  , "min: 1 x2 x3 -1 x3 ;"
  , "1 x1 +4 x1 ~x2 -2 x5 >= 2;"
  , "-1 x1 +4 x2 -2 x5 >= 3;"
  , "12345678901234567890 x4 +4 x3 >= 10;"
  , "2 x2 x3 +3 x4 ~x5 +2 ~x1 x2 +3 ~x1 x2 x3 ~x4 ~x5 = 5 ;"
  ]

exampleNLC2 :: String
exampleNLC2 = unlines
  [ "* #variable= 6 #constraint= 3 #product= 9 sizeproduct= 18"
  , "*"
  , "* Factorization problem: find the smallest P such that P*Q=N"
  , "* P is a 3 bits number (x3 x2 x1)"
  , "* Q is a 3 bits number (x6 x5 x4)"
  , "* N=35"
  , "* "
  , "* minimize the value of P"
  , "min: +1 x1 +2 x2 +4 x3 ;"
  , "* P>=2 (to avoid trivial factorization)"
  , "+1 x1 +2 x2 +4 x3 >=2;"
  , "* Q>=2 (to avoid trivial factorization)"
  , "+1 x4 +2 x5 +4 x6 >=2;"
  , "*"
  , "* P*Q=N"
  , "+1 x1 x4 +2 x1 x5 +4 x1 x6 +2 x2 x4 +4 x2 x5 +8 x2 x6 +4 x3 x4 +8 x3 x5 +16 x3 x6 = 35;"
  ]

exampleWBO1 :: String
exampleWBO1 = unlines $
  [ "* #variable= 1 #constraint= 2 #soft= 2 mincost= 2 maxcost= 3 sumcost= 5"
  , "soft: 6 ;"
  , "[2] +1 x1 >= 1 ;"
  , "[3] -1 x1 >= 0 ;"
  ]

exampleWBO2 :: String
exampleWBO2 = unlines $
  [ "* #variable= 2 #constraint= 3 #soft= 2 mincost= 2 maxcost= 3 sumcost= 5"
  , "soft: 6 ;"
  , "[2] +1 x1 >= 1 ;"
  , "[3] +1 x2 >= 1 ;"
  , "-1 x1 -1 x2 >= -1 ;"
  ]

exampleWBO3 :: String
exampleWBO3 = unlines $
  [ "* #variable= 4 #constraint= 6 #soft= 4 mincost= 2 maxcost= 5 sumcost= 14"
  , "soft: 6 ;"
  , "[2] +1 x1 >= 1;"
  , "[3] +1 x2 >= 1;"
  , "[4] +1 x3 >= 1;"
  , "[5] +1 x4 >= 1;"
  , "-1 x1 -1 x2 >= -1 ;"
  , "-1 x3 -1 x4 >= -1 ;"
  ]

------------------------------------------------------------------------
-- Utilities

checkOPBFile :: FilePath -> IO ()
checkOPBFile fname = do
  r <- parseOPBFile fname
  case r of
    Left err -> assertFailure $ show err
    Right opb -> do
      r2 <- M.parseOPBFile fname
      case r2 of
        Left err2 -> assertFailure $ show err2
        Right opb2 -> opb2 @?= opb
      r3 <- A.parseOPBFile fname
      case r3 of
        Left err2 -> assertFailure $ show err2
        Right opb2 -> opb2 @?= opb

      withSystemTempFile "TestPBFile.opb" $ \tmppath h -> do
        hClose h
        writeOPBFile tmppath opb
        r4 <- parseOPBFile tmppath
        case r4 of
          Left err2 -> assertFailure $ show err2
          Right opb2 -> opb2 @?= opb

checkOPBString :: String -> String -> IO ()
checkOPBString name str = do
  case parseOPBString name str of
    Left err -> assertFailure $ show err
    Right opb -> do
      let s = toOPBString opb
          bs = toOPBByteString opb
      BSChar8.unpack bs @?= s

      case parseOPBString name s of
        Left err -> assertFailure $ show err
        Right opb2 -> opb2 @?= opb
      case parseOPBByteString name bs of
        Left err -> assertFailure $ show err
        Right opb2 -> opb2 @?= opb

      case M.parseOPBString name s of
        Left err -> assertFailure $ show err
        Right opb2 -> opb2 @?= opb
      case M.parseOPBByteString name bs of
        Left err -> assertFailure $ show err
        Right opb2 -> opb2 @?= opb

      case A.parseOPBByteString bs of
        Left err -> assertFailure err
        Right opb2 -> opb2 @?= opb

checkWBOFile :: FilePath -> IO ()
checkWBOFile fname = do
  r <- parseWBOFile fname
  case r of
    Left err -> assertFailure $ show err
    Right wbo -> do
      r2 <- M.parseWBOFile fname
      case r2 of
        Left err2 -> assertFailure $ show err2
        Right wbo2 -> wbo2 @?= wbo
      r3 <- A.parseWBOFile fname
      case r3 of
        Left err2 -> assertFailure $ show err2
        Right wbo2 -> wbo2 @?= wbo

      withSystemTempFile "TestPBFile.wbo" $ \tmppath h -> do
        hClose h
        writeWBOFile tmppath wbo
        r4 <- parseWBOFile tmppath
        case r4 of
          Left err2 -> assertFailure $ show err2
          Right wbo2 -> wbo2 @?= wbo

checkWBOString :: String -> String -> IO ()
checkWBOString name str = do
  case parseWBOString name str of
    Left err -> assertFailure $ show err
    Right wbo -> do
      let s = toWBOString wbo
          bs = toWBOByteString wbo
      BSChar8.unpack bs @?= s

      case parseWBOString name s of
        Left err -> assertFailure $ show err
        Right wbo2 -> wbo2 @?= wbo
      case parseWBOByteString name bs of
        Left err -> assertFailure $ show err
        Right wbo2 -> wbo2 @?= wbo

      case M.parseWBOString name s of
        Left err -> assertFailure $ show err
        Right wbo2 -> wbo2 @?= wbo
      case M.parseWBOByteString name bs of
        Left err -> assertFailure $ show err
        Right wbo2 -> wbo2 @?= wbo

      case A.parseWBOByteString bs of
        Left err -> assertFailure err
        Right wbo2 -> wbo2 @?= wbo

testOPB :: String -> Bool
testOPB s = sf == sf2
  where
    Right sf  = parseOPBString "-" s
    Right sf2 = parseOPBString "-" (toOPBString sf)

testWBO :: String -> Bool
testWBO s = sf == sf2
  where
    Right sf  = parseWBOString "-" s
    Right sf2 = parseWBOString "-" (toWBOString sf)

------------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
