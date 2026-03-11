-- Do not edit! Automatically created with doctest-extract from src/Data/Spreadsheet.hs
{-# LINE 24 "src/Data/Spreadsheet.hs" #-}

module Test.Data.Spreadsheet where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 25 "src/Data/Spreadsheet.hs" #-}
import     qualified Data.Spreadsheet as Spreadsheet
import     qualified Control.Monad.Exception.Asynchronous.Lazy as MEA
import     qualified Test.QuickCheck as QC

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Spreadsheet:100: "
{-# LINE 100 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 100 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.fromString '"' '\t' "\"hello\"\t\"world\"\n\"end\"\n"
  )
  [ExpectedLine [LineChunk "Exceptional {exception = Nothing, result = [[\"hello\",\"world\"],[\"end\"]]}"]]
 DocTest.printPrefix "Data.Spreadsheet:102: "
{-# LINE 102 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 102 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.fromString '"' ',' "\"hello,world\",\"really\"\n\"end\"\n"
  )
  [ExpectedLine [LineChunk "Exceptional {exception = Nothing, result = [[\"hello,world\",\"really\"],[\"end\"]]}"]]
 DocTest.printPrefix "Data.Spreadsheet:104: "
{-# LINE 104 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 104 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.fromString '"' ';' "\"hello \"\"world\"\"\"\n\"really\"\n"
  )
  [ExpectedLine [LineChunk "Exceptional {exception = Nothing, result = [[\"hello \\\"world\\\"\"],[\"really\"]]}"]]
 DocTest.printPrefix "Data.Spreadsheet:106: "
{-# LINE 106 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 106 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.fromString '"' ',' "\"hello\nworld\"\n"
  )
  [ExpectedLine [LineChunk "Exceptional {exception = Nothing, result = [[\"hello\\nworld\"]]}"]]
 DocTest.printPrefix "Data.Spreadsheet:129: "
{-# LINE 129 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 129 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.toString '"' '\t' [["hello","world"],["end"]]
  )
  [ExpectedLine [LineChunk "\"\\\"hello\\\"\\t\\\"world\\\"\\n\\\"end\\\"\\n\""]]
 DocTest.printPrefix "Data.Spreadsheet:131: "
{-# LINE 131 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 131 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.toString '"' ',' [["hello,world","really"],["end"]]
  )
  [ExpectedLine [LineChunk "\"\\\"hello,world\\\",\\\"really\\\"\\n\\\"end\\\"\\n\""]]
 DocTest.printPrefix "Data.Spreadsheet:133: "
{-# LINE 133 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 133 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.toString '"' ';' [["hello \"world\""],["really"]]
  )
  [ExpectedLine [LineChunk "\"\\\"hello \\\"\\\"world\\\"\\\"\\\"\\n\\\"really\\\"\\n\""]]
 DocTest.printPrefix "Data.Spreadsheet:135: "
{-# LINE 135 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 135 "src/Data/Spreadsheet.hs" #-}
    Spreadsheet.toString '"' ',' [["hello\nworld"]]
  )
  [ExpectedLine [LineChunk "\"\\\"hello\\nworld\\\"\\n\""]]
 DocTest.printPrefix "Data.Spreadsheet:137: "
{-# LINE 137 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 137 "src/Data/Spreadsheet.hs" #-}
    take 50 $ Spreadsheet.toString '"' ',' $ repeat ["hello","world"]
  )
  [ExpectedLine [LineChunk "\"\\\"hello\\\",\\\"world\\\"\\n\\\"hello\\\",\\\"world\\\"\\n\\\"hello\\\",\\\"world\\\"\\n\\\"h\""]]
 DocTest.printPrefix "Data.Spreadsheet:139: "
{-# LINE 139 "src/Data/Spreadsheet.hs" #-}
 DocTest.example(
{-# LINE 139 "src/Data/Spreadsheet.hs" #-}
    take 50 $ Spreadsheet.toString '"' ',' [cycle ["hello","world"]]
  )
  [ExpectedLine [LineChunk "\"\\\"hello\\\",\\\"world\\\",\\\"hello\\\",\\\"world\\\",\\\"hello\\\",\\\"world\\\",\\\"h\""]]
 DocTest.printPrefix "Data.Spreadsheet:142: "
{-# LINE 142 "src/Data/Spreadsheet.hs" #-}
 DocTest.property(
{-# LINE 142 "src/Data/Spreadsheet.hs" #-}
        
   QC.forAll (QC.elements ";,\t ") $ \sep tableNE ->
   let table = map QC.getNonEmpty tableNE in
   table ==
   MEA.result (Spreadsheet.fromString '"' sep
                  (Spreadsheet.toString '"' sep table))
  )
