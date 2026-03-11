import Test.Hspec

import Data.ByteCountReader
import Data.Text (unpack)

main :: IO ()
main = hspec $ 
     do it "Unparsable strings should return Nothing" $
             sizeInBytes "unparseable" `shouldBe` Nothing
        it "Unknown units should return nothing" $
             sizeInBytes "5 Z" `shouldBe` Nothing
        it "Unknown units for base 2 function should return nothing" $
             sizeInBytesAssumingBase2 "5 Z" `shouldBe` Nothing
        testHappyPathScenarios sizeInBytes
          [
            ("1b", 1)
          , ("2 b", 2)
          , ("1 kb", 1000)
          , ("0.5 kib", 512)
          , ("1 kib", 1024)
          , ("2 kb", 2000)
          , ("3 kb", 3000)
          , ("2 mB", 2000000)
          , (".5 MiB", 524288)
          , ("2 gB", 2000000000)
          , ("4 GiB", 4294967296)
          , ("4      Tb", 4000000000000)
          , ("1      tIb", 1099511627776)
          ]
        testHappyPathScenarios sizeInBytesAssumingBase2
          [ 
            ("0 b", 0)
          , ("5 b", 5)
          , ("1 kb", 1024)
          , ("0.5 kib", 512)
          , ("1 kib", 1024)
          , ("2 kb", 2048)
          , ("3 kb", 3072)
          , ("1 MB", 1048576)
          , (".5 MiB", 524288)
          , ("4 gB", 4294967296)
          , ("4 GiB", 4294967296)
          , ("2 TB", 2199023255552)
          , ("2 TiB", 2199023255552)
          ]
        where testHappyPathScenarios functionUnderTest = foldl (>>) (return ()) . map (parseTest functionUnderTest)
              parseTest functionUnderTest (string, expectedValue) = it ("Parse " <> unpack string) $
                                                                       functionUnderTest string `shouldBe` Just expectedValue



