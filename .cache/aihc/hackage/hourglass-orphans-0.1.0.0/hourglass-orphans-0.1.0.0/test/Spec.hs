{-# LANGUAGE OverloadedStrings #-}

import Data.Hourglass.Types.Orphans
import Data.Aeson.Types
import Test.Hspec
import Data.Hourglass.Types
import Data.Text

main :: IO ()
main =
  hspec $
  do describe "Month instance" $
       do it "January" $
            do let x = January :: Month
               toJSON x `shouldBe` (String "January")
          it "December" $
            do let x = December :: Month
               toJSON x `shouldBe` (String "December")
     describe "Weekday instance" $
       do it "Sunday" $
            do let x = Sunday :: WeekDay
               toJSON x `shouldBe` (String "Sunday")
          it "Friday" $
            do let x = Friday :: WeekDay
               toJSON x `shouldBe` (String "Friday")
     describe "NanoSeconds" $
       do it "90000" $
            do let x = NanoSeconds 90000
               toJSON x `shouldBe` (Number 90000.0)
     describe "Elapsed" $
       do it "sample 1" $
            do let x = Elapsed 500
               toJSON x `shouldBe` (Number 500.0)
     describe "Date" $
       do it "sample 1" $
            do let x =
                     Date
                     { dateYear = 2000
                     , dateMonth = January
                     , dateDay = 3
                     }
               toJSON x `shouldBe`
                 object
                   [ ("dateMonth", String "January")
                   , ("dateDay", Number 3.0)
                   , ("dateYear", Number 2000.0)
                   ]
