-- Do not edit! Automatically created with doctest-extract from src/Data/Bool/HT/Private.hs
module DocTest.Data.Bool.HT.Private where

import Data.Bool.HT.Private
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest


test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Bool.HT.Private:55: "
{-# LINE 55 "src/Data/Bool/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 55 "src/Data/Bool/HT/Private.hs" #-}
    True ?: ("yes", "no")
  )
  [ExpectedLine [LineChunk "\"yes\""]]
 DocTest.printPrefix "Data.Bool.HT.Private:57: "
{-# LINE 57 "src/Data/Bool/HT/Private.hs" #-}
 DocTest.example(
{-# LINE 57 "src/Data/Bool/HT/Private.hs" #-}
    False ?: ("yes", "no")
  )
  [ExpectedLine [LineChunk "\"no\""]]
 DocTest.printPrefix "Data.Bool.HT.Private:73: "
{-# LINE 73 "src/Data/Bool/HT/Private.hs" #-}
 DocTest.property(
{-# LINE 73 "src/Data/Bool/HT/Private.hs" #-}
      \a b -> implies a b == (a<=b)
  )
