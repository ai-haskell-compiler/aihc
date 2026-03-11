-- Do not edit! Automatically created with doctest-extract from src/Data/Maybe/HT.hs
{-# LINE 6 "src/Data/Maybe/HT.hs" #-}

module DocTest.Data.Maybe.HT where

import Data.Maybe.HT
import qualified Test.DocTest.Driver as DocTest

{-# LINE 7 "src/Data/Maybe/HT.hs" #-}
import     Control.Monad (guard)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Maybe.HT:15: "
{-# LINE 15 "src/Data/Maybe/HT.hs" #-}
 DocTest.property(
{-# LINE 15 "src/Data/Maybe/HT.hs" #-}
      \b x ->  (guard b >> x)  ==  (toMaybe b =<< (x::Maybe Char))
  )
