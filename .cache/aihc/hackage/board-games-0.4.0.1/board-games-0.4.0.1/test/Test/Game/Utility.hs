-- Do not edit! Automatically created with doctest-extract from private/Game/Utility.hs
{-# LINE 17 "private/Game/Utility.hs" #-}

module Test.Game.Utility where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 18 "private/Game/Utility.hs" #-}
import     Game.Utility (Choice, mergeChoice, noChoice)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Game.Utility:58: "
{-# LINE 58 "private/Game/Utility.hs" #-}
 DocTest.property(
{-# LINE 58 "private/Game/Utility.hs" #-}
      \a -> a == mergeChoice noChoice (a :: Choice Char)
  )
 DocTest.printPrefix "Game.Utility:59: "
{-# LINE 59 "private/Game/Utility.hs" #-}
 DocTest.property(
{-# LINE 59 "private/Game/Utility.hs" #-}
      \a -> a == mergeChoice a (noChoice :: Choice Char)
  )
 DocTest.printPrefix "Game.Utility:60: "
{-# LINE 60 "private/Game/Utility.hs" #-}
 DocTest.property(
{-# LINE 60 "private/Game/Utility.hs" #-}
      \a b -> mergeChoice a b == mergeChoice b (a :: Choice Char)
  )
