-- Do not edit! Automatically created with doctest-extract from src/Game/Labyrinth.hs
{-# LINE 51 "src/Game/Labyrinth.hs" #-}

module Test.Game.Labyrinth where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 52 "src/Game/Labyrinth.hs" #-}
import     qualified Game.Labyrinth as Labyrinth

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Game.Labyrinth:138: "
{-# LINE 138 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 138 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.shiftRowLeft k (Labyrinth.shiftRowRight k b)
  )
 DocTest.printPrefix "Game.Labyrinth:139: "
{-# LINE 139 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 139 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.shiftRowRight k (Labyrinth.shiftRowLeft k b)
  )
 DocTest.printPrefix "Game.Labyrinth:154: "
{-# LINE 154 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 154 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.shiftColumnUp k (Labyrinth.shiftColumnDown k b)
  )
 DocTest.printPrefix "Game.Labyrinth:155: "
{-# LINE 155 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 155 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.shiftColumnDown k (Labyrinth.shiftColumnUp k b)
  )
 DocTest.printPrefix "Game.Labyrinth:203: "
{-# LINE 203 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 203 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.cycleRowLeft k (Labyrinth.cycleRowRight k b)
  )
 DocTest.printPrefix "Game.Labyrinth:204: "
{-# LINE 204 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 204 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.cycleRowRight k (Labyrinth.cycleRowLeft k b)
  )
 DocTest.printPrefix "Game.Labyrinth:213: "
{-# LINE 213 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 213 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.cycleColumnUp k (Labyrinth.cycleColumnDown k b)
  )
 DocTest.printPrefix "Game.Labyrinth:214: "
{-# LINE 214 "src/Game/Labyrinth.hs" #-}
 DocTest.property(
{-# LINE 214 "src/Game/Labyrinth.hs" #-}
      \k b -> b == Labyrinth.cycleColumnDown k (Labyrinth.cycleColumnUp k b)
  )
