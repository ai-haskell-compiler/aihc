-- Do not edit! Automatically created with doctest-extract from src/Game/Mastermind.hs
{-# LINE 56 "src/Game/Mastermind.hs" #-}

module Test.Game.Mastermind where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 57 "src/Game/Mastermind.hs" #-}
import     qualified Test.Mastermind as TestMM
import     Test.Mastermind (CodeSetInt, alphabet, Code(Code), CodePair(CodePair), forAllEval)
import     qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
import     qualified Game.Mastermind.CodeSet as CodeSet
import     qualified Game.Mastermind as MM
import     qualified Data.EnumSet as EnumSet
import     Game.Mastermind (Eval(Eval))
import     Control.Monad (replicateM)
import     Data.Function.HT (compose2)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Game.Mastermind:74: "
{-# LINE 74 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 74 "src/Game/Mastermind.hs" #-}
      \(CodePair secret attempt) -> MM.evaluate secret attempt == MM.evaluate attempt secret
  )
 DocTest.printPrefix "Game.Mastermind:128: "
{-# LINE 128 "src/Game/Mastermind.hs" #-}
 DocTest.example(
{-# LINE 128 "src/Game/Mastermind.hs" #-}
    filter ((MM.Eval 2 0 ==) . MM.evaluate "aabbb") $ replicateM 5 ['a'..'c']
  )
  [ExpectedLine [LineChunk "[\"aaaaa\",\"aaaac\",\"aaaca\",\"aaacc\",\"aacaa\",\"aacac\",\"aacca\",\"aaccc\",\"acbcc\",\"accbc\",\"acccb\",\"cabcc\",\"cacbc\",\"caccb\",\"ccbbc\",\"ccbcb\",\"cccbb\"]"]]
 DocTest.printPrefix "Game.Mastermind:130: "
{-# LINE 130 "src/Game/Mastermind.hs" #-}
 DocTest.example(
{-# LINE 130 "src/Game/Mastermind.hs" #-}
    CodeSet.flatten (MM.matching (EnumSet.fromList ['a'..'c']) "aabbb" (Eval 2 0) :: CodeSetTree.T Char)
  )
  [ExpectedLine [LineChunk "[\"aaaaa\",\"aaaac\",\"aaaca\",\"aaacc\",\"aacaa\",\"aacac\",\"aacca\",\"aaccc\",\"acbcc\",\"accbc\",\"acccb\",\"cabcc\",\"cacbc\",\"caccb\",\"ccbbc\",\"ccbcb\",\"cccbb\"]"]]
 DocTest.printPrefix "Game.Mastermind:133: "
{-# LINE 133 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 133 "src/Game/Mastermind.hs" #-}
      \(CodePair secret attempt) -> CodeSetTree.member secret $ MM.matching alphabet attempt (MM.evaluate secret attempt)
  )
 DocTest.printPrefix "Game.Mastermind:134: "
{-# LINE 134 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 134 "src/Game/Mastermind.hs" #-}
      \(CodePair secret attempt) -> forAllEval secret $ \eval -> (eval == MM.evaluate secret attempt) == CodeSetTree.member secret (MM.matching alphabet attempt eval)
  )
 DocTest.printPrefix "Game.Mastermind:135: "
{-# LINE 135 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 135 "src/Game/Mastermind.hs" #-}
      \(Code attempt) -> forAllEval attempt $ \eval0 -> forAllEval attempt $ \eval1 -> eval0 == eval1 || CodeSetTree.null (compose2 CodeSetTree.intersection (MM.matching alphabet attempt) eval0 eval1)
  )
 DocTest.printPrefix "Game.Mastermind:136: "
{-# LINE 136 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 136 "src/Game/Mastermind.hs" #-}
      \(Code attempt) -> forAllEval attempt $ \eval -> all ((eval ==) . MM.evaluate attempt) $ take 100 $ CodeSet.flatten $ (MM.matching alphabet attempt eval :: CodeSetInt)
  )
 DocTest.printPrefix "Game.Mastermind:137: "
{-# LINE 137 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 137 "src/Game/Mastermind.hs" #-}
      \(Code attempt) -> forAllEval attempt $ \eval -> let set :: CodeSetInt; set = MM.matching alphabet attempt eval in map (CodeSet.select set) [0 .. min 100 (CodeSet.size set) - 1] == take 100 (CodeSet.flatten set)
  )
 DocTest.printPrefix "Game.Mastermind:138: "
{-# LINE 138 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 138 "src/Game/Mastermind.hs" #-}
      TestMM.intersections
  )
 DocTest.printPrefix "Game.Mastermind:139: "
{-# LINE 139 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 139 "src/Game/Mastermind.hs" #-}
      TestMM.solve
  )
 DocTest.printPrefix "Game.Mastermind:180: "
{-# LINE 180 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 180 "src/Game/Mastermind.hs" #-}
      \(Code attempt) -> fromIntegral (EnumSet.size alphabet) ^ length attempt == sum (map snd (MM.partitionSizes alphabet attempt))
  )
 DocTest.printPrefix "Game.Mastermind:250: "
{-# LINE 250 "src/Game/Mastermind.hs" #-}
 DocTest.property(
{-# LINE 250 "src/Game/Mastermind.hs" #-}
      TestMM.bestSeparatingCode
  )
