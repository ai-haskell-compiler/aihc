-- Do not edit! Automatically created with doctest-extract from src/Solve.hs
{-# LINE 12 "src/Solve.hs" #-}

module Test.Solve where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 13 "src/Solve.hs" #-}
import     qualified Solve
import     qualified Data.List as List
import     Data.Eq.HT (equating)
import     Control.Functor.HT (void)
import     Test.Utility
       (solve, genOperands, genResult, genEquation,
        normalizeSubExpr, normalizeSum)
import     qualified Test.QuickCheck as QC

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Solve:138: "
{-# LINE 138 "src/Solve.hs" #-}
 DocTest.example(
{-# LINE 138 "src/Solve.hs" #-}
    solve [25, 50, 75, 100, 3, 6] 952
  )
  [ExpectedLine [LineChunk "25+6*75*(3+100)/50"],ExpectedLine [LineChunk "(3*75*(6+100)-50)/25"]]
 DocTest.printPrefix "Solve:142: "
{-# LINE 142 "src/Solve.hs" #-}
 DocTest.example(
{-# LINE 142 "src/Solve.hs" #-}
    solve [75, 50, 2, 3, 8, 7] 812
  )
  [ExpectedLine [LineChunk "50+(2+75)*(3+7)-8"],ExpectedLine [LineChunk "2*7*(8+50)"],WildCardLine,ExpectedLine [LineChunk "50*(7+75)/(2+3)-8"],ExpectedLine [LineChunk "(3+(2+75)/7)*(8+50)"]]
 DocTest.printPrefix "Solve:149: "
{-# LINE 149 "src/Solve.hs" #-}
 DocTest.example(
{-# LINE 149 "src/Solve.hs" #-}
    solve [100, 75, 50, 10, 5, 1] 102
  )
  [ExpectedLine [LineChunk "1+100+5*10/50"],ExpectedLine [LineChunk "1+100+50/5/10"],WildCardLine,ExpectedLine [LineChunk "(100+50/(75-5*10))/1"],ExpectedLine [LineChunk "(100+(5+75)/(50-10))/1"],ExpectedLine [LineChunk "(100+(75-5-50)/10)/1"]]
 DocTest.printPrefix "Solve:158: "
{-# LINE 158 "src/Solve.hs" #-}
 DocTest.property(
{-# LINE 158 "src/Solve.hs" #-}
        
   QC.forAll genOperands $ \xs ->
   QC.forAll (genResult xs) $ \x ->
      not $ null $ Solve.run (xs,x)
  )
 DocTest.printPrefix "Solve:164: "
{-# LINE 164 "src/Solve.hs" #-}
 DocTest.property(
{-# LINE 164 "src/Solve.hs" #-}
        
   QC.forAll genOperands $ \xs ->
   QC.forAll (genResult xs) $ \x ->
   QC.forAll (QC.shuffle xs) $ \xs1 ->
      void (Solve.run (xs,x)) == void (Solve.run (xs1,x))
  )
 DocTest.printPrefix "Solve:171: "
{-# LINE 171 "src/Solve.hs" #-}
 DocTest.property(
{-# LINE 171 "src/Solve.hs" #-}
        
   QC.forAll genOperands $ \xs ->
   QC.forAll (genResult xs) $ \x ->
   QC.forAll (QC.shuffle xs) $ \xs1 ->
      equating (List.sort . map (normalizeSubExpr normalizeSum))
         (Solve.run (xs,x)) (Solve.run (xs1,x))
  )
 DocTest.printPrefix "Solve:179: "
{-# LINE 179 "src/Solve.hs" #-}
 DocTest.property(
{-# LINE 179 "src/Solve.hs" #-}
        
   QC.forAll genOperands $ \xs ->
   QC.forAll (genEquation xs) $ \(expr,x) ->
      elem expr $
      List.sort $ map (normalizeSubExpr normalizeSum) $ Solve.run (xs,x)
  )
