-- Do not edit! Automatically created with doctest-extract from src/Algebra/PrincipalIdealDomain.hs
{-# LINE 64 "src/Algebra/PrincipalIdealDomain.hs" #-}

module Test.Algebra.PrincipalIdealDomain where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 65 "src/Algebra/PrincipalIdealDomain.hs" #-}
import     qualified Algebra.PrincipalIdealDomain as PID
import     Test.NumericPrelude.Utility ((/\))
import     qualified Test.QuickCheck as QC

genResidueClass     :: QC.Gen (Integer,Integer)
genResidueClass     = do
       m <- fmap QC.getNonZero $ QC.arbitrary
       a <- QC.choose (min 0 $ 1+m, max 0 $ m-1)
       return (m,a)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Algebra.PrincipalIdealDomain:305: "
{-# LINE 305 "src/Algebra/PrincipalIdealDomain.hs" #-}
 DocTest.property
{-# LINE 305 "src/Algebra/PrincipalIdealDomain.hs" #-}
     (QC.listOf genResidueClass /\ \xs -> case PID.chineseRemainderMulti xs of Nothing -> True; Just (n,b) -> abs n == abs (foldl lcm 1 (map fst xs)) && map snd xs == map (mod b . fst) xs)
 DocTest.printPrefix "Algebra.PrincipalIdealDomain:306: "
{-# LINE 306 "src/Algebra/PrincipalIdealDomain.hs" #-}
 DocTest.property
{-# LINE 306 "src/Algebra/PrincipalIdealDomain.hs" #-}
     (\(QC.NonEmpty ms) b -> let xs = map (\(QC.NonZero m) -> (m, mod b m)) ms in case PID.chineseRemainderMulti xs of Nothing -> False; Just (n,c) -> abs n == abs (foldl lcm 1 (map QC.getNonZero ms)) && mod b n == (c::Integer))
 DocTest.printPrefix "Algebra.PrincipalIdealDomain:298: "
{-# LINE 298 "src/Algebra/PrincipalIdealDomain.hs" #-}
 DocTest.example
{-# LINE 298 "src/Algebra/PrincipalIdealDomain.hs" #-}
   (PID.chineseRemainderMulti [(100,21), (10000,2021::Integer)])
  [ExpectedLine [LineChunk "Just (10000,2021)"]]
 DocTest.printPrefix "Algebra.PrincipalIdealDomain:300: "
{-# LINE 300 "src/Algebra/PrincipalIdealDomain.hs" #-}
 DocTest.example
{-# LINE 300 "src/Algebra/PrincipalIdealDomain.hs" #-}
   (PID.chineseRemainderMulti [(97,90),(99,10),(100,0::Integer)])
  [ExpectedLine [LineChunk "Just (960300,100000)"]]
 DocTest.printPrefix "Algebra.PrincipalIdealDomain:302: "
{-# LINE 302 "src/Algebra/PrincipalIdealDomain.hs" #-}
 DocTest.example
{-# LINE 302 "src/Algebra/PrincipalIdealDomain.hs" #-}
   (PID.chineseRemainderMulti [(95,30),(97,27),(98,8),(99,1::Integer)])
  [ExpectedLine [LineChunk "Just (89403930,1000000)"]]
